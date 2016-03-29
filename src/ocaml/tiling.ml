open Core.Std
open Asm

module FreshReg   = Fresh.Make(struct let name = "_asmreg" end)
module FreshLabel = Fresh.Make(struct let name = "_asmlabel" end)

let binop_to_instr (op: Ir.binop_code) =
	match op with
	| ADD -> addq
	| SUB -> subq
	| AND -> andq 
	| OR -> orq
	| XOR -> xorq
	| _ -> failwith "shouldn't happen -- binop_to_instr"
	
let cmp_to_instr (op: Ir.binop_code) =
	match op with
	| EQ -> sete
	| NEQ -> setne
	| LT -> setl 
	| GT -> setg
	| LEQ -> setle
	| GEQ -> setge
	| _ -> failwith "shouldn't happen -- cmp_to_instr"

let shift_to_instr (op: Ir.binop_code) =
	match op with
	| LSHIFT -> shlq
	| RSHIFT -> shrq
	| ARSHIFT -> sarq 
	| _ -> failwith "shouldn't happen -- shift_to_instr"

let non_imm_cmp op reg1 reg2 = 
	[cmpq (Reg reg1) (Reg reg2); (cmp_to_instr op) (Reg reg2)]

let imm_cmp op const reg =
	[cmpq (Asm.Const const) (Reg reg); (cmp_to_instr op) (Reg reg)]

(* Java style shifting: mod RHS operand by word size *)
let non_imm_shift op reg1 reg2 =
	[movq (Reg reg2) (Reg (Real Rcx)); (shift_to_instr op) (Reg (Real Cl)) (Reg reg1)]

let imm_shift op const reg =
	[(shift_to_instr op) (Asm.Const const) (Reg reg)]

let imm_binop op const reg =
	[(binop_to_instr op) (Asm.Const const) (Reg reg)]
	
let non_imm_binop op reg1 reg2 =
	[(binop_to_instr op) (Reg reg1) (Reg reg2)]
	
let rec munch_expr (e: Ir.expr) : abstract_reg * abstract_asm list =
  match e with
  | BinOp (e1, opcode, e2) -> 
		begin
			let (reg1, asm1) = munch_expr e1 in
			let (reg2, asm2) = munch_expr e2 in
			match opcode with
			| ADD | SUB | AND | OR | XOR ->
				(reg2, asm1 @ asm2 @ (non_imm_binop opcode reg1 reg2))
			| LSHIFT | RSHIFT | ARSHIFT ->
			 	(reg1, asm1 @ asm2 @ (non_imm_shift opcode reg1 reg2))
			| EQ | NEQ | LT | GT | LEQ | GEQ ->
			  (reg2, asm1 @ asm2 @ (non_imm_cmp opcode reg1 reg2))	
			| MUL | HMUL ->
				let mul_asm = [
					movq (Reg reg2) (Reg (Real Rax));
					imulq (Reg reg1);
				] in
				let r = if opcode = MUL then Rax else Rdx in
				(Real r, asm1 @ asm2 @ mul_asm)
			| DIV | MOD -> 
				let div_asm = [
					movq (Reg reg1) (Reg (Real Rax));
					idivq (Reg reg2);
				] in
				let r = if opcode = DIV then Rax else Rdx in
				(Real r, asm1 @ asm2 @ div_asm)	  	
		end
  | Call (func, arglist) -> failwith "implement me"
  | Const c ->
      let new_tmp = FreshReg.fresh () in
      (Fake new_tmp, [mov (Asm.Const c) (Reg (Fake new_tmp))])
  | Mem (e, memtype) ->
      let (e_reg, e_asm) = munch_expr e in
      let new_tmp = FreshReg.fresh () in
      (Fake new_tmp, [mov (Mem (Base (None, e_reg))) (Reg (Fake new_tmp))])
  | Name str ->
      let new_tmp = FreshReg.fresh () in
      (Fake new_tmp, [mov (Label str) (Reg (Fake new_tmp))])
  | Temp str -> (Fake str, [])
  | ESeq _ -> failwith "eseq shouldn't exist"

let rec munch_stmt (s: Ir.stmt) : abstract_asm list =
	match s with
  | CJumpOne (e1, tru) ->
		begin
			match e1 with
			| BinOp (e1, ((EQ|NEQ|LT|GT|LEQ|GEQ) as op), e2) ->
				let tru_label = Asm.Label tru in
				let cond_jump =
					match op with
					| EQ -> je tru_label
					| NEQ -> jne tru_label
					| LT -> jl tru_label 
					| GT -> jg tru_label 
					| LEQ -> jle tru_label 
					| GEQ -> jge tru_label 
					| _ -> failwith "can't happen"
				in
				let (e1_reg, e1_lst) = munch_expr e1 in
				let (e2_reg, e2_lst) = munch_expr e2 in
				let jump_lst = [
					cmpq (Reg e2_reg) (Reg e1_reg);
					cond_jump;
				] in
				e1_lst @ e2_lst @ jump_lst
			| _ ->
				let (binop_reg, binop_lst) = munch_expr e1 in
				let tru_label = Asm.Label tru in
				let jump_lst = [
					cmpq (Const 0L) (Reg binop_reg);
					jnz tru_label;
				] in
				binop_lst @ jump_lst
		end
  | Jump (Name s) -> [jmp (Asm.Label s)]
  | Exp e -> snd (munch_expr e)
  | Label l -> [label_op l]
  | Move (e1, e2) ->
		let (e1_reg, e1_lst) = munch_expr e1 in
		let (e2_reg, e2_lst) = munch_expr e2 in
		e1_lst @ e2_lst @ [movq (Reg e2_reg) (Reg e1_reg)]
  | Seq s_list -> List.map ~f:munch_stmt s_list |> List.concat
  | Return -> [leave; ret]
	| Jump _ -> failwith "jump to a non label shouldn't exist"
	| CJump _ -> failwith "cjump shouldn't exist"

let rec chomp_expr (e: Ir.expr) : abstract_reg * abstract_asm list =
  match e with
	(* incr cases *)
  | BinOp (e1, ADD, Const 1L)
  | BinOp (Const 1L, ADD, e1) -> 
		let (reg1, asm1) = chomp_expr e1 in
		(reg1, asm1 @ [incq (Reg reg1)])		
	(* decr case *)
	| BinOp (e1, SUB, Const 1L) ->
		let (reg1, asm1) = chomp_expr e1 in
		(reg1, asm1 @ [decq (Reg reg1)])
	(* binop with immediate cases *)
	|	BinOp (e1, ((ADD|SUB|AND|OR|XOR) as op), (Const x as e2))
	| BinOp ((Const x as e2), ((ADD|SUB|AND|OR|XOR) as op), e1) ->
		let (reg1, asm1) = chomp_expr e1 in
		let (reg2, asm2) = chomp_expr e2 in
		let max_int32 = Int64.of_int32_exn (Int32.max_value) in
		let min_int32 = Int64.of_int32_exn (Int32.min_value) in
		(* checking if immidiate is within 32 bits *)
		if min_int32 <= x || x <= max_int32 then
			(reg1, asm1 @ (imm_binop op x reg1))	
		else
			(reg2, asm1 @ asm2 @ (non_imm_binop op reg1 reg2))
	| BinOp (e1, ((LSHIFT|RSHIFT|ARSHIFT) as op), (Const x as e2))
	| BinOp ((Const x as e2), ((LSHIFT|RSHIFT|ARSHIFT) as op), e1) ->
		let (reg1, asm1) = chomp_expr e1 in
		let (reg2, asm2) = chomp_expr e2 in
		(* checking if shift is within 8 bits *)
		if Int64.neg(128L) <= x || x <= 128L then
			(reg1, asm1 @ (imm_shift op x reg1)) 
		else 
			(reg1, asm1 @ asm2 @ (non_imm_shift op reg1 reg2))
	| BinOp (e1, ((EQ|NEQ|LT|GT|LEQ|GEQ) as op), (Const x as e2))
	| BinOp ((Const x as e2), ((EQ|NEQ|LT|GT|LEQ|GEQ) as op), e1) ->
		let (reg1, asm1) = chomp_expr e1 in
		let (reg2, asm2) = chomp_expr e2 in
		let max_int32 = Int64.of_int32_exn (Int32.max_value) in
		let min_int32 = Int64.of_int32_exn (Int32.min_value) in
		if min_int32 <= x || x <= max_int32 then
			(reg1, asm1 @ (imm_cmp op x reg1))
		else
			(reg2, asm1 @ asm2 @ (non_imm_cmp op reg1 reg2))
	(* binop with non-immediate cases *)
  | BinOp (e1, opcode, e2) -> 
		begin
			let (reg1, asm1) = chomp_expr e1 in
			let (reg2, asm2) = chomp_expr e2 in
			match opcode with
			| ADD | SUB | AND | OR | XOR ->
				(reg2, asm1 @ asm2 @ (non_imm_binop opcode reg1 reg2))
			| LSHIFT | RSHIFT | ARSHIFT ->
			 	(reg1, asm1 @ asm2 @ (non_imm_shift opcode reg1 reg2))
			| EQ | NEQ | LT | GT | LEQ | GEQ ->
			  (reg2, asm1 @ asm2 @ (non_imm_cmp opcode reg1 reg2))	
			| MUL | HMUL ->
				let mul_asm = [
					movq (Reg reg2) (Reg (Real Rax));
					imulq (Reg reg1);
				] in
				let r = if opcode = MUL then Rax else Rdx in
				(Real r, asm1 @ asm2 @ mul_asm)
			| DIV | MOD -> 
				let div_asm = [
					movq (Reg reg1) (Reg (Real Rax));
					idivq (Reg reg2);
				] in
				let r = if opcode = DIV then Rax else Rdx in
				(Real r, asm1 @ asm2 @ div_asm)
  	end
  | Call (func, arglist) -> failwith "implement me"
  | Const c ->
      let new_tmp = FreshReg.fresh () in
      (Fake new_tmp, [mov (Asm.Const c) (Reg (Fake new_tmp))])
  | Mem (e, memtype) ->
      let (e_reg, e_asm) = chomp_expr e in
      let new_tmp = FreshReg.fresh () in
      (Fake new_tmp, [mov (Mem (Base (None, e_reg))) (Reg (Fake new_tmp))])
  | Name str ->
      let new_tmp = FreshReg.fresh () in
      (Fake new_tmp, [mov (Label str) (Reg (Fake new_tmp))])
  | Temp str -> (Fake str, [])
  | ESeq _ -> failwith "eseq shouldn't exist"

let register_allocate asms =
  (*
  (* [fakes_of_operand ] returns the names of all the fake registers in asm. *)

  (* [fakes asm] returns the names of all the fake registers in asm. *)
  let fakes_of_asm asm =
    match asm with
    | BinOp (_, Reg (Fake s1), Reg (Fake s2)) -> [s1; s2]
    | BinOp (_, Reg (Fake s1), _)
    | BinOp (_, _, Reg (Fake s1)) -> [s1]
    | BinOp _ -> []
    | UnOp (_, Reg (Fake s)) -> [s]
    | UnOp _ -> []
    | ZeroOp _ -> []
  in

  (* [reals asm] returns all of the real registers in asm. *)
  let fakes asm =
    match asm with
    | BinOp (_, Reg (Fake s1), Reg (Fake s2)) -> [s1; s2]
    | BinOp (_, Reg (Fake s1), _)
    | BinOp (_, _, Reg (Fake s1)) -> [s1]
    | BinOp _ -> []
    | UnOp (_, Reg (Fake s)) -> [s]
    | UnOp _ -> []
    | ZeroOp _ -> []
  in

  (* env maps each fake name to an index, starting at 1, into the stack. For
   * example, if the fake name "foo" is mapped to n in env, then Reg (Fake
   * "foo") will be spilled to -8n(%rbp). *)
  let env =
    List.concat_map ~f:fakes asms
    |> List.dedup
    |> List.mapi ~f:(fun i asm -> (asm, i + 1))
    |> String.Map.of_alist_exn
  in

  let translate name env =
    let i = String.Map.find_exn env name in
    let offset = Int64.of_int (-8 * i) in
    Mem (Base (Some offset, Rax))
  in

  (* op "foo", "bar", "baz"
   *
   * mov -8(%rbp), %rax  \
   * mov -16(%rbp), %rbx  } pre
   * mov -24(%rbp), %rcx /
   *
   * op %rax, %rbx, %rcx  } translation
   *
   * mov %rax, -8(%rbp)  \
   * mov %rbx, -16(%rbp)  } post
   * mov %rcx, -24(%rbp) /
   *)
  let allocate asm env =
    let (op, args) =
      match asm with
      | BinOp (s, a, b) -> (s, [a; b])
      | UnOp (s, a) -> (s, [a])
      | ZeroOp s  -> (s, [])
    in
    let pre = List.map args ~f:(fun fake -> failwith "a") in
    let translation = failwith "" in
    let post = failwith "A"  in
    pre @ translation @ post
  in

  (* help asms env [] *)
  failwith "a"
  *)
  failwith "TODO"
