open Core.Std
open Asm
open Typecheck
open Ir_generation
open Func_context

module FreshReg     = Fresh.Make(struct let name = "_asmreg" end)
module FreshLabel   = Fresh.Make(struct let name = "_asmlabel" end)
module FreshRetPtr = Fresh.Make(struct let name = "_asmretptr" end)

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

let rec munch_expr
    (curr_ctx: func_context)
    (fcontexts: func_contexts)
    (e: Ir.expr) =
  match e with
  | BinOp (e1, opcode, e2) ->
  	begin
			let (reg1, asm1) = munch_expr curr_ctx fcontexts e1 in
			let (reg2, asm2) = munch_expr curr_ctx fcontexts e2 in
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
  | Mem (e, _) ->
      let (e_reg, e_asm) = munch_expr curr_ctx fcontexts e in
      let new_tmp = FreshReg.fresh () in
      (Fake new_tmp, e_asm @ [mov (Mem (Base (None, e_reg))) (Reg (Fake new_tmp))])
  | Name str ->
      let new_tmp = FreshReg.fresh () in
      (Fake new_tmp, [mov (Label str) (Reg (Fake new_tmp))])
  | Temp str -> begin
      let new_tmp = Fake (FreshReg.fresh ()) in
      match FreshRetReg.get str with
      (* moving rets from callee return *)
      | Some i ->
          let retmov =
            if i < 2 then
               movq (Reg (Real (ret_reg i))) (Reg new_tmp)
            else
               movq (Mem ((i-2+curr_ctx.max_args)$(Real Rsp))) (Reg new_tmp) in
          (new_tmp, [retmov])
      | None ->
          (* moving passed arguments as callee into vars *)
          match FreshArgReg.get str with
          | Some i ->
              let i' = (max 0 (curr_ctx.num_rets - 2)) + i in
              let argmov = 
                if i' < 2 then
                   movq (Reg (Real (arg_reg i'))) (Reg new_tmp)
                else
                   movq (Mem ((i'-6)$(Real Rsp))) (Reg new_tmp) in
              (new_tmp, [argmov])
          | None -> (Fake str, [])
  end
  | Call (Name (fname), arglist) -> failwith "finish me"
  | Call _ -> failwith "Call should always have a Name first"
  | ESeq _ -> failwith "ESeq shouldn't exist"

and munch_stmt
    (curr_ctx: func_context)
    (fcontexts: func_contexts)
    (s: Ir.stmt) =
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
				let (e1_reg, e1_lst) = munch_expr curr_ctx fcontexts e1 in
				let (e2_reg, e2_lst) = munch_expr curr_ctx fcontexts e2 in
				let jump_lst = [
					cmpq (Reg e2_reg) (Reg e1_reg);
					cond_jump;
				] in
				e1_lst @ e2_lst @ jump_lst
			| _ ->
				let (binop_reg, binop_lst) = munch_expr curr_ctx fcontexts e1 in
				let tru_label = Asm.Label tru in
				let jump_lst = [
					cmpq (Const 0L) (Reg binop_reg);
					jnz tru_label;
				] in
				binop_lst @ jump_lst
		end
  | Jump (Name s) -> [jmp (Asm.Label s)]
  | Exp e -> snd (munch_expr curr_ctx fcontexts e)
  | Label l -> [label_op l]
  | Move (Name n, e) -> begin
      let dest =
        match FreshRetReg.get n with
        | Some i ->
            if i < 2 then Reg (Real (ret_reg i))
            else Reg (Fake (FreshRetPtr.gen (i-2)))
        | None -> Reg (Fake n) in
        let (e_reg, e_lst) = munch_expr curr_ctx fcontexts e in
        e_lst @ [movq (Reg e_reg) dest]
  end
  | Move (Mem (e1, _), e2) ->
		let (e1_reg, e1_lst) = munch_expr curr_ctx fcontexts e1 in
		let (e2_reg, e2_lst) = munch_expr curr_ctx fcontexts e2 in
		e1_lst @ e2_lst @ [movq (Reg e2_reg) (Mem (Base (None, e1_reg)))]
  | Seq s_list -> List.map ~f:(munch_stmt curr_ctx fcontexts) s_list |> List.concat
  | Return -> [leave; ret] 
  | Move _ -> failwith "Move has a non TEMP/MEM lhs"
	| Jump _ -> failwith "jump to a non label shouldn't exist"
	| CJump _ -> failwith "cjump shouldn't exist"

and munch_func_decl
    (fcontexts: func_contexts)
    ((fname, stmt, _): Ir.func_decl) = 

  let curr_ctx = String.Map.find_exn fcontexts fname in
  let body_asm = munch_stmt curr_ctx fcontexts stmt in
  let num_temps = List.length (fakes_of_asms body_asm) in

  let label = [Lab fname] in
  (* TODO: add directives *)
  let directives = [] in

  (* Building function prologue
   *  - use enter to save rbp, update rbp/rsp,
   *      allocate num_temps + num_caller_save words on stack
   *  - Align stack if not aligned to 16 bytes. To maintain this,
   *      we maintain the invariant that all stackframes have
   *      16k bytes for some k\in N. This should work if the
   *      bottom of the stack is a multiple of 16.
   *  - allocate space for tuple returns + argument passing
   *  - move passed mult. ret pointers into fresh temps *)
  let tot_temps = num_temps + num_caller_save in
  let tot_rets_n_args = curr_ctx.max_rets + curr_ctx.max_args in
  (* saved rip + saved rbp + temps + rets n args  *)
  let tot_stack_size = 1 + 1 + tot_temps + tot_rets_n_args in
  let prologue =
    let init = [enter (const tot_temps) (const 0)] in
    let padding =
      if tot_stack_size mod 16 = 0 then []
      else [push (const 0)] in
    let rets_n_args =
      [movq (const (tot_rets_n_args * 8)) (Reg (Real Rsp))] in
    let ret_ptr_mov = 
      let f i =
        let ret_tmp = Fake (FreshRetPtr.gen i) in
        if i < 2 then
          movq (Reg (Real (arg_reg i))) (Reg ret_tmp)
        else
          movq (Mem ((i-2)$(Real Rsp))) (Reg ret_tmp) in
      List.map ~f (List.range 0 (curr_ctx.num_rets - 2)) in
    init @ padding @ rets_n_args @ ret_ptr_mov in

  label @ directives @ prologue @ body_asm
 

and munch_comp_unit
    (fcontexts: func_contexts)
    ((prog_name, func_decls): Ir.comp_unit) =
  List.concat_map ~f:(munch_func_decl fcontexts) (String.Map.data func_decls)

let rec chomp_expr (e: Ir.expr) : abstract_reg * abstract_asm list =
  let max_int32 = Int64.of_int32_exn (Int32.max_value) in
  let min_int32 = Int64.of_int32_exn (Int32.min_value) in
  match e with
  (* neg case *)
  | BinOp (Const 0L, SUB, e1) ->
    let (reg1, asm1) = chomp_expr e1 in
    (reg1, asm1 @ [negq (Reg reg1)])
  (* incr cases *)
  | BinOp (e1, ADD, Const 1L)
  | BinOp (Const 1L, ADD, e1) ->
    let (reg1, asm1) = chomp_expr e1 in
    (reg1, asm1 @ [incq (Reg reg1)])
  (* decr case *)
  | BinOp (e1, SUB, Const 1L) ->
    let (reg1, asm1) = chomp_expr e1 in
    (reg1, asm1 @ [decq (Reg reg1)])
  (* lea cases with constants *)
  (* reg1 = reg1 * {1,2,4,8} + reg2 +/- const *)
  (* c + (r1 * {1,2,4,8} + r2) *)
  | BinOp ((Const x as e3), (ADD as op), BinOp (BinOp (e1, MUL, (Const (1L|2L|4L|8L) as mult_c)), ADD, e2))
  | BinOp ((Const x as e3), (ADD as op), BinOp (BinOp ((Const (1L|2L|4L|8L) as mult_c), MUL, e1), ADD, e2))
  (* c + (r2 + r1 * {1,2,4,8}) *)
  | BinOp ((Const x as e3), (ADD as op), BinOp (e2, ADD, BinOp (e1, MUL, (Const (1L|2L|4L|8L) as mult_c))))
  | BinOp ((Const x as e3), (ADD as op), BinOp (e2, ADD, BinOp ((Const (1L|2L|4L|8L) as mult_c), MUL, e1)))
  (* (r1 * {1,2,4,8} + r2) +/- c *)
  | BinOp (BinOp (BinOp (e1, MUL, (Const (1L|2L|4L|8L) as mult_c)), ADD, e2), (ADD|SUB as op), (Const x as e3))
  | BinOp (BinOp (BinOp ((Const (1L|2L|4L|8L) as mult_c), MUL, e1), ADD, e2), (ADD|SUB as op), (Const x as e3))
  (* (r2 + r1 * {1,2,4,8}) +/- c *)
  | BinOp (BinOp (e2, ADD, BinOp (e1, MUL, (Const (1L|2L|4L|8L) as mult_c))), (ADD|SUB as op), (Const x as e3))
  | BinOp (BinOp (e2, ADD, BinOp ((Const (1L|2L|4L|8L) as mult_c), MUL, e1)), (ADD|SUB as op), (Const x as e3))
  (* r2 + (r1 * {1,2,4,8} +/- c *)
  | BinOp (e2, ADD, BinOp (BinOp (e1, MUL, (Const (1L|2L|4L|8L) as mult_c)), (ADD|SUB as op), (Const x as e3)))
  | BinOp (e2, ADD, BinOp (BinOp ((Const (1L|2L|4L|8L) as mult_c), MUL, e1), (ADD|SUB as op), (Const x as e3)))
  (* r2 + (c + r1 * {1,2,4,8}) *)
  | BinOp (e2, ADD, BinOp ((Const x as e3), (ADD as op), BinOp (e1, MUL, (Const (1L|2L|4L|8L) as mult_c))))
  | BinOp (e2, ADD, BinOp ((Const x as e3), (ADD as op), BinOp ((Const (1L|2L|4L|8L) as mult_c), MUL, e1)))
  (* (r1 * {1,2,4,8} +/- c) + r2 *)
  | BinOp (BinOp (BinOp (e1, MUL, (Const (1L|2L|4L|8L) as mult_c)), (ADD|SUB as op), (Const x as e3)), ADD, e2)
  | BinOp (BinOp (BinOp ((Const (1L|2L|4L|8L) as mult_c), MUL, e1), (ADD|SUB as op), (Const x as e3)), ADD, e2)
  (* (c + r1 * {1,2,4,8}) + r2 *)
  | BinOp (BinOp ((Const x as e3), (ADD as op), BinOp (e1, MUL, (Const (1L|2L|4L|8L) as mult_c))), ADD, e2)
  | BinOp (BinOp ((Const x as e3), (ADD as op), BinOp ((Const (1L|2L|4L|8L) as mult_c), MUL, e1)), ADD, e2) ->
    let (reg1, asm1) = chomp_expr e1 in
    let (reg2, asm2) = chomp_expr e2 in
    let (reg3, asm3) = chomp_expr e3 in
    let displacement =
      if min_int32 <= x && x <= max_int32 then
        match op with
        | ADD -> Some x
        | SUB -> Some (Int64.neg x)   
        | _ -> failwith "cannot happen -- lea reg1 = reg1 * {1,2,4,8} + reg2 +/- const displacement"
      else
        None
    in
    let binop_mem =
      match mult_c with
      | Const 1L -> BaseOff (displacement, reg2, reg1, One)
      | Const 2L -> BaseOff (displacement, reg2, reg1, Two)
      | Const 4L -> BaseOff (displacement, reg2, reg1, Four)
      | Const 8L -> BaseOff (displacement, reg2, reg1, Eight)
      | _ -> failwith "cannot happen -- lea reg1 = reg1 * {1,2,4,8} + reg2 +/- const binop_mem" 
    in
    if min_int32 <= x && x <= max_int32 then
      (reg1, asm1 @ asm2 @ [leaq (Mem binop_mem) (Reg reg1)])
    else
      (reg1, asm1 @ asm2 @ [leaq (Mem binop_mem) (Reg reg1)] @ asm3 @ (non_imm_binop op reg3 reg1))
  (* reg = reg * {1,2,3,4,5,8,9} +/ const *)
  | BinOp (BinOp (e1, MUL, (Const (1L|2L|3L|4L|5L|8L|9L) as mult_const)), (ADD|SUB as op), (Const x as e2))  
  | BinOp (BinOp ((Const (1L|2L|3L|4L|5L|8L|9L) as mult_const), MUL, e1), (ADD|SUB as op), (Const x as e2))  
  | BinOp ((Const x as e2), (ADD as op), BinOp (e1, MUL, (Const (1L|2L|3L|4L|5L|8L|9L) as mult_const))) 
  | BinOp ((Const x as e2), (ADD as op), BinOp ((Const (1L|2L|3L|4L|5L|8L|9L) as mult_const), MUL, e1)) ->
    let (reg1, asm1) = chomp_expr e1 in
    let (reg2, asm2) = chomp_expr e2 in
    let displacement = 
      if min_int32 <= x && x <= max_int32 then
        match op with
        | ADD -> Some x
        | SUB -> Some (Int64.neg x)
        | _ -> failwith "cannot happen -- lea reg = reg * {1,2,3,4,5,8,9} +/- const displacement"
      else 
        None
    in
    let binop_mem =
      match mult_const with
      | Const 1L -> Off (displacement, reg1, One)
      | Const 2L -> Off (displacement, reg1, Two)   
      | Const 3L -> BaseOff (displacement, reg1, reg1, Two)
      | Const 4L -> Off (displacement, reg1, Four) 
      | Const 5L -> BaseOff (displacement, reg1, reg1, Four) 
      | Const 8L -> Off (displacement, reg1, Eight) 
      | Const 9L -> BaseOff (displacement, reg1, reg1, Eight)
      | _ -> failwith "cannot happen -- lea reg = reg * {1,2,3,4,5,8,9} +/- const binop_mem" 
    in
    if min_int32 <= x && x <= max_int32 then
      (reg1, asm1 @ [leaq (Mem binop_mem) (Reg reg1)])
    else
      (reg1, asm1 @ [leaq (Mem binop_mem) (Reg reg1)] @ asm2 @ (non_imm_binop op reg2 reg1))
  (* reg2 = reg1 + reg2 +/- const *)
  | BinOp ((Const x as e3), (ADD as op), BinOp (e1, ADD, e2)) 
  | BinOp (BinOp (e1, ADD, e2), (ADD|SUB as op), (Const x as e3))
  | BinOp (e1, ADD, BinOp ((Const x as e3), (ADD as op), e2))
  | BinOp (BinOp ((Const x as e3), (ADD as op), e2), ADD, e1)
  | BinOp (e1, ADD, BinOp (e2, (ADD|SUB as op), (Const x as e3)))
  | BinOp (BinOp (e2, (ADD|SUB as op), (Const x as e3)), ADD, e1) ->
    let (reg1, asm1) = chomp_expr e1 in
    let (reg2, asm2) = chomp_expr e2 in
    let (reg3, asm3) = chomp_expr e3 in   
    if min_int32 <= x && x <= max_int32 then
      let binop_mem =
        match op with
        | ADD -> BaseOff (Some x, reg1, reg2, One)
        | SUB -> BaseOff (Some (Int64.neg x), reg1, reg2, One)
        | _ -> failwith "cannot happen -- lea reg2 = reg1 + reg2 +/- const"
      in
      (reg2, asm1 @ asm2 @ [leaq (Mem binop_mem) (Reg reg2)]) 
    else 
     (reg3, asm1 @ asm2 @ (non_imm_binop ADD reg1 reg2) @ (non_imm_binop op reg2 reg3))
  (* reg = reg +/- const *)
  | BinOp (e1, (ADD|SUB as op), (Const x as e2))
  | BinOp ((Const x as e2), (ADD as op), e1) ->
    let (reg1, asm1) = chomp_expr e1 in
    let (reg2, asm2) = chomp_expr e2 in
    if min_int32 <= x && x <= max_int32 then
      let binop_mem = 
        match op with
        | ADD -> Base (Some x, reg1) 
        | SUB -> Base (Some (Int64.neg x), reg1)
        | _ -> failwith "cannot happen -- lea reg = reg +/- const"
      in
      (reg1, asm1 @ [leaq (Mem binop_mem) (Reg reg1)])  
    else  
      (reg1, asm1 @ asm2 @ (non_imm_binop op reg2 reg1)) 
  (* lea cases without constants *)
  | BinOp (BinOp ((Const (1L|2L|4L|8L) as mult_c), MUL, e1), ADD, e2)
  | BinOp (BinOp (e1, MUL, (Const (1L|2L|4L|8L) as mult_c)), ADD, e2)
  | BinOp (e2, ADD, BinOp ((Const (1L|2L|4L|8L) as mult_c), MUL, e1))
  | BinOp (e2, ADD, BinOp (e1, MUL, (Const (1L|2L|4L|8L) as mult_c))) ->
    let (reg1, asm1) = chomp_expr e1 in
    let (reg2, asm2) = chomp_expr e2 in
    let binop_mem =
      match mult_c with
      | Const 1L -> BaseOff (None, reg2, reg1, One)
      | Const 2L -> BaseOff (None, reg2, reg1, Two)
      | Const 4L -> BaseOff (None, reg2, reg1, Four)
      | Const 8L -> BaseOff (None, reg2, reg1, Eight)
      | _ -> failwith "cannot happen -- lea reg1 = reg1 * {1,2,4,8} + reg2 +/- const binop_mem" 
    in 
    (reg1, asm1 @ asm2 @ [leaq (Mem binop_mem) (Reg reg1)])
  | BinOp (e1, MUL, (Const (1L|2L|3L|4L|5L|8L|9L) as mult_c)) 
  | BinOp ((Const (1L|2L|3L|4L|5L|8L|9L) as mult_c), MUL, e1) ->
    let (reg1, asm1) = chomp_expr e1 in
    let binop_mem =
      match mult_c with
      | Const 1L -> Off (None, reg1, One)
      | Const 2L -> Off (None, reg1, Two)   
      | Const 3L -> BaseOff (None, reg1, reg1, Two)
      | Const 4L -> Off (None, reg1, Four) 
      | Const 5L -> BaseOff (None, reg1, reg1, Four) 
      | Const 8L -> Off (None, reg1, Eight) 
      | Const 9L -> BaseOff (None, reg1, reg1, Eight)
      | _ -> failwith "cannot happen -- lea reg = reg * {1,2,3,4,5,8,9} +/- const binop_mem" 
    in
    (reg1, asm1 @ [leaq (Mem binop_mem) (Reg reg1)])
  | BinOp (e1, ADD, e2) ->
    let (reg1, asm1) = chomp_expr e1 in
    let (reg2, asm2) = chomp_expr e2 in
    let binop_mem = BaseOff (None, reg1, reg2, One) in
    (reg1, asm1 @ asm2 @ [leaq (Mem binop_mem) (Reg reg1)]) 
  (* binop with immediate cases *)
  (* add and sub are not included b/c it is taken care of in lea cases *)
  | BinOp (e1, ((AND|OR|XOR) as op), (Const x as e2))
  | BinOp ((Const x as e2), ((AND|OR|XOR) as op), e1) ->
    let (reg1, asm1) = chomp_expr e1 in
    let (reg2, asm2) = chomp_expr e2 in
    (* checking if immidiate is within 32 bits *)
    if min_int32 <= x && x <= max_int32 then
      (reg1, asm1 @ (imm_binop op x reg1))
    else
      (reg2, asm1 @ asm2 @ (non_imm_binop op reg1 reg2))
  | BinOp (e1, ((LSHIFT|RSHIFT|ARSHIFT) as op), (Const x as e2))
  | BinOp ((Const x as e2), ((LSHIFT|RSHIFT|ARSHIFT) as op), e1) ->
    let (reg1, asm1) = chomp_expr e1 in
    let (reg2, asm2) = chomp_expr e2 in
    (* checking if shift is within 8 bits *)
    if Int64.neg(128L) <= x && x <= 128L then
      (reg1, asm1 @ (imm_shift op x reg1))
    else
      (reg1, asm1 @ asm2 @ (non_imm_shift op reg1 reg2))
  | BinOp (e1, ((EQ|NEQ|LT|GT|LEQ|GEQ) as op), (Const x as e2))
  | BinOp ((Const x as e2), ((EQ|NEQ|LT|GT|LEQ|GEQ) as op), e1) ->
    let (reg1, asm1) = chomp_expr e1 in
    let (reg2, asm2) = chomp_expr e2 in
    if min_int32 <= x && x <= max_int32 then
      (reg1, asm1 @ (imm_cmp op x reg1))
    else
      (reg2, asm1 @ asm2 @ (non_imm_cmp op reg1 reg2))
  (* binop with non-immediate cases *)
  | BinOp (e1, opcode, e2) ->
    begin
      let (reg1, asm1) = chomp_expr e1 in
      let (reg2, asm2) = chomp_expr e2 in
      match opcode with
      | SUB | AND | OR | XOR ->
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
  | Mem (e, _) ->
      let (e_reg, e_asm) = chomp_expr e in
      let new_tmp = FreshReg.fresh () in
      (Fake new_tmp, e_asm @ [mov (Mem (Base (None, e_reg))) (Reg (Fake new_tmp))])
  | Name str ->
      let new_tmp = FreshReg.fresh () in
      (Fake new_tmp, [mov (Label str) (Reg (Fake new_tmp))])
  | Temp str -> (Fake str, [])
  | ESeq _ -> failwith "eseq shouldn't exist"

let register_allocate asms =
  (* spill_env maps each fake name to an index, starting at 1, into the stack.
   * For example, if the fake name "foo" is mapped to n in spill_env, then Reg
   * (Fake "foo") will be spilled to -8n(%rbp). *)
  let spill_env =
    fakes_of_asms asms
    |> List.mapi ~f:(fun i asm -> (asm, i + 1))
    |> String.Map.of_alist_exn
  in

  (* Given an environment and name, return the memory address that fake is
   * spilled into. For example, `spill_address {"foo": 4}` "foo" = -32(%rbp)` *)
  let spill_address (spill_env: int String.Map.t) (fake: string) : reg operand =
    let i = String.Map.find_exn spill_env fake in
    let offset = Int64.of_int (-8 * i) in
    Mem (Base (Some offset, Rbp))
  in

  (* Recursively applies f to all the abstract_registers in asm. *)
  let abstract_reg_map (f: abstract_reg -> reg)  (asm: abstract_asm) =
    match asm with
    | Op (s, operands) ->
        Op (s, List.map operands ~f:(fun operand ->
          match operand with
          | Reg r -> Reg (f r)
          | Mem (Base (n, base)) -> Mem (Base (n, f base))
          | Mem (Off (n, off, scale)) -> Mem (Off (n, f off, scale))
          | Mem (BaseOff (n, base, off, scale)) -> Mem (BaseOff (n, f base, f off, scale))
          | Label l -> Label l
          | Const c -> Const c
        ))
    | Lab l -> Lab l
    | Directive (d, args) -> Directive (d, args)
  in

  (* Certain real registers are output into abstract assembly. For example,
   * multiplication and division use rax and rdx. These registers shouldn't be
   * present in abstract assembly. *)
  let unused_regs = [R13; R14; R15] in

  (* Translate fake registers using the register environment and leave real
   * registers alone. *)
  let translate_reg (reg_env: reg String.Map.t) (r: abstract_reg) : reg =
    match r with
    | Fake s -> String.Map.find_exn reg_env s
    | Real r -> r
  in

  let allocate (env: int String.Map.t) (asm: abstract_asm) : asm list =
    let spill = spill_address env in
    match asm with
    | Op (_, operands) ->
      let fakes = fakes_of_operands operands in
      let unused_regs = List.take unused_regs (List.length fakes) in
      let fake_to_real = List.zip_exn fakes unused_regs in
      let reg_env = String.Map.of_alist_exn fake_to_real in
      let fake_to_op f = Reg (String.Map.find_exn reg_env f) in

      let pre = List.map fakes ~f:(fun fake -> mov (spill fake) (fake_to_op fake)) in
      let translation = [abstract_reg_map (translate_reg reg_env) asm] in
      let post = List.map fakes ~f:(fun fake -> mov (fake_to_op fake) (spill fake)) in
      pre @ translation @ post
    | Lab l -> [Lab l]
    | Directive (d, args) -> [Directive (d, args)]
  in

  List.concat_map ~f:(allocate spill_env) asms
