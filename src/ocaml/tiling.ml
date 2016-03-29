open Core.Std
open Asm
open Typecheck

module FreshReg   = Fresh.Make(struct let name = "_asmreg" end)
module FreshLabel = Fresh.Make(struct let name = "_asmlabel" end)

type func_context = {
  num_args : int;
  num_rets : int;
}

type func_contexts = func_context String.Map.t

let translate_argreg fcontexts fname regname =
  failwith "implement me"

let rec munch_expr (fcontexts: func_contexts) (e: Ir.expr) =
  match e with
  | BinOp (e1, opcode, e2) -> begin
    let (reg1, asm1) = munch_expr fcontexts e1 in
    let (reg2, asm2) = munch_expr fcontexts e2 in

    let cmp_action set_func =
      let cmp_asm = [
        cmpq (Reg reg1) (Reg reg2);
        set_func (Reg reg2);
      ] in
      (reg2, asm1 @ asm2 @ cmp_asm) in

    (* Java style shifting: mod RHS operand by word size *)
    let shift_action shift_func =
      let shift_asm = [
        movq (Reg reg2) (Reg (Real Rcx));
        shift_func (Reg (Real Cl)) (Reg reg1);
      ] in
      (reg1, asm1 @ asm2 @ shift_asm) in

      match opcode with
      | ADD -> (reg2, asm1 @ asm2 @ [addq (Reg reg1) (Reg reg2)])
      | SUB -> (reg2, asm1 @ asm2 @ [subq (Reg reg1) (Reg reg2)])
      | MUL | HMUL -> begin
        let mul_asm = [
          movq (Reg reg2) (Reg (Real Rax));
          imulq (Reg reg1);
        ] in
        let r = if opcode = MUL then Rax else Rdx in
        (Real r, asm1 @ asm2 @ mul_asm)
      end
      | DIV | MOD -> begin
        let div_asm = [
          movq (Reg reg1) (Reg (Real Rax));
          idivq (Reg reg2);
        ] in
        let r = if opcode = DIV then Rax else Rdx in
        (Real r, asm1 @ asm2 @ div_asm)
      end
      | AND-> (reg2, asm1 @ asm2 @ [andq (Reg reg1) (Reg reg2)])
      | OR-> (reg2, asm1 @ asm2 @ [orq (Reg reg1) (Reg reg2)])
      | XOR-> (reg2, asm1 @ asm2 @ [xorq (Reg reg1) (Reg reg2)])
      | LSHIFT -> shift_action salq
      | RSHIFT -> shift_action shrq
      | ARSHIFT -> shift_action sarq
      | EQ -> cmp_action sete
      | NEQ-> cmp_action setne
      | LT -> cmp_action setl
      | GT -> cmp_action setg
      | LEQ -> cmp_action setle
      | GEQ -> cmp_action setge
  end
  | Const c ->
      let new_tmp = FreshReg.fresh () in
      (Fake new_tmp, [mov (Asm.Const c) (Reg (Fake new_tmp))])
  | Mem (e, memtype) ->
      let (e_reg, e_asm) = munch_expr fcontexts e in
      let new_tmp = FreshReg.fresh () in
      (Fake new_tmp, [mov (Mem (Base (None, e_reg))) (Reg (Fake new_tmp))])
  | Name str ->
      let new_tmp = FreshReg.fresh () in
      (Fake new_tmp, [mov (Label str) (Reg (Fake new_tmp))])
  | Temp str -> (Fake str, [])
  | Call (Name (fname), arglist) -> 
      
  | Call _ -> failwith "Call should always have a Name first"
  | ESeq _ -> failwith "ESeq shouldn't exist"

and munch_stmt (fcontexts: func_contexts) (s: Ir.stmt) =
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
				in
				let (e1_reg, e1_lst) = munch_expr fcontexts e1 in
				let (e2_reg, e2_lst) = munch_expr fcontexts e2 in
				let jump_lst = [
					cmpq (Reg e2_reg) (Reg e1_reg);
					cond_jump;
				] in
				e1_lst @ e2_lst @ jump_lst
			| _ ->
				let (binop_reg, binop_lst) = munch_expr fcontexts e1 in
				let tru_label = Asm.Label tru in
				let jump_lst = [
					cmpq (Const 0L) (Reg binop_reg);
					jnz tru_label;
				] in
				binop_lst @ jump_lst
		end
  | Jump (Name s) -> [jmp (Asm.Label s)]
  | Exp e -> snd (munch_expr fcontexts e)
  | Label l -> [label_op l]
  | Move (e1, e2) ->
		let (e1_reg, e1_lst) = munch_expr fcontexts e1 in
		let (e2_reg, e2_lst) = munch_expr fcontexts e2 in
		e1_lst @ e2_lst @ [movq (Reg e2_reg) (Reg e1_reg)]
  | Seq s_list -> List.map ~f:(munch_stmt fcontexts) s_list |> List.concat
  | Return -> [ret]
	| Jump _ -> failwith "jump to a non label shouldn't exist"
	| CJump _ -> failwith "cjump shouldn't exist"

and munch_func_decl (fcontexts: func_contexts) ((fname, stmt, _): Ir.func_decl) = 
  munch_stmt fcontexts stmt

and munch_comp_unit ((prog_name, func_decls): Ir.comp_unit) =
  let get_context (_, _, (argtyps, rettyps)) = 
    let type_num (e: Expr.t) : int =
      match e with
      | TupleT tlist -> List.length tlist
      | _ -> 1 in
    { num_args = type_num argtyps;
      num_rets = type_num rettyps; } in
  let fcontexts = String.Map.map ~f:get_context func_decls in
  List.concat_map ~f:(munch_func_decl fcontexts) (String.Map.data func_decls)


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
