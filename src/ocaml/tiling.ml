open Core.Std
open Asm

module FreshReg   = Fresh.Make(struct let name = "_asmreg" end)
module FreshLabel = Fresh.Make(struct let name = "_asmlabel" end)

let rec munch_expr (e: Ir.expr) : abstract_reg * abstract_asm list =
  match e with
  | BinOp (e1, opcode, e2) -> begin
    let (reg1, asm1) = munch_expr e1 in
    let (reg2, asm2) = munch_expr e2 in

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

let register_allocate asms =
  (* env maps each fake name to an index, starting at 1, into the stack. For
   * example, if the fake name "foo" is mapped to n in env, then Reg (Fake
   * "foo") will be spilled to -8n(%rbp). *)
  let env =
    fakes_of_asms asms
    |> List.mapi ~f:(fun i asm -> (asm, i + 1))
    |> String.Map.of_alist_exn
  in

  let spill_address (env: int String.Map.t) (fake: string) : reg operand =
    let i = String.Map.find_exn env fake in
    let offset = Int64.of_int (-8 * i) in
    Mem (Base (Some offset, Rbp))
  in

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
    | Directive (d, args) -> Directive (d, args)
  in

  let translate_reg (r: abstract_reg) : reg =
    match r with
    | Fake r -> Rax
    | Real r -> r
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
  let allocate (env: int String.Map.t) (asm: abstract_asm) : asm list =
    let spill = spill_address env in
    match asm with
    | Op (op, operands) ->
      let fakes = fakes_of_operands operands in
      let pre = List.map fakes ~f:(fun fake -> mov (spill fake) (Reg Rax)) in
      let translation = [abstract_reg_map translate_reg asm] in
      let post = List.map fakes ~f:(fun fake -> mov (Reg Rax) (spill fake)) in
      pre @ translation @ post
    | Directive _ -> []
  in

  List.concat_map ~f:(allocate env) asms
