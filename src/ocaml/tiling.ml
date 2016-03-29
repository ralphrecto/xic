open Core.Std
open Asm

let fresh_reg =
  let counter = ref (-1) in
  fun () ->
    incr counter;
    "_asmreg" ^ (string_of_int !counter)

let fresh_label =
	let counter = ref (-1) in
	fun () ->
		incr counter;
		"_asmlabel" ^ (string_of_int !counter)	

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
      let new_tmp = fresh_reg () in
      (Fake new_tmp, [mov (Asm.Const c) (Reg (Fake new_tmp))])
  | Mem (e, memtype) -> 
      let (e_reg, e_asm) = munch_expr e in
      let new_tmp = fresh_reg () in
      (Fake new_tmp, [mov (Mem (Base (None, e_reg))) (Reg (Fake new_tmp))])
  | Name str ->
      let new_tmp = fresh_reg () in
      (Fake new_tmp, [mov (Label str) (Reg (Fake new_tmp))])
  | Temp str -> (Fake str, [])
  | ESeq _ -> failwith "eseq shouldn't exist"

let rec munch_stmt (s: Ir.stmt) : abstract_asm list =
	match s with 
  | CJumpOne (e1, tru) -> 
		begin
			match e1 with
			| BinOp (e1, EQ, e2) ->
				let (e1_reg, e1_lst) = munch_expr e1 in
				let (e2_reg, e2_lst) = munch_expr e2 in
				let fresh_l = fresh_label () in
				let jump_lst = [
					cmpq (Reg e2_reg) (Reg e1_reg);
					jne (Asm.Label fresh_l); 
					jmp (Asm.Label tru);
					label_op fresh_l;
				] in
				e1_lst @ e2_lst @ jump_lst
			| BinOp (e1, NEQ, e2) ->
				let (e1_reg, e1_lst) = munch_expr e1 in
				let (e2_reg, e2_lst) = munch_expr e2 in
				let fresh_l = fresh_label () in
				let jump_lst = [
					cmpq (Reg e2_reg) (Reg e1_reg);	
					je (Asm.Label fresh_l);
					jmp (Asm.Label tru);
					label_op fresh_l;
				] in
				e1_lst @ e2_lst @ jump_lst	
			| BinOp (e1, LT, e2) -> 
				let (e1_reg, e1_lst) = munch_expr e1 in
				let (e2_reg, e2_lst) = munch_expr e2 in
				let fresh_l = fresh_label () in
				let jump_lst = [
					cmpq (Reg e2_reg) (Reg e1_reg);	
					jge (Asm.Label fresh_l);
					jmp (Asm.Label tru);
					label_op fresh_l;
				] in
				e1_lst @ e2_lst @ jump_lst
			| BinOp (e1, GT, e2) ->
				let (e1_reg, e1_lst) = munch_expr e1 in
				let (e2_reg, e2_lst) = munch_expr e2 in
				let fresh_l = fresh_label () in
				let jump_lst = [
					cmpq (Reg e2_reg) (Reg e1_reg);	
					jle (Asm.Label fresh_l);
					jmp (Asm.Label tru);
					label_op fresh_l;
				] in
				e1_lst @ e2_lst @ jump_lst
			| BinOp (e1, LEQ, e2) ->
				let (e1_reg, e1_lst) = munch_expr e1 in
				let (e2_reg, e2_lst) = munch_expr e2 in
				let fresh_l = fresh_label () in
				let jump_lst = [
					cmpq (Reg e2_reg) (Reg e1_reg);	
					jg (Asm.Label fresh_l);
					jmp (Asm.Label tru);
					label_op fresh_l;
				] in
				e1_lst @ e2_lst @ jump_lst
			| BinOp (e1, GEQ, e2) ->
				let (e1_reg, e1_lst) = munch_expr e1 in
				let (e2_reg, e2_lst) = munch_expr e2 in
				let fresh_l = fresh_label () in
				let jump_lst = [
					cmpq (Reg e2_reg) (Reg e1_reg);	
					jl (Asm.Label fresh_l);
					jmp (Asm.Label tru);
					label_op fresh_l;
				] in
				e1_lst @ e2_lst @ jump_lst
			| _ -> 
				let (binop_reg, binop_lst) = munch_expr e1 in
				let fresh_l = fresh_label () in
				let jump_lst = [
					cmpq (Const 0L) (Reg binop_reg);
					jz (Asm.Label fresh_l);
					jmp (Asm.Label tru);
					label_op fresh_l;
				] in			
				binop_lst @ jump_lst
		end
  | Jump (Name s) -> [jmp (Asm.Label s)]
	| Jump _ -> failwith "jump to a non label shouldn't exist"
  | Exp e -> snd (munch_expr e)
  | Label l -> [label_op l]
  | Move (e1, e2) -> failwith "didn't implement"
  | Seq s_list -> List.map ~f:munch_stmt s_list |> List.concat
  | Return -> [ret]
	| CJump _ -> failwith "cjump shouldn't exist"
