open Core.Std
open Asm

let fresh_reg =
  let counter = ref -1 in
  fun () ->
    incr counter;
    "_asmreg" ^ (string_of_int !counter)

let munch_expr (e: Ir.expr) : abstract_reg * abstract_asm list =
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
        (Reg (Real r), asm1 @ asm2 @ mul_asm)
      end
      | DIV | MOD -> begin
        let div_asm = [
          movq (Reg reg1) (Reg (Real Rax));
          idivq (Reg reg2);
        ] in
        let r = if opcode = DIV then Rax else Rdx in
        (Reg (Real r), asm1 @ asm2 @ div_asm)
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
  | Const c -> failwith "implement me"
  | Mem (e, memtype) -> failwith "implement me"
  | Name str -> failwith "implement me"
  | Temp str -> failwith "implement me"
  | ESeq _ -> failwith "eseq shouldn't exist"
