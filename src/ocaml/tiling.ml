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
      match opcode with
      | ADD -> (reg2, asm1 @ asm2 @ [addq (Reg reg1) (Reg reg2)])
      | SUB -> (reg2, asm1 @ asm2 @ [subq (Reg reg1) (Reg reg2)])
      | MUL -> begin
        let mul_asm = [
          movq (Reg reg2) (Reg (Real Rax));
          imulq (Reg reg1);
        ] in
        (Reg (Real Rax), asm1 @ asm2 @ mul_asm)
      end
      | HMUL-> begin
        let hmul_asm = [
          movq (Reg reg2) (Reg (Real Rax));
          imulq (Reg reg1);
        ] in
        (Reg (Real Rdx), asm1 @ asm2 @ hmul_asm)
      end
      | DIV-> begin
        let div_asm = [
          movq (Reg reg1) (Reg (Real Rax));
          idivq (Reg reg2);
        ] in
        (Reg (Real Rax), asm1 @ asm2 @ div_asm)
      end
      | MOD-> begin
        let mod_asm = [
          movq (Reg reg1) (Reg (Real Rax));
          idivq (Reg reg2);
        ] in
        (Reg (Real Rdx), asm1 @ asm2 @ mod_asm)
      end
      | AND-> (reg2, asm1 @ asm2 @ [andq (Reg reg1) (Reg reg2)])
      | OR-> (reg2, asm1 @ asm2 @ [orq (Reg reg1) (Reg reg2)])
      | XOR-> (reg2, asm1 @ asm2 @ [xorq (Reg reg1) (Reg reg2)])
      (* Java style shifting: mod RHS operand by word size *)
      | LSHIFT-> begin
        let lshift_asm = [
          movq (Reg reg2) (Reg (Real Rcx));
          salq (Reg (Real Cl)) (Reg reg1);
        ] in
        (reg1, asm1 @ asm2 @ lshift_asm)
      end
      | RSHIFT-> begin
        let rshift_asm = [
          movq (Reg reg2) (Reg (Real Rcx));
          shrq (Reg (Real Cl)) (Reg reg1);
        ] in
        (reg1, asm1 @ asm2 @ rshift_asm)
      end
      | ARSHIFT-> begin
        let arshift_asm = [
          movq (Reg reg2) (Reg (Real Rcx));
          sarq (Reg (Real Cl)) (Reg reg1);
        ] in
        (reg1, asm1 @ asm2 @ rshift_asm)
      end
      | EQ-> (reg2, asm1 @ asm2 @ [addq (Reg reg1) (Reg reg2)])
      | NEQ-> (reg2, asm1 @ asm2 @ [addq (Reg reg1) (Reg reg2)])
      | LT-> (reg2, asm1 @ asm2 @ [addq (Reg reg1) (Reg reg2)])
      | GT-> (reg2, asm1 @ asm2 @ [addq (Reg reg1) (Reg reg2)])
      | LEQ-> (reg2, asm1 @ asm2 @ [addq (Reg reg1) (Reg reg2)])
      | GEQ-> (reg2, asm1 @ asm2 @ [addq (Reg reg1) (Reg reg2)])
  end
  | Call of expr * expr list
  | Const of Int64.t
  | Mem of expr * mem_type
  | Name of string
  | Temp of string
  | ESeq _ -> failwith "eseq shouldn't exist"
