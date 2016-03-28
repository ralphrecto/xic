type label = string

type const = int64

type abstract_reg = int

type reg =
  | Rax
  | Rbx
  | Rcx
  | Rdx
  | Rsi
  | Rdi
  | Rbp
  | Rsp
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15

type scale =
  | One
  | Two
  | Four
  | Eight

type 'reg mem =
  | Base    of const option * 'reg                 (* (%rax),        $8(%rax)        *)
  | Off     of const option * 'reg * scale         (* (,%rax,4),     $8(,%rax,4)     *)
  | BaseOff of const option * 'reg * 'reg * scale  (* (%rax,%rbx,4), $8(%rax,%rbx,4) *)

type 'reg operand =
  | Label of label
  | Reg   of 'reg
  | Const of const
  | Mem   of 'reg mem

type 'reg asm_template =
  | Binop of string * 'reg operand * 'reg operand
  | Unop  of string * 'reg operand

type abstract_asm = abstract_reg asm_template

type asm = reg asm_template

let addq (a: 'reg operand) (b: 'reg operand) : 'reg asm_template =
  match a, b with
  | Reg _, Reg _
  | Reg _, Mem _
  | Mem _, Reg _
  | Reg _, Const _
  | Mem _, Const _ -> Binop ("addq", a, b)
  | _ -> failwith "bad"

let je (a: 'reg operand) : 'reg asm_template =
  match a with
  | Label _ -> Unop ("je", a)
  | _ -> failwith "bad"
