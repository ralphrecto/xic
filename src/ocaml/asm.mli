open Core.Std

(******************************************************************************)
(* types                                                                      *)
(******************************************************************************)
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

type 'reg asm_template = private
  | BinOp  of string * 'reg operand * 'reg operand (* add %rax, %rbx *)
  | UnOp   of string * 'reg operand                (* jl foo *)
  | ZeroOp of string                               (* return *)

type abstract_asm = abstract_reg asm_template

type asm = reg asm_template

(******************************************************************************)
(* instructions                                                               *)
(******************************************************************************)
(* binops *)
val addq : 'reg operand -> 'reg operand -> 'reg asm_template
val leaq : 'reg operand -> 'reg operand -> 'reg asm_template

(* unops *)
val push : 'reg operand -> 'reg asm_template
val pop  : 'reg operand -> 'reg asm_template

val jmp  : 'reg operand -> 'reg asm_template
val je   : 'reg operand -> 'reg asm_template
val jne  : 'reg operand -> 'reg asm_template
val jz   : 'reg operand -> 'reg asm_template
val jg   : 'reg operand -> 'reg asm_template
val jge  : 'reg operand -> 'reg asm_template
val jl   : 'reg operand -> 'reg asm_template
val jle  : 'reg operand -> 'reg asm_template
val call : 'reg operand -> 'reg asm_template

(* zeroops *)
val ret : 'reg asm_template
