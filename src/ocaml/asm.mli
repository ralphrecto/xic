open Core.Std

(******************************************************************************)
(* types                                                                      *)
(******************************************************************************)
type label = string

type const = int64

type reg =
  | Rax
  | Rbx
  | Rcx
  | Cl
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

type abstract_reg =
  | Fake of string
  | Real of reg

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

(* arithmetic *)
val addq : 'reg operand -> 'reg operand -> 'reg asm_template
val subq : 'reg operand -> 'reg operand -> 'reg asm_template
val imulq : 'reg operand -> 'reg asm_template
val idivq : 'reg operand -> 'reg asm_template

(* logical/bitwise operations *)
val andq : 'reg operand -> 'reg operand -> 'reg asm_template
val orq : 'reg operand -> 'reg operand -> 'reg asm_template
val xorq : 'reg operand -> 'reg operand -> 'reg asm_template

(* shifts *)
val salq : 'reg operand -> 'reg operand -> 'reg asm_template 
val shrq : 'reg operand -> 'reg operand -> 'reg asm_template
val sarq : 'reg operand -> 'reg operand -> 'reg asm_template

(* move/setting operations *)
val movq : 'reg operand -> 'reg operand -> 'reg asm_template
val sete : 'reg operand -> 'reg asm_template
val setne : 'reg operand -> 'reg asm_template
val setl : 'reg operand -> 'reg asm_template
val setg : 'reg operand -> 'reg asm_template
val setle : 'reg operand -> 'reg asm_template
val setge : 'reg operand -> 'reg asm_template

(* comparisons *)
val cmpq : 'reg operand -> 'reg operand -> 'reg asm_template
val leaq : 'reg operand -> 'reg operand -> 'reg asm_template

(* stack operations *)
val push : 'reg operand -> 'reg asm_template
val pop  : 'reg operand -> 'reg asm_template

(* jumps *)
val jmp  : 'reg operand -> 'reg asm_template
val je   : 'reg operand -> 'reg asm_template
val jne  : 'reg operand -> 'reg asm_template
val jz   : 'reg operand -> 'reg asm_template
val jg   : 'reg operand -> 'reg asm_template
val jge  : 'reg operand -> 'reg asm_template
val jl   : 'reg operand -> 'reg asm_template
val jle  : 'reg operand -> 'reg asm_template
val call : 'reg operand -> 'reg asm_template
val ret : 'reg asm_template
