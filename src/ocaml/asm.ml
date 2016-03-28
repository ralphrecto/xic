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
  | Base    of const option * 'reg
  | Off     of const option * 'reg * scale
  | BaseOff of const option * 'reg * 'reg * scale

type 'reg operand =
  | Label of label
  | Reg   of 'reg
  | Const of const
  | Mem   of 'reg mem

type 'reg asm_template =
  | BinOp  of string * 'reg operand * 'reg operand
  | UnOp   of string * 'reg operand
  | ZeroOp of string

type abstract_asm = abstract_reg asm_template

type asm = reg asm_template

(******************************************************************************)
(* instructions                                                               *)
(******************************************************************************)
let die () =
  failwith "invalid assembly instruction"

(* binops *)
let addq (a: 'reg operand) (b: 'reg operand) : 'reg asm_template =
  match a, b with
  | Reg _, Reg _
  | Reg _, Mem _
  | Mem _, Reg _
  | Const _, Reg _
  | Const _, Mem _ -> BinOp ("addq", a, b)
  | _ -> failwith "bad"

let leaq (a: 'reg operand) (b: 'reg operand) : 'reg asm_template =
  match a, b with
  | Mem _, Reg _ -> BinOp ("leaq", a, b)
  | _ -> failwith "bad"

(* unops *)
let push a =
  match a with
  | Reg _ | Mem _ | Const _ -> UnOp ("push", a)
  | _ -> die ()

let pop a =
  match a with
  | Reg _ | Mem _ -> UnOp ("pop", a)
  | _ -> die ()

let unop_label (op: string) (l: 'reg operand) =
  match l with
  | Label _ -> UnOp (op, l)
  | _ -> die ()
let jmp  l = unop_label "jmp"  l
let je   l = unop_label "je"   l
let jne  l = unop_label "jne"  l
let jz   l = unop_label "jz"   l
let jg   l = unop_label "jg"   l
let jge  l = unop_label "jge"  l
let jl   l = unop_label "jl"   l
let jle  l = unop_label "jle"  l
let call l = unop_label "call" l

(* zeroops *)
let ret = ZeroOp "ret"
