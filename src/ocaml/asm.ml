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
  | Base    of const option * 'reg
  | Off     of const option * 'reg * scale
  | BaseOff of const option * 'reg * 'reg * scale

type 'reg operand =
  | Label of label
  | Reg   of 'reg
  | Const of const
  | Mem   of 'reg mem

type 'reg asm_template =
  | Op of string * 'reg operand list
  | Directive of string * string list

type abstract_asm = abstract_reg asm_template

type asm = reg asm_template

(******************************************************************************)
(* instructions                                                               *)
(******************************************************************************)
let die () =
  failwith "invalid assembly instruction"

(* arithmetic *)
let binop_arith_generic (arith_name: string)
  (src: 'reg operand) (dest: 'reg operand) : 'reg asm_template =
  match src, dest with
  | Reg _, Reg _
  | Reg _, Mem _
  | Mem _, Reg _
  | Const _, Reg _
  | Const _, Mem _ -> Op (arith_name, [src; dest])
  | _ -> die ()

let unop_arith_generic
  (arith_name: string)
  (src: 'reg operand)
    : 'reg asm_template =
  match src with
  | (Reg _ | Mem _ ) -> Op (arith_name, [src])
  | _ -> die ()

let addq src dest = binop_arith_generic "addq" src dest
let subq src dest = binop_arith_generic "subq" src dest
let incq dest = unop_arith_generic "incq" dest
let decq dest = unop_arith_generic "decq" dest
let imulq src = unop_arith_generic "imulq" src
let idivq src = unop_arith_generic "idivq" src


(* logical/bitwise operations *)
let logic_generic logic_name src dest =
  match src, dest with
  | _, (Mem _ | Reg _) -> Op (logic_name, [src; dest])
  | _ -> die ()

let andq src dest = logic_generic "andq" src dest
let orq src dest = logic_generic "orq" src dest
let xorq src dest = logic_generic "xorq" src dest


(* shifts *)
let shift_generic shiftname a b =
  match a, b with
  | (Reg _ | Const _ ), (Mem _ | Reg _ ) -> Op (shiftname, [a; b])
  | _ -> die ()

let shlq a b = shift_generic "shlq" a b
let shrq a b = shift_generic "shrq" a b
let sarq a b = shift_generic "sarq" a b


(* move/setting operations *)
let mov_generic mov_name src dest =
  match src, dest with
  | Mem _, Mem _ -> die ()
  | _, (Mem _ | Reg _ ) -> Op (mov_name, [src; dest])
  | _ -> die ()

let mov src dest = mov_generic "mov" src dest
let movq src dest = mov_generic "movq" src dest

let set_generic setname dest =
  match dest with
  | Mem _ | Reg _ -> Op (setname, [dest])
  | _ -> die ()

let sete dest = set_generic "sete" dest
let setne dest = set_generic "setne" dest
let setl dest = set_generic "setl" dest
let setg dest = set_generic "setg" dest
let setle dest = set_generic "setle" dest
let setge dest = set_generic "setge" dest


(* comparisons *)
let cmpq a b =
  match a, b with
  | Mem _, Mem _ -> die ()
  | _, (Reg _ | Mem _) -> Op ("cmpq", [a; b])
  | _ -> die ()

let leaq (a: 'reg operand) (b: 'reg operand) : 'reg asm_template =
  match a, b with
  | Mem _, Reg _ -> Op ("leaq", [a; b])
  | _ -> failwith "bad"


(* stack operations *)
let push a =
  match a with
  | Reg _ | Mem _ | Const _ -> Op ("push", [a])
  | _ -> die ()

let pop a =
  match a with
  | Reg _ | Mem _ -> Op ("pop", [a])
  | _ -> die ()


(* jumps *)
let unop_label (op: string) (l: 'reg operand) =
  match l with
  | Label _ -> Op (op, [l])
  | _ -> die ()

let jmp  l = unop_label "jmp"  l
let je   l = unop_label "je"   l
let jne  l = unop_label "jne"  l
let jnz  l = unop_label "jnz"  l
let jz   l = unop_label "jz"   l
let jg   l = unop_label "jg"   l
let jge  l = unop_label "jge"  l
let jl   l = unop_label "jl"   l
let jle  l = unop_label "jle"  l
let call l = unop_label "call" l

(* zeroops *)
let label_op l = Op (l^":", [])
let leave = Op ("leave", [])
let ret = Op ("retq", [])
