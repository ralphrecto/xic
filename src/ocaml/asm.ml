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

type fake = string
type abstract_reg =
  | Fake of fake
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
  | Lab of label
  | Directive of string * string list
  | Comment of string

type abstract_asm = abstract_reg asm_template

type asm = reg asm_template

type asm_prog = asm list

(******************************************************************************)
(* important register helpers                                                 *)
(******************************************************************************)
let ( $ ) n reg =
  Base (Some (Int64.of_int n), reg)

let const n =
  Const (Int64.of_int n)

let arg_reg = function
  | 0 -> Some Rdi
  | 1 -> Some Rsi
  | 2 -> Some Rdx
  | 3 -> Some Rcx
  | 4 -> Some R8
  | 5 -> Some R9
  | _ -> None

let ret_reg = function
  | 0 -> Some Rax
  | 1 -> Some Rdx
  | _ -> None

let callee_arg_op i =
  match arg_reg i with
  | Some r -> Reg (Real r)
  | None ->
      (*
       * | ...          |
       * +--------------+
       * | ARG7         |
       * +--------------+
       * | ARG6         |
       * +--------------+
       * | ret pointer  |
       * +--------------+
       * | saved rbp    | <--- rbp
       * +--------------+
       * | ...          |
       *)
      let offset = (i - 6 + 2) * 8 in
      Mem (offset$(Real Rbp))

let callee_ret_op ret_ptr i =
  match ret_reg i with
  | Some r -> Reg (Real r)
  | None -> Mem ((8 * (i - 2)) $ ret_ptr)

let caller_ret_op ~max_args ~i =
  match ret_reg i with
  | Some r -> Reg (Real r)
  | None ->
      (*
       * | ret_3          |
       * +----------------+
       * | ret_2          |
       * +----------------+
       * | arg_{max_args} |
       * +----------------+
       * | ...            |
       * +----------------+
       * | arg_1          |
       * +----------------+
       * | arg_0          | <--- rsp
       * +----------------+
       * | garbage        |
       *)
      let offset = ((i-2) + (max (max_args-6) 0)) * 8 in
      Mem (offset $ (Real Rsp))

module Abbreviations = struct
  let arax = Reg (Real Rax)
  let arbx = Reg (Real Rbx)
  let arcx = Reg (Real Rcx)
  let acl  = Reg (Real Cl)
  let ardx = Reg (Real Rdx)
  let arsi = Reg (Real Rsi)
  let ardi = Reg (Real Rdi)
  let arbp = Reg (Real Rbp)
  let arsp = Reg (Real Rsp)
  let ar8  = Reg (Real R8)
  let ar9  = Reg (Real R9)
  let ar10 = Reg (Real R10)
  let ar11 = Reg (Real R11)
  let ar12 = Reg (Real R12)
  let ar13 = Reg (Real R13)
  let ar14 = Reg (Real R14)
  let ar15 = Reg (Real R15)

  let fake s = Reg (Fake s)
  let a = fake "a"
  let b = fake "b"
  let c = fake "c"
  let w = fake "w"
  let x = fake "x"
  let y = fake "y"
  let z = fake "z"

  let rax = Reg Rax
  let rbx = Reg Rbx
  let rcx = Reg Rcx
  let cl  = Reg Cl
  let rdx = Reg Rdx
  let rsi = Reg Rsi
  let rdi = Reg Rdi
  let rbp = Reg Rbp
  let rsp = Reg Rsp
  let r8  = Reg R8
  let r9  = Reg R9
  let r10 = Reg R10
  let r11 = Reg R11
  let r12 = Reg R12
  let r13 = Reg R13
  let r14 = Reg R14
  let r15 = Reg R15

  let mrax = Mem (Base (None, Rax))
  let mrbx = Mem (Base (None, Rbx))
  let mrcx = Mem (Base (None, Rcx))
  let mcl  = Mem (Base (None, Cl))
  let mrdx = Mem (Base (None, Rdx))
  let mrsi = Mem (Base (None, Rsi))
  let mrdi = Mem (Base (None, Rdi))
  let mrbp = Mem (Base (None, Rbp))
  let mrsp = Mem (Base (None, Rsp))
  let mr8  = Mem (Base (None, R8 ))
  let mr9  = Mem (Base (None, R9 ))
  let mr10 = Mem (Base (None, R10))
  let mr11 = Mem (Base (None, R11))
  let mr12 = Mem (Base (None, R12))
  let mr13 = Mem (Base (None, R13))
  let mr14 = Mem (Base (None, R14))
  let mr15 = Mem (Base (None, R15))

  let ($) n mem =
    match mem with
    | Mem (Base (None, b)) -> Mem (Base (Some n, b))
    | Mem (Off (None, o, s)) -> Mem (Off (Some n, o, s))
    | Mem (BaseOff (None, b, o, s)) -> Mem (BaseOff (Some n, b, o, s))
    | _ -> failwith "invalid $ application"

  let ( * ) reg scale =
    match scale with
    | 1 -> Mem (Off (None, reg, One))
    | 2 -> Mem (Off (None, reg, Two))
    | 4 -> Mem (Off (None, reg, Four))
    | 8 -> Mem (Off (None, reg, Eight))
    | _ -> failwith "invalid scale"

  let (+) base off =
    match base, off with
    | Mem (Base (Some n, r)), Mem (Off (None, b, o))
    | Mem (Base (None, r)), Mem (Off (Some n, b, o)) -> Mem (BaseOff (Some n, r, b, o))
    | Mem (Base (None, r)), Mem (Off (None, b, o)) -> Mem (BaseOff (None, r, b, o))
    | _ -> failwith "invalid + application"
end


(******************************************************************************)
(* functions                                                                  *)
(******************************************************************************)
let string_of_const n =
  sprintf "$%s" (Int64.to_string n)

let string_of_label l =
  l

let string_of_reg reg =
  match reg with
  | Rax -> "%rax"
  | Rbx -> "%rbx"
  | Rcx -> "%rcx"
  | Cl  -> "%cl"
  | Rdx -> "%rdx"
  | Rsi -> "%rsi"
  | Rdi -> "%rdi"
  | Rbp -> "%rbp"
  | Rsp -> "%rsp"
  | R8  -> "%r8"
  | R9  -> "%r9"
  | R10 -> "%r10"
  | R11 -> "%r11"
  | R12 -> "%r12"
  | R13 -> "%r13"
  | R14 -> "%r14"
  | R15 -> "%r15"

let string_of_abstract_reg reg =
  match reg with
  | Fake s -> "%" ^ s
  | Real r -> string_of_reg r

let string_of_scale scale =
  match scale with
  | One   -> "1"
  | Two   -> "2"
  | Four  -> "4"
  | Eight -> "8"

let string_of_mem_const n = Int64.to_string n

let string_of_mem f mem =
  let sos = string_of_scale in
  let somc = string_of_mem_const in

  match mem with
  | Base (None, b) -> sprintf "(%s)" (f b)
  | Base (Some n, b) -> sprintf "%s(%s)" (somc n) (f b)
  | Off (None, o, s) -> sprintf "(,%s,%s)" (f o) (sos s)
  | Off (Some n, o, s) -> sprintf "%s(,%s,%s)" (somc n) (f o) (sos s)
  | BaseOff (None, b, o, s) -> sprintf "(%s,%s,%s)" (f b) (f o) (sos s)
  | BaseOff (Some n, b, o, s) -> sprintf "%s(%s,%s,%s)" (somc n) (f b) (f o) (sos s)

let string_of_operand f o =
  match o with
  | Label l -> string_of_label l
  | Reg r -> f r
  | Const c -> string_of_const c
  | Mem m -> string_of_mem f m

let string_of_asm_template f asm =
  let comma_spaces ss = String.concat ~sep:", " ss in
  let soo = string_of_operand f in
  match asm with
  | Op (s, operands) -> sprintf "    %s %s" s (comma_spaces (List.map ~f:soo operands))
  | Lab s -> s ^ ":"
  | Directive (d, args) -> sprintf "    .%s %s" d (comma_spaces args)
  | Comment s -> sprintf "    # %s" s

let string_of_abstract_asm asm =
  string_of_asm_template string_of_abstract_reg asm

let string_of_abstract_asms asms =
  Util.join (List.map ~f:string_of_abstract_asm asms)

let string_of_asm asm =
  string_of_asm_template string_of_reg asm

let string_of_asms asms =
  Util.join (List.map ~f:string_of_asm asms)

let fakes_of_reg r =
  match r with
  | Fake s -> [s]
  | Real _ -> []

let fakes_of_regs rs =
  Util.ordered_dedup (List.concat_map ~f:fakes_of_reg rs)

let fakes_of_operand o =
  match o with
  | Reg r -> fakes_of_reg r
  | Mem (Base (_, r)) -> fakes_of_reg r
  | Mem (Off (_, r, _)) -> fakes_of_reg r
  | Mem (BaseOff (_, r1, r2, _)) -> fakes_of_regs [r1; r2]
  | Label _
  | Const _ -> []

let fakes_of_operands os =
  Util.ordered_dedup (List.concat_map ~f:fakes_of_operand os)

let fakes_of_asm asm =
  match asm with
  | Op (_, ops) -> Util.ordered_dedup (List.concat_map ~f:fakes_of_operand ops)
  | Lab _
  | Directive _
  | Comment _ -> []

let fakes_of_asms asms =
  Util.ordered_dedup (List.concat_map ~f:fakes_of_asm asms)

(******************************************************************************)
(* instructions                                                               *)
(******************************************************************************)
let die () =
  failwith "invalid assembly instruction"

(* assembler directives *)
let text : abstract_asm = Directive ("text", [])

let globl (label: string) : abstract_asm =
  Directive ("globl", [label])

let align (n: int) : abstract_asm =
  if n < 1 then die ()
  else Directive ("align", [string_of_int n])

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
let negq src = unop_arith_generic "negq" src

(* logical/bitwise operations *)
let logic_generic logic_name src dest =
  match src, dest with
  | _, (Mem _ | Reg _) -> Op (logic_name, [src; dest])
  | _ -> die ()

let andq src dest = logic_generic "andq" src dest
let orq src dest = logic_generic "orq" src dest
let xorq src dest = logic_generic "xorq" src dest

(* bit test *)
let bt bit src = logic_generic "bt" bit src

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

(* let mov src dest = mov_generic "mov" src dest *)
let movq src dest = mov_generic "movq" src dest

let set_abstract setname dest =
  match dest with
  | Reg (Real Cl) -> Op (setname, [dest])
  | _ -> die ()

let set_real setname dest =
  match dest with
  | Reg Cl -> Op (setname, [dest])
  | _ -> die ()

let asete  dest = set_abstract "sete"  dest
let asetne dest = set_abstract "setne" dest
let asetl  dest = set_abstract "setl"  dest
let asetg  dest = set_abstract "setg"  dest
let asetle dest = set_abstract "setle" dest
let asetge dest = set_abstract "setge" dest
let asetz  dest = set_abstract "setz"  dest
let asetnz dest = set_abstract "setnz" dest
let asets  dest = set_abstract "sets"  dest
let asetns dest = set_abstract "setns" dest
let asetc  dest = set_abstract "setc"  dest
let asetnc dest = set_abstract "setnc" dest

let sete  dest = set_real "sete"  dest
let setne dest = set_real "setne" dest
let setl  dest = set_real "setl"  dest
let setg  dest = set_real "setg"  dest
let setle dest = set_real "setle" dest
let setge dest = set_real "setge" dest
let setz  dest = set_real "setz"  dest
let setnz dest = set_real "setnz" dest
let sets  dest = set_real "sets"  dest
let setns dest = set_real "setns" dest
let setc  dest = set_real "setc"  dest
let setnc dest = set_real "setnc" dest

(* laod effective address *)
let leaq src dest =
  match src, dest with
  | Mem _, Reg _ -> Op ("leaq", [src; dest])
  | _ -> die ()

(* comparisons *)
let cmpq a b =
  match a, b with
  | Mem _, Mem _ -> die ()
  | _, (Reg _ | Mem _) -> Op ("cmpq", [a; b])
  | _ -> die ()

(* test *)
let test a b =
  match a, b with
  | Reg _, (Reg _ | Mem _) -> Op ("test", [a; b])
  | _ -> die ()

(* stack operations *)
let push a =
  match a with
  | Reg _ | Mem _ | Const _ -> Op ("push", [a])
  | _ -> die ()

let pushq a =
  match a with
  | Reg _ | Mem _ | Const _ -> Op ("pushq", [a])
  | _ -> die ()

let pop a =
  match a with
  | Reg _ | Mem _ -> Op ("pop", [a])
  | _ -> die ()

let enter a b =
  match a, b with
  | Const _, Const _ -> Op ("enter", [a; b])
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
let js   l = unop_label "js"   l
let jns  l = unop_label "jns"  l
let jc   l = unop_label "jc"   l
let jnc  l = unop_label "jnc"  l
let call l = unop_label "call" l

(* zeroops *)
let label_op l = Lab l
let leave = Op ("leave", [])
let ret = Op ("retq", [])
