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

type 'reg asm_template =
  | Op of string * 'reg operand list  (* size <= 3 *)
  | Lab of label
  | Directive of string * string list (* e.g. .align 4, .globl foo *)

type abstract_asm = abstract_reg asm_template

type asm = reg asm_template

(******************************************************************************)
(* asm helpers                                                                *)
(******************************************************************************)
(* function argument registers, 0 to 5 *)
val arg_reg : int -> reg
(* return value registers, 0 to 1 *)
val ret_reg : int -> reg

(* offsets n words from reg contents *)
val ( $ ) : int -> abstract_reg -> abstract_reg mem

val const : int -> abstract_reg operand 

(* helpful constants *)
val num_caller_save : int

(******************************************************************************)
(* functions                                                                  *)
(******************************************************************************)
(* Pretty prints assembly using the GNU assembler syntax. *)
val string_of_const : const -> string
val string_of_label : label -> string
val string_of_reg : reg -> string
val string_of_abstract_reg : abstract_reg -> string
val string_of_scale : scale -> string
val string_of_mem : ('reg -> string) -> 'reg mem -> string
val string_of_operand : ('reg -> string) -> 'reg operand -> string
val string_of_asm_template : ('reg -> string) -> 'reg asm_template -> string
val string_of_abstract_asm : abstract_asm -> string
val string_of_asm : asm -> string
val string_of_asms : asm list -> string

(* Returns all the _unique_ fakes names in a register, operand, or assembly
 * instruction. The returned names are returned in the order in which they
 * first appear. For example, calling fakes_of_asms on the following assembly:
 *
 *     mov %rax %foo
 *     mov %bar %rbx
 *     push -8(%baz, %moo, 4)
 *     leave
 *     svd %bar, %baz, %foo
 *     ret
 *
 * returns ["foo"; "bar"; "baz"; "moo"].
 * *)
val fakes_of_reg      : abstract_reg              -> string list
val fakes_of_regs     : abstract_reg list         -> string list
val fakes_of_operand  : abstract_reg operand      -> string list
val fakes_of_operands : abstract_reg operand list -> string list
val fakes_of_asm      : abstract_asm              -> string list
val fakes_of_asms     : abstract_asm list         -> string list

(******************************************************************************)
(* instructions                                                               *)
(******************************************************************************)
(* arithmetic *)
val addq : 'reg operand -> 'reg operand -> 'reg asm_template
val subq : 'reg operand -> 'reg operand -> 'reg asm_template
val incq : 'reg operand -> 'reg asm_template
val decq : 'reg operand -> 'reg asm_template
val imulq : 'reg operand -> 'reg asm_template
val idivq : 'reg operand -> 'reg asm_template
val negq : 'reg operand -> 'reg asm_template

(* logical/bitwise operations *)
val andq : 'reg operand -> 'reg operand -> 'reg asm_template
val orq : 'reg operand -> 'reg operand -> 'reg asm_template
val xorq : 'reg operand -> 'reg operand -> 'reg asm_template

(* shifts *)
val shlq : 'reg operand -> 'reg operand -> 'reg asm_template
val shrq : 'reg operand -> 'reg operand -> 'reg asm_template
val sarq : 'reg operand -> 'reg operand -> 'reg asm_template

(* move/setting operations *)
val mov : 'reg operand -> 'reg operand -> 'reg asm_template
val movq : 'reg operand -> 'reg operand -> 'reg asm_template
val sete : 'reg operand -> 'reg asm_template
val setne : 'reg operand -> 'reg asm_template
val setl : 'reg operand -> 'reg asm_template
val setg : 'reg operand -> 'reg asm_template
val setle : 'reg operand -> 'reg asm_template
val setge : 'reg operand -> 'reg asm_template
val setz : 'reg operand -> 'reg asm_template
val setnz : 'reg operand -> 'reg asm_template
val sets : 'reg operand -> 'reg asm_template
val setns : 'reg operand -> 'reg asm_template

(* load effective address *)
val leaq : 'reg operand -> 'reg operand -> 'reg asm_template

(* comparisons *)
val cmpq : 'reg operand -> 'reg operand -> 'reg asm_template
val leaq : 'reg operand -> 'reg operand -> 'reg asm_template

(* test *)
val test : 'reg operand -> 'reg operand -> 'reg asm_template

(* stack operations *)
val push : 'reg operand -> 'reg asm_template
val pop  : 'reg operand -> 'reg asm_template
val enter : 'reg operand -> 'reg operand -> 'reg asm_template

(* jumps *)
val jmp  : 'reg operand -> 'reg asm_template
val je   : 'reg operand -> 'reg asm_template
val jne  : 'reg operand -> 'reg asm_template
val jnz  : 'reg operand -> 'reg asm_template
val jz   : 'reg operand -> 'reg asm_template
val jg   : 'reg operand -> 'reg asm_template
val jge  : 'reg operand -> 'reg asm_template
val jl   : 'reg operand -> 'reg asm_template
val jle  : 'reg operand -> 'reg asm_template
val js   : 'reg operand -> 'reg asm_template
val jns  : 'reg operand -> 'reg asm_template
val call : 'reg operand -> 'reg asm_template

(* zeroops *)
val label_op : string -> 'reg asm_template
val leave : 'reg asm_template
val ret : 'reg asm_template
