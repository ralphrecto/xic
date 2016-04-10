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

type asm_prog = asm list

(******************************************************************************)
(* asm helpers                                                                *)
(******************************************************************************)
(* offsets n words from reg contents *)
val ( $ ) : int -> abstract_reg -> abstract_reg mem

val const : int -> abstract_reg operand

(* `arg_reg i` returns the ith function argument register if 0 <= i <= 5. *)
val arg_reg : int -> reg option
(* `ret_reg i` returns the ith return value register if 0 <= i <= 1. *)
val ret_reg : int -> reg option

(* `callee_arg_op i` returns the ith function argument passed to this function.
 * We are accessing these arguments as a callee.
 *
 *      callee_arg_op 0 = %rdi
 *      callee_arg_op 1 = %rsi
 *      callee_arg_op 2 = %rdx
 *      callee_arg_op 3 = %rcx
 *      callee_arg_op 4 = %r8
 *      callee_arg_op 5 = %r9
 *      callee_arg_op 6 = $16(%rbp)
 *      callee_arg_op 7 = $24(%rbp)
 *)
val callee_arg_op : int -> abstract_reg operand

(* `callee_ret_op ret_ptr i` returns the ith return value of our function with
 * implicit 0th argument ret_ptr.  We are accessing these return values as a
 * callee.
 *
 *     callee_ret_op p 0 = %rax
 *     callee_ret_op p 1 = %rdx
 *     callee_ret_op p 2 = (%p)
 *     callee_ret_op p 3 = 8(%p)
 *     callee_ret_op p 4 = 16(%p)
 *
 * function context has max_args.. *)
val callee_ret_op : abstract_reg -> int -> abstract_reg operand

(* `caller_ret_op max_args i` returns the ith return value of the function we
 * just called assuming the max_args of our current function context is
 * max_args. We are accessing these return values as a caller.
 *
 *     caller_ret_op 0 = %rax
 *     caller_ret_op 1 = %rdx
 *     caller_ret_op 2 = $(8 * (max_args + 0))(%rsp)
 *     caller_ret_op 3 = $(8 * (max_args + 1))(%rsp)
 *     caller_ret_op 4 = $(8 * (max_args + 2))(%rsp)
 *
 * function context has max_args.. *)
val caller_ret_op : max_args:int -> i:int -> abstract_reg operand

module Abbreviations: sig
  (* abstract real registers *)
  val arax : abstract_reg operand
  val arbx : abstract_reg operand
  val arcx : abstract_reg operand
  val acl  : abstract_reg operand
  val ardx : abstract_reg operand
  val arsi : abstract_reg operand
  val ardi : abstract_reg operand
  val arbp : abstract_reg operand
  val arsp : abstract_reg operand
  val ar8  : abstract_reg operand
  val ar9  : abstract_reg operand
  val ar10 : abstract_reg operand
  val ar11 : abstract_reg operand
  val ar12 : abstract_reg operand
  val ar13 : abstract_reg operand
  val ar14 : abstract_reg operand
  val ar15 : abstract_reg operand

  (* abstract fake registers *)
  val fake : string -> abstract_reg operand
  val a : abstract_reg operand
  val b : abstract_reg operand
  val c : abstract_reg operand
  val w : abstract_reg operand
  val x : abstract_reg operand
  val y : abstract_reg operand
  val z : abstract_reg operand

  (* real registers *)
  val rax : reg operand
  val rbx : reg operand
  val rcx : reg operand
  val cl  : reg operand
  val rdx : reg operand
  val rsi : reg operand
  val rdi : reg operand
  val rbp : reg operand
  val rsp : reg operand
  val r8  : reg operand
  val r9  : reg operand
  val r10 : reg operand
  val r11 : reg operand
  val r12 : reg operand
  val r13 : reg operand
  val r14 : reg operand
  val r15 : reg operand

  (* Mem (Base (None, Rax)), Mem (Base (None, Rax)), ... *)
  val mrax : reg operand
  val mrbx : reg operand
  val mrcx : reg operand
  val mcl  : reg operand
  val mrdx : reg operand
  val mrsi : reg operand
  val mrdi : reg operand
  val mrbp : reg operand
  val mrsp : reg operand
  val mr8  : reg operand
  val mr9  : reg operand
  val mr10 : reg operand
  val mr11 : reg operand
  val mr12 : reg operand
  val mr13 : reg operand
  val mr14 : reg operand
  val mr15 : reg operand

  (* Memory operand constructors.
   *     mrax                  = (%rax),
   *     8L $ mrax             = $8(%rax)
   *     rax * 4               = (,%rax,4)
   *     8L $ (rax * 4)        = $8(,%rax,4)
   *     mrax + rbx * 4        = (%rax,%rbx,4)
   *     8L $ (mrax + rbx * 4) = $8(%rax,%rbx,4) *)
  val ( $ ) : int64 -> 'reg operand -> 'reg operand
  val ( * ) : 'reg -> int -> 'reg operand
  val ( + ) : 'reg operand -> 'reg operand -> 'reg operand
end

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
val string_of_abstract_asms : abstract_asm list -> string
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
(*assembler directives *)
val text : abstract_asm
val globl : string -> abstract_asm
val align : int -> abstract_asm

(* arithmetic *)
val addq : 'reg operand -> 'reg operand -> 'reg asm_template
val subq : 'reg operand -> 'reg operand -> 'reg asm_template
val incq : 'reg operand -> 'reg asm_template
val decq : 'reg operand -> 'reg asm_template
val negq : 'reg operand -> 'reg asm_template
val imulq : 'reg operand -> 'reg asm_template
val idivq : 'reg operand -> 'reg asm_template

(* logical/bitwise operations *)
val andq : 'reg operand -> 'reg operand -> 'reg asm_template
val orq : 'reg operand -> 'reg operand -> 'reg asm_template
val xorq : 'reg operand -> 'reg operand -> 'reg asm_template

(* bit test *)
val bt : 'reg operand -> 'reg operand -> 'reg asm_template

(* shifts *)
val shlq : 'reg operand -> 'reg operand -> 'reg asm_template
val shrq : 'reg operand -> 'reg operand -> 'reg asm_template
val sarq : 'reg operand -> 'reg operand -> 'reg asm_template

(* move/setting operations *)
(* val mov : 'reg operand -> 'reg operand -> 'reg asm_template *)
val movq : 'reg operand -> 'reg operand -> 'reg asm_template

val asete  : abstract_reg operand -> abstract_asm
val asetne : abstract_reg operand -> abstract_asm
val asetl  : abstract_reg operand -> abstract_asm
val asetg  : abstract_reg operand -> abstract_asm
val asetle : abstract_reg operand -> abstract_asm
val asetge : abstract_reg operand -> abstract_asm
val asetz  : abstract_reg operand -> abstract_asm
val asetnz : abstract_reg operand -> abstract_asm
val asets  : abstract_reg operand -> abstract_asm
val asetns : abstract_reg operand -> abstract_asm
val asetc  : abstract_reg operand -> abstract_asm
val asetnc : abstract_reg operand -> abstract_asm

val sete  : reg operand -> asm
val setne : reg operand -> asm
val setl  : reg operand -> asm
val setg  : reg operand -> asm
val setle : reg operand -> asm
val setge : reg operand -> asm
val setz  : reg operand -> asm
val setnz : reg operand -> asm
val sets  : reg operand -> asm
val setns : reg operand -> asm
val setc  : reg operand -> asm
val setnc : reg operand -> asm

(* load effective address *)
val leaq : 'reg operand -> 'reg operand -> 'reg asm_template

(* comparisons *)
val cmpq : 'reg operand -> 'reg operand -> 'reg asm_template

(* test *)
val test : 'reg operand -> 'reg operand -> 'reg asm_template

(* stack operations *)
val push : 'reg operand -> 'reg asm_template
val pushq : 'reg operand -> 'reg asm_template
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
val jc   : 'reg operand -> 'reg asm_template
val jnc  : 'reg operand -> 'reg asm_template
val call : 'reg operand -> 'reg asm_template

(* zeroops *)
val label_op : string -> 'reg asm_template
val leave : 'reg asm_template
val ret : 'reg asm_template
