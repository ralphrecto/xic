open Core.Std
open Func_context

module FreshReg   : Fresh.S
module FreshLabel : Fresh.S

(* current func context -> map of all contexts -> expr ->
 * reg containing expr value, result assembly *)
val munch_expr : func_context -> func_contexts -> Ir.expr -> Asm.abstract_reg * Asm.abstract_asm list
(* current func context -> map of all contexts -> stmt -> assembly *)
val munch_stmt : func_context -> func_contexts -> Ir.stmt -> Asm.abstract_asm list
val munch_func_decl : func_contexts -> Ir.func_decl -> Asm.abstract_asm list
val munch_comp_unit : func_contexts -> Ir.comp_unit -> Asm.abstract_asm list
val chomp_expr      : func_context -> func_contexts -> Ir.expr -> Asm.abstract_reg * Asm.abstract_asm list
val chomp_stmt      : func_context -> func_contexts -> Ir.stmt -> Asm.abstract_asm list
(* Naive register allocation.
 *
 * It is assumed that you call register_allocate with the body of a function.
 *
 * First, the list of fake names appearing anywhere in the assembly is gathered
 * in the order in which they first appear. See Asm.fakes_of_asms for more
 * information.
 *
 * Next, fake names are associated with a position in the stack in the order in
 * which they were gathered. For example, the first fake names is spilled to
 * -8(%rbp), the next fake names is spilled to -16(%rbp), etc.
 *
 * Next, for each assembly instruction the values of each fake register is read
 * from the stack before the instruction and written to the stack after the
 * instruction. The fake registers are assigned to registers r13, r14, and r15
 * in the order they appear. For example, the following abstract assembly.
 *
 *     op "foo", "bar", "baz"
 *
 * is converted to the following assembly:
 *
 *     mov -8(%rbp), %rax  \
 *     mov -16(%rbp), %rbx  } read from stack
 *     mov -24(%rbp), %rcx /
 *     op %rax, %rbx, %rcx  } translated op
 *     mov %rax, -8(%rbp)  \
 *     mov %rbx, -16(%rbp)  } written to stack
 *     mov %rcx, -24(%rbp) /
 *
 * It is heavily assumed that r13, r14, and r15 are not produced in abstract
 * assembly!
 *
 * Note that this is all register allocate does! It doesn't translate ARGi
 * registers, it doesn't translate RETi registers, it doesn't prepend enter
 * instructions or append leave instructions. All these operations should be
 * performed as the abstract assembly is being produced.
 *)
val register_allocate : Asm.abstract_asm list -> Asm.asm list
