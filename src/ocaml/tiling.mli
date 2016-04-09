open Core.Std
open Func_context

module FreshReg   : Fresh.S
module FreshLabel : Fresh.S

(* Maximal munch tiling algorithm with baby tiles.
 *
 * For example, munch_expr takes in
 *     - the function context of the current function being munched,
 *     - a map of all function contexts, and
 *     - an IR expression; it returns
 *     - the name of the register holding the result and a list of asm
 *       instructions.
 * The other munch functions are similar.
 *
 * Here is our stack layout:
 *
 *                 | ...       |
 *                 | ARG8      | | args to this function
 *                 | ARG7      | |
 *                 | ARG6      |/
 *                 | saved rip |
 *         rbp --> | saved rbp |
 *                 | rax       |\
 *                 | rbx       | | callee-saved
 *                 | rcx       | |
 *                 | rdx       | |
 *                 | rsi       | |
 *                 | rdi       | |
 *                 | rbp       | | callee-saved
 *                 | rsp       | |
 *                 | r8        | |
 *                 | r9        | |
 *                 | r10       | |
 *                 | r11       | |
 *                 | r12       | | callee-saved
 *                 | r13       | | callee-saved
 *                 | r14       | | callee-saved
 *                 | r15       |/  callee-saved
 *                 | temp_0    |\
 *                 | temp_1    | | stack space for fake registers
 *                 | ...       | |
 *                 | temp_n    |/
 *                 | padding   | | optional padding to 16-byte align stack
 *                 | RETm      |\
 *                 | ...       | | returns from function being called
 *                 | RET4      | |
 *                 | RET3      |/
 *                 | ARGo      |\
 *                 | ...       | | arguments to function being called
 *                 | ARG7      | |
 *         rsp --> | ARG6      |/
 *
 * From top to bottom:
 *     - ARG6, ARG7, ... are passed to us above the saved return pointer
 *     - the return pointer is pushed on the stack by the call instruction
 *     - the enter instruction stores the old value of rbp and points rbp there
 *     - we save all callee-saved registers because we are a callee
 *     - we save all caller-saved registers because we call functions too
 *     - we have a stack location for every temp we  use
 *     - we pad to 16-bytes before calling a function
 *     - we stack allocate space for the function we're calling to return stuff
 *     - we then push ARG6, ARG7, ...
 *)
val munch_expr      : func_context -> func_contexts -> Ir.expr      -> Asm.fake * Asm.abstract_asm list
val munch_stmt      : func_context -> func_contexts -> Ir.stmt      -> Asm.abstract_asm list
val munch_func_decl :                 func_contexts -> Ir.func_decl -> Asm.abstract_asm list
val munch_comp_unit :                 func_contexts -> Ir.comp_unit -> Asm.abstract_asm list

(* Maximal munch tiling algorithm with good tiles. *)
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
