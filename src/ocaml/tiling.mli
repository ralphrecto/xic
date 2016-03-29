open Core.Std

module FreshReg   : Fresh.S
module FreshLabel : Fresh.S

val munch_expr : Ir.expr -> Asm.abstract_reg * Asm.abstract_asm list
val munch_stmt : Ir.stmt -> Asm.abstract_asm list

val register_allocate : Asm.abstract_asm list -> Asm.asm list
