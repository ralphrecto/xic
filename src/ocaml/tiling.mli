open Core.Std

module FreshReg   : Fresh.S
module FreshLabel : Fresh.S

(* function metadata that is useful for asm generation *)
type func_context = {
  num_args : int;
  num_rets : int;
}

type func_contexts = func_context String.Map.t

val munch_expr : func_context -> Ir.expr -> Asm.abstract_reg * Asm.abstract_asm list
val munch_stmt : func_context -> Ir.stmt -> Asm.abstract_asm list
val munch_func_decl : func_context -> Ir.func_decl -> Asm.abstract_asm list
val munch_comp_unit : Ir.comp_unit -> Asm.abstract_asm list

val register_allocate : Asm.abstract_asm list -> Asm.asm list
