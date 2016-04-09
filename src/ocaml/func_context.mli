open Core.Std

(* function info helpful for code generation *)
type func_context = {
  (* number of args/rets of this function *)
  num_args : int;
  num_rets : int;
  (* max number of args/rets spilled of all functions called by this function *)
  max_args : int;
  (* includes ret pointer args *)
  max_rets : int;
}

(* abi compliant names -> func_context *)
type func_contexts = func_context String.Map.t

val get_context_map : Typecheck.callable_decl list -> Ir.comp_unit -> func_contexts
