open Core.Std

(** helper functions *)
module ExprSet : sig
  include Set.S with type Elt.t = Ir.expr
end

val get_subexpr      : Ir.expr -> ExprSet.t
val get_subexpr_stmt : Ir.stmt -> ExprSet.t
val get_mem_temp     : Ir.expr -> ExprSet.t

val kill_func_args : Ir.expr list -> ExprSet.t
val kill_expr      : Ir.expr -> ExprSet.t
val kill_stmt      : Ir.stmt -> ExprSet.t

(** Anticipated Expressions (a.k.a. Very Busy Expressions) *)
module BusyExprLattice : sig
  type data =
    | Univ
    | Set of ExprSet.t
  include Dataflow.LowerSemilattice with type data := data
end

module BusyExprCFG : sig
  include Dataflow.CFGWithLatticeT with
    module Lattice = BusyExprLattice and
    module CFG = Cfg.IrCfg
end
