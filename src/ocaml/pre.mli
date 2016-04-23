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

(** Preprocessing Step.
 *
 * Insert a dummy node along all edges entering a node
 * with more than one predecessor. Each dummy node is of the form
 *
 *     Node {num=i; ir=dummy_ir}.
 *
 * where i is fresh starting originally 1 larger than the largest num in the
 * graph. Nodes are visited in order of their nums; edges are visited in the
 * order of the num of the src. Note that the graph is modified in place! *)
val dummy_ir : Ir.stmt
val preprocess : Cfg.IrCfg.t -> unit

(** Anticipated Expressions (a.k.a. Very Busy Expressions) *)
module BusyExprLattice : sig
  type data = ExprSet.t
  include Dataflow.LowerSemilattice with type data := data
end

module BusyExprCFG : sig
  include Dataflow.CFGWithLatticeT with
    module Lattice = BusyExprLattice and
    module CFG = Cfg.IrCfg
end

(** Available Expressions *)
module AvailExprLattice : sig
  type data = ExprSet.t
  include Dataflow.LowerSemilattice with type data := data
end

module AvailExprCFG : sig
  include Dataflow.CFGWithLatticeT with
    module Lattice = BusyExprLattice and
    module CFG = Cfg.IrCfg
end

(** Postponable Expressions *)
module PostponeExprLattice : sig
  type data = ExprSet.t
  include Dataflow.LowerSemilattice with type data := data
end

module PostponeExprCFG : sig
  include Dataflow.CFGWithLatticeT with
    module Lattice = PostponeExprLattice and
    module CFG = Cfg.IrCfg
end
