open Core.Std

(** Types and Helper Functions *)
module ExprSet : sig
  include Set.S with type Elt.t = Ir.expr
  val to_string : t -> string
end

module ExprSetIntersectLattice : sig
  include Dataflow.LowerSemilattice with type data = ExprSet.t
end

module ExprSetUnionLattice : sig
  include Dataflow.LowerSemilattice with type data = ExprSet.t
end

module type ExprSetIntersectCFG = sig
  include Dataflow.CFGWithLatticeT with
    module Lattice = ExprSetIntersectLattice and
    module CFG = Cfg.IrCfg
end

module type ExprSetUnionCFG = sig
  include Dataflow.CFGWithLatticeT with
    module Lattice = ExprSetUnionLattice and
    module CFG = Cfg.IrCfg
end

val get_subexpr      : Ir.expr -> ExprSet.t
val get_subexpr_stmt : Ir.stmt -> ExprSet.t
val get_mem_temp     : Ir.expr -> ExprSet.t

val kill_func_args : Ir.expr list -> ExprSet.t
val kill_expr      : Ir.expr -> ExprSet.t
val kill_stmt      : Ir.stmt -> ExprSet.t

(**
 * Preprocessing Step.
 *
 * Insert a dummy node along all edges entering a node
 * with more than one predecessor. Each dummy node is of the form
 *
 *     Node {num=i; ir=dummy_ir}.
 *
 * where i is fresh starting originally 1 larger than the largest num in the
 * graph. Nodes are visited in order of their nums; edges are visited in the
 * order of the num of the src. Note that the graph is modified in place!
 *)
val dummy_ir : Ir.stmt
val preprocess : Cfg.IrCfg.t -> unit

(**
 * Anticipated Expressions (a.k.a. Very Busy Expressions)
 * =======================
 * Domain            : Sets of expressions
 * Direction         : Backwards
 * Transfer function : in(n) = use(n) + (out(n) - kill(n))
 * Boundary          : in[exit] = 0
 * Meet (/\)         : intersection
 * Initialization    : in[n] = set of all exprs
 *)
module BusyExprCFG : ExprSetIntersectCFG

(**
 * Available Expressions
 * =====================
 * Domain            : Sets of expressions
 * Direction         : Forwards
 * Transfer function : out(n) = (anticipated[n].in + in[n]) - kill(n)
 * Boundary          : out[start] = 0
 * Meet (/\)         : intersection
 * Initialization    : out[n] = set of all exprs
 *)
module AvailExprCFG : ExprSetIntersectCFG

(**
 * Postponable Expressions
 * =======================
 * Domain            : Sets of expressions
 * Direction         : Forwards
 * Transfer function : out(n) = (earliest(n) + in[n]) - use(n)
 * Boundary          : out[start] = 0
 * Meet (/\)         : intersection
 * Initialization    : out[n] = set of all exprs
 *)
module PostponeExprCFG : ExprSetIntersectCFG

(**
 * Used Expressions
 * ================
 * Domain            : Sets of expressions
 * Direction         : Backwards
 * Transfer function : in(n) = (use(n) + out[n]) - latest(n)
 * Boundary          : in[exit] = 0
 * Meet (/\)         : union
 * Initialization    : in[n] = empty set
 *)
module UsedExprCFG : ExprSetIntersectCFG
