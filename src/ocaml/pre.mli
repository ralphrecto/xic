open Core.Std

(** Types and Helper Functions *)
module ExprSet : sig
  include Set.S with type Elt.t = Ir.expr
  val concat_map : 'a list -> f:('a -> t) -> t
  val to_string : t -> string
  val to_small_string : t -> string
end

module ExprMap : Map.S with type Key.t = Ir.expr

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
val get_subexpr_stmt_v : Cfg.IrDataStartExit.t -> ExprSet.t

val get_mem_temp     : Ir.expr -> ExprSet.t

val kill_func_args : Ir.expr list -> ExprSet.t
val kill_expr      : Ir.expr -> ExprSet.t
val kill_stmt      : Ir.stmt -> ExprSet.t
val kill_stmt_v : Cfg.IrDataStartExit.t -> ExprSet.t

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
module BusyExprCFG : sig
  type extra_info = {
    (* the graph *)
    g : Cfg.IrCfg.t;
    (* all of the used expressions in the graph *)
    univ : ExprSetIntersectLattice.data;
    (* uses[B] *)
    uses : Cfg.IrCfg.vertex -> ExprSetIntersectLattice.data;
    (* kill[B] *)
    kills : Cfg.IrCfg.vertex -> ExprSetIntersectLattice.data;
  }
  include ExprSetIntersectCFG with type extra_info := extra_info
end
module BusyExpr : (module type of Dataflow.GenericAnalysis(BusyExprCFG))

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
module AvailExprCFG : sig
  type extra_info = {
    (* the graph *)
    g : Cfg.IrCfg.t;
    (* all of the used expressions in the graph *)
    univ : ExprSetIntersectLattice.data;
    (* anticipated[B].in *)
    busy : Cfg.IrCfg.vertex -> ExprSetIntersectLattice.data;
    (* kill[B] *)
    kills : Cfg.IrCfg.vertex -> ExprSetIntersectLattice.data;
  }
  include ExprSetIntersectCFG with type extra_info := extra_info
end
module AvailExpr : (module type of Dataflow.GenericAnalysis(AvailExprCFG))

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
module PostponeExprCFG : sig
  type extra_info = {
    (* the graph *)
    g : Cfg.IrCfg.t;
    (* all of the used expressions in the graph *)
    univ : ExprSetIntersectLattice.data;
    (* e_use_{B} *)
    uses : Cfg.IrCfg.vertex -> ExprSetIntersectLattice.data;
    (* earlieset[B] *)
    earliest : Cfg.IrCfg.vertex -> ExprSetIntersectLattice.data;
  }
  include ExprSetIntersectCFG with type extra_info := extra_info
end
module PostponeExpr : (module type of Dataflow.GenericAnalysis(PostponeExprCFG))

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
module UsedExprCFG : sig
  type extra_info = {
    (* e_use_{B} *)
    uses : Cfg.IrCfg.vertex -> ExprSetUnionLattice.data;
    (* latest[B] *)
    latest : Cfg.IrCfg.vertex -> ExprSetUnionLattice.data;
  }
  include ExprSetUnionCFG with type extra_info := extra_info
end
module UsedExpr : (module type of Dataflow.GenericAnalysis(UsedExprCFG))

(** The whole enchilada! *)
val subst : Ir.expr ->
            latest:ExprSet.t ->
            used:ExprSet.t ->
            freshes:(Ir.expr ExprMap.t) ->
            Ir.expr

val red_elim : Cfg.IrCfg.t ->
               univ:ExprSet.t ->
               latest:(Cfg.IrCfg.V.t -> ExprSet.t) ->
               used:(Cfg.IrCfg.V.t -> ExprSet.t) ->
               Cfg.IrCfg.t

val flatten : Cfg.IrCfg.t -> Ir.stmt list

val pre : Ir.stmt list -> Ir.stmt list
