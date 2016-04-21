open Core.Std
open Graph

module type ControlFlowGraph = Graph.Sig.I

(**
 * The type of vertex labels in a CFG. For example,
 *    - an AST CFG would have AST statements as labels,
 *    - an IR CFG would have IR statements as labels, and
 *    - an assembly CFG would have assembly instructions as labels.
 *)
module type NodeData = sig
  type t
end

(** The type of edge labels. *)
module EdgeData : sig
  type t =
    | Normal (* non-branching edge *)
    | True   (* true branch *)
    | False  (* false branch *)
  [@@deriving sexp, compare]
  val default : t
end

(* Make(N) is a graph with vertexes labeled with N.t and edges labled with
 * EdgeData.t This graph is the CFG. *)
module Make(N: NodeData) :
  (module type of Imperative.Graph.AbstractLabeled(N)(EdgeData))

(* IR CFG *)
module IrData : sig
  type t = {
    num: int;
    ir:  Ir.stmt;
  }
end
module IrCfg : sig
  include (module type of Make(IrData))
  val create_cfg : Ir.stmt list -> t
end

(* Abstract Assembly CFG *)
module AsmData : sig
  type t = {
    num: int;
    asm: Asm.abstract_asm;
  }
end
module AsmCfg : sig
  include (module type of Make(AsmData))
  val create_cfg : Asm.abstract_asm list -> t
end
