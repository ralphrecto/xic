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

  (* pretty printed vertex value *)
  val to_string : t -> string

  (* (1) a = b <==> to_int a = to_int b
   * (2) for all a. to_int a >= 0 *)
  val to_int    : t -> int
end

(** A functor to make any type polymorphic comparable and hashable. *)
module Poly(N: NodeData) : sig
  include NodeData with type t = N.t
  val compare : t -> t -> int
  val hash    : t -> int
  val equal   : t -> t -> bool
end

(** A functor to generate explicit labels for start and exit nodes. *)
module StartExit(N: NodeData) : sig
  type t =
    | Node of N.t
    | Start
    | Exit
  include NodeData with type t := t
end

(** The type of edge labels. *)
module EdgeData : sig
  type t =
    | Normal (* non-branching edge *)
    | True   (* true branch *)
    | False  (* false branch *)
  [@@deriving compare]

  val default : t
  val to_string : t -> string
end

(* Make(N) is a graph with vertexes labeled with N.t and edges labled with
 * EdgeData.t This graph is the CFG. *)
module Make(N: NodeData) : sig
  include (module type of
    Imperative.Digraph.ConcreteBidirectionalLabeled(Poly(N))(EdgeData))
  val to_dot : Pervasives.out_channel -> t -> unit
end

(* IR CFG *)
module IrData : sig
  type t = {
    num: int;
    ir:  Ir.stmt;
  }
  include NodeData with type t := t
end
module IrDataStartExit : (module type of StartExit(IrData))
module IrCfg : sig
  include (module type of Make(IrDataStartExit))
  val create_cfg : Ir.stmt list -> t
end

(* Abstract Assembly CFG *)
module AsmData : sig
  type t = {
    num: int;
    asm: Asm.abstract_asm;
  }
  include NodeData with type t := t
end
module AsmDataStartExit : (module type of StartExit(AsmData))
module AsmCfg : sig
  include (module type of Make(AsmDataStartExit))
  val create_cfg : Asm.abstract_asm list -> t
end
