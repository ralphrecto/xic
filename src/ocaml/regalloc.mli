open Core.Std
open Asm
open Cfg

module AbstractRegSet : Set.S with type Elt.t = abstract_reg

module type UseDefsT = sig
  type usedefs = AbstractRegSet.t * AbstractRegSet.t

  type usedef_pattern =
    | Binop of string list * (abstract_reg operand -> abstract_reg operand -> usedefs)
    | Unop of string list * (abstract_reg operand -> usedefs)
    | Zeroop of string list * usedefs

  type usedef_val =
    | BinopV of string * abstract_reg operand * abstract_reg operand
    | UnopV of string * abstract_reg operand
    | ZeroopV of string

  val usedef_match : usedef_pattern list -> usedef_val -> usedefs

  val usedvars : abstract_asm -> usedefs
end

module UseDefs : UseDefsT

module LiveVariableLattice : Dataflow.LowerSemilattice with
  type data = AbstractRegSet.t

module AsmWithLiveVar : Dataflow.CFGWithLatticeT
  with module CFG = Cfg.AsmCfg
  and module Lattice = LiveVariableLattice
  and type extra_info = unit

module LiveVariableAnalysis : (module type of Dataflow.GenericAnalysis(AsmWithLiveVar))

module type InterferenceGraphT = sig
  type nodestate =
    | Precolored
    | Initial
    | Spilled

  type nodedata = abstract_reg

  include Graph.Sig.I
    with type V.t = nodedata
    and type V.label = nodedata
    and type E.t = nodedata * nodedata
    and type E.label = unit

  val string_of_nodedata : nodedata -> string
end

module InterferenceGraph : InterferenceGraphT

module IG = InterferenceGraph

module NodeDataKey : Hashtbl.Key with
  type t = IG.nodedata

module NodeData : (module type of (Hashable.Make (NodeDataKey)))

type color =
  | Reg1
  | Reg2
  | Reg3
  | Reg4
  | Reg5
  | Reg6
  | Reg7
  | Reg8
  | Reg9
  | Reg10
  | Reg11
  | Reg12
  | Reg13
  | Reg14

type temp_move = {
  src: IG.nodedata;
  dest: IG.nodedata;
  move: AsmData.t; (* { num; abstract_asm } *)
}

type alloc_context = {
  (* IG node lists *)
  precolored         : IG.nodedata list;
  initial            : IG.nodedata list;
  simplify_wl        : IG.nodedata list;
  freeze_wl          : IG.nodedata list;
  spill_wl           : IG.nodedata list;
  spilled_nodes      : IG.nodedata list;
  coalesced_nodes    : IG.nodedata list;
  colored_nodes      : IG.nodedata list;
  select_stack       : IG.nodedata list;
  (* move lists *)
  coalesced_moves    : temp_move list;
  constrained_moves  : temp_move list;
  frozen_moves       : temp_move list;
  worklist_moves     : temp_move list;
  active_moves       : temp_move list;
  (* other data structures *)
  move_list          : (temp_move list) NodeData.Table.t;
  alias              : IG.nodedata NodeData.Table.t;
  degree             : int NodeData.Table.t;
  nodestate          : IG.nodestate NodeData.Table.t;
  (* TODO: make color type, change int to color type *)
  color_map          : color NodeData.Table.t;
  (* number of times a temp appears; used for spill heuristic *)
  node_occurrences   : int NodeData.Table.t;
  inter_graph        : IG.t;
  (* number of available machine registers for allocation *)
  num_colors         : int;
}


(* build stage of register allocation *)
val build : ?init:bool -> alloc_context -> abstract_asm list -> alloc_context

(* performs register allocation with move coalescing *)
val reg_alloc : ?debug:bool -> abstract_asm list -> asm list
