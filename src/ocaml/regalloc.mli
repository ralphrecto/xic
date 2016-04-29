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

module ARegKey : Map.Key with
  type t = abstract_reg

module ARegPairKey : Set.Elt with
  type t = abstract_reg * abstract_reg

module AReg : sig
  module Map : module type of Map.Make (ARegKey)
  module Set : module type of Set.Make (ARegKey)
end

module ARegPair : sig
  module Set : module type of Set.Make (ARegPairKey)
end

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
  src: abstract_reg;
  dest: abstract_reg;
  move: AsmData.t; (* { num; abstract_asm } *)
}

type alloc_context = {
  (* IG node lists *)
  precolored         : abstract_reg list;
  initial            : abstract_reg list;
  simplify_wl        : abstract_reg list;
  freeze_wl          : abstract_reg list;
  spill_wl           : abstract_reg list;
  spilled_nodes      : abstract_reg list;
  coalesced_nodes    : abstract_reg list;
  colored_nodes      : abstract_reg list;
  select_stack       : abstract_reg list;
  (* coalesced nodes whose aliases were spilled *)
  coalesced_spills   : abstract_reg list;

  (* move lists *)
  coalesced_moves    : temp_move list;
  constrained_moves  : temp_move list;
  frozen_moves       : temp_move list;
  worklist_moves     : temp_move list;
  active_moves       : temp_move list;

  (* interference graph / node related *)
  degree             : int AReg.Map.t;
  adj_list           : AReg.Set.t AReg.Map.t;
  adj_set            : ARegPair.Set.t;
  move_list          : (temp_move list) AReg.Map.t;
  alias              : abstract_reg AReg.Map.t;
  color_map          : color AReg.Map.t;

  (* other data structures *)
  (* number of times a temp appears; used for spill heuristic *)
  node_occurrences   : int AReg.Map.t;
  (* number of available machine registers for allocation *)
  num_colors         : int;
}


(* build stage of register allocation *)
val build : alloc_context -> abstract_asm list -> alloc_context

(* performs register allocation with move coalescing *)
val reg_alloc : ?debug:bool -> abstract_asm list -> asm list
