open Core.Std
open Asm
open Cfg

(* ************************************************************************** *)
(* Maps and Sets                                                              *)
(* ************************************************************************** *)
module ARegKey : sig
  type t = abstract_reg [@@deriving sexp, compare]
end

module ARegPairKey : sig
  type t = abstract_reg * abstract_reg [@@deriving sexp, compare]
end

module AReg : sig
  module Set : module type of Set.Make (ARegKey)
  module Map : module type of Map.Make (ARegKey)
end

module ARegPair : sig
  module Set : module type of Set.Make (ARegPairKey)
  module Map : module type of Map.Make (ARegPairKey)
end

val string_of_areg          : ARegKey.t -> string
val string_of_areg_set      : AReg.Set.t -> string
val string_of_areg_map      : 'a AReg.Map.t -> f:('a -> string) -> string
val string_of_areg_pair     : ARegPairKey.t -> string
val string_of_areg_pair_set : ARegPair.Set.t -> string
val string_of_areg_pair_map : 'a ARegPair.Map.t -> f:('a -> string) -> string

(* ************************************************************************** *)
(* Live Variable Analysis                                                     *)
(* ************************************************************************** *)
module UseDefs : sig
  type usedefs = AReg.Set.t * AReg.Set.t

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

module LiveVariableLattice : Dataflow.LowerSemilattice with type data = AReg.Set.t

module AsmWithLiveVar : Dataflow.CFGWithLatticeT
  with module CFG = Cfg.AsmCfg
  and module Lattice = LiveVariableLattice
  and type extra_info = unit

module LiveVariableAnalysis : (module type of Dataflow.GenericAnalysis(AsmWithLiveVar))

(* ************************************************************************** *)
(* Register Allocation Types                                                  *)
(* ************************************************************************** *)
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

val reg_of_color    : color -> reg
val color_of_reg    : reg   -> color
val string_of_color : color -> string

(* get_next_color cs returns a color not in cs if possible, or None otherwise *)
val get_next_color : color list -> color option

type temp_move = {
  src:  abstract_reg;
  dest: abstract_reg;
  move: AsmData.t;
} [@@deriving sexp, compare]

module TempMoveSet : Set.S with type Elt.t = temp_move
val string_of_temp_move : temp_move -> string
val string_of_temp_move_set : TempMoveSet.t -> string

type alloc_context = {
  (* IG node lists *)
  precolored         : AReg.Set.t;
  initial            : AReg.Set.t;
  simplify_wl        : AReg.Set.t;
  freeze_wl          : AReg.Set.t;
  spill_wl           : AReg.Set.t;
  spilled_nodes      : AReg.Set.t;
  coalesced_nodes    : AReg.Set.t;
  colored_nodes      : AReg.Set.t;
  select_stack       : abstract_reg list;
  (* coalesced nodes whose aliases were spilled *)
  coalesced_spills   : AReg.Set.t;

  (* move lists *)
  coalesced_moves    : TempMoveSet.t;
  constrained_moves  : TempMoveSet.t;
  frozen_moves       : TempMoveSet.t;
  worklist_moves     : TempMoveSet.t;
  active_moves       : TempMoveSet.t;

  (* interference graph / node related *)
  degree             : int AReg.Map.t;
  adj_list           : AReg.Set.t AReg.Map.t;
  adj_set            : ARegPair.Set.t;
  move_list          : TempMoveSet.t AReg.Map.t;
  alias              : abstract_reg AReg.Map.t;
  color_map          : color AReg.Map.t;

  (* other data structures *)
  (* number of times a temp appears; used for spill heuristic *)
  node_occurrences   : int AReg.Map.t;
  (* number of available machine registers for allocation *)
  num_colors         : int;

  (* used to verify no nodes are being dropped. *)
  all_moves          : TempMoveSet.t;
  (* used to verify no moves are being dropped. *)
  all_nodes          : AReg.Set.t;
}

val empty_ctx : alloc_context

val string_of_precolored        : AReg.Set.t -> string
val string_of_initial           : AReg.Set.t -> string
val string_of_simplify_wl       : AReg.Set.t -> string
val string_of_freeze_wl         : AReg.Set.t -> string
val string_of_spill_wl          : AReg.Set.t -> string
val string_of_spilled_nodes     : AReg.Set.t -> string
val string_of_coalesced_nodes   : AReg.Set.t -> string
val string_of_colored_nodes     : AReg.Set.t -> string
val string_of_select_stack      : abstract_reg list -> string
val string_of_coalesced_spills  : AReg.Set.t -> string
val string_of_coalesced_moves   : TempMoveSet.t -> string
val string_of_constrained_moves : TempMoveSet.t -> string
val string_of_frozen_moves      : TempMoveSet.t -> string
val string_of_worklist_moves    : TempMoveSet.t -> string
val string_of_active_moves      : TempMoveSet.t -> string
val string_of_degree            : int AReg.Map.t -> string
val string_of_adj_list          : AReg.Set.t AReg.Map.t -> string
val string_of_adj_set           : ARegPair.Set.t -> string
val string_of_move_list         : TempMoveSet.t AReg.Map.t -> string
val string_of_alias             : abstract_reg AReg.Map.t -> string
val string_of_color_map         : color AReg.Map.t -> string
val string_of_node_occurrences  : int AReg.Map.t -> string
val string_of_num_colors        : int -> string
val string_of_alloc_context     : alloc_context -> string

(* ************************************************************************** *)
(* Helpers                                                                    *)
(* ************************************************************************** *)

(* ************************************************************************** *)
(* Invariants                                                                 *)
(* ************************************************************************** *)
val disjoint_list_ok : alloc_context -> bool
val disjoint_set_ok : alloc_context -> bool
val all_nodes_ok : alloc_context -> bool
val all_moves_ok : alloc_context -> bool
val select_stack_no_dups_ok : alloc_context -> bool
val degree_ok : alloc_context -> bool
val simplify_ok : alloc_context -> bool
val freeze_ok : alloc_context -> bool
val spill_ok : alloc_context -> bool

(* valid_coloring c returns true if c.color_map is a valid coloring of
 * c.adj_list. That is, every node in adj_list has a different color that all
 * its neighbors. *)
val valid_coloring : alloc_context -> bool

(* ************************************************************************** *)
(* Register Allocation                                                        *)
(* ************************************************************************** *)
(* build stage of register allocation *)
val build :
  alloc_context ->
  abstract_asm list ->
  alloc_context * (AsmCfg.vertex -> LiveVariableAnalysis.CFGL.data)

(* performs register allocation with move coalescing *)
val reg_alloc : ?debug:bool -> abstract_asm list -> asm list
