open Core.Std
open Graph
open Cfg
open Dataflow
open Asm
open Tiling
open Fresh

module AbstractRegSet : Set.S with type Elt.t = abstract_reg = Set.Make (
  struct
  type t = abstract_reg

  let t_of_sexp _ = failwith "implement t_of_sexp for AbstractRegElt"
  let sexp_of_t _ = failwith "implement sexp_of_t for AbstractRegElt"
  (* we arbitrarily choose Fakes < Reals *)
  let compare a b =
    match a, b with
    | Fake _, Fake _ | Real _, Real _ -> 0
    | Fake _, Real _ -> -1
    | Real _, Fake _ -> 1
  end
)

module LiveVariableLattice : LowerSemilattice with type data = AbstractRegSet.t with
  type data = AbstractRegSet.t = struct

  type data = AbstractRegSet.t
  let ( ** ) = AbstractRegSet.union
  let ( === ) = AbstractRegSet.equal
  let to_string _ = failwith "TODO"
end

module AsmWithLiveVar : CFGWithLatticeT
  with module CFG = AsmCfg
  and module Lattice = LiveVariableLattice
  and type extra_info = unit = struct
  module Lattice = LiveVariableLattice
  module CFG = AsmCfg
  module ADSE = AsmDataStartExit
  open Lattice
  open CFG

  type graph = CFG.t
  type node = CFG.V.t
  type edge = CFG.E.t
  type data = Lattice.data

  type extra_info = unit

  let direction = `Backward

  (* returns a sets of vars used and defd, respectively *)
  let usedvars : abstract_asm -> AbstractRegSet.t * AbstractRegSet.t =
    let set_of_arg (arg: abstract_reg operand) : AbstractRegSet.t =
      AbstractRegSet.of_list (regs_of_operand arg) in
    let binops_use_plus_def =  [
      "addq";
      "subq";
      "andq";
      "orq";
      "xorq";
      "shlq";
      "shrq";
      "sarq";
      "leaq";
      "movq"
    ] in
    let binops_use = [
      "bt";
      "cmpq";
      "test"
    ] in
    let unops_use_plus_def = [
      "incq";
      "decq";
      "negq";
    ] in
    (* TODO: these should go in as a special case since we se CL for the
     * instructions right now.
     * Although for register allocation we probably want to do something smarter
     * and not default to CL but any other 8 bit register *)
    let unops_def = [
      "asete";
      "asetne";
      "asetl";
      "asetg";
      "asetle";
      "asetge";
      "asetz";
      "asetnz";
      "asets";
      "asetns";
      "asetc";
      "asetnc";
      "pop"
    ] in
    let unops_use = [
      "push" ;
      "pushq"
    ] in
    let unops_special = [
      "imulq";
      "idivq"
    ] in
    function
      | Op (name, arg :: []) ->
        let arg_set = set_of_arg arg in
        if List.mem unops_use_plus_def name then
          (arg_set, arg_set)
        else if List.mem unops_def name then
          (AbstractRegSet.empty, arg_set)
        else if List.mem unops_use name then
          (arg_set, AbstractRegSet.empty)
          (* TODO: HANDLE SPECIAL CASES!!! *)
        else if List.mem unops_special name then
          (AbstractRegSet.empty, AbstractRegSet.empty)
        else (AbstractRegSet.empty, AbstractRegSet.empty)
      | Op (name, arg1 :: arg2 :: []) ->
        let arg1_set = set_of_arg arg1 in
        let arg2_set = set_of_arg arg2 in
        let arg_union = AbstractRegSet.union arg1_set arg2_set in
        if List.mem binops_use_plus_def name then
          (arg_union, arg2_set)
        else if List.mem binops_use name then
          (arg_union, AbstractRegSet.empty)
        else (AbstractRegSet.empty, AbstractRegSet.empty)
      | _ -> (AbstractRegSet.empty, AbstractRegSet.empty)

  let init (_: extra_info) (_: graph) (n: AsmCfg.V.t)  =
    match n with
    | Start | Exit -> AbstractRegSet.empty
    | Node n_data -> fst (usedvars n_data.asm)

  let transfer (_: extra_info) (e: AsmCfg.E.t) (d: Lattice.data) =
    (* We use src because live variable analysis is backwards *)
    match E.src e with 
    | Start | Exit -> AbstractRegSet.empty
    | Node n_data ->
        let use_n, def_n = usedvars n_data.asm in
        AbstractRegSet.union use_n (AbstractRegSet.diff d def_n)
end

module LiveVariableAnalysis = GenericAnalysis (AsmWithLiveVar)

module type InterferenceGraphT = sig
  (* TODO: add the rest of node states *)
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
end

module InterferenceGraph : InterferenceGraphT = struct
  type nodestate =
    | Precolored
    | Initial
    | Spilled

  type nodedata = abstract_reg

  include Imperative.Graph.Concrete (struct
    type t = nodedata
    let compare = Pervasives.compare
    let hash    = Hashtbl.hash
    let equal   = (=)
  end)

  (* TODO: Remove or figure out how to integrate sets *)
  module Extended_T = struct
    include Tuple.Make (String) (String)
    include Tuple.Sexpable (String) (String)
    include Tuple.Comparable (String) (String)
  end
  module TS = Set.Make (Extended_T)

end

module IG = InterferenceGraph

module NodeData = struct
  module T = struct
    type t = IG.nodedata
    let compare = compare
    let hash = Hashtbl.hash
    let t_of_sexp _ = failwith "NodeData: implement t_of_sexp"
    let sexp_of_t _ = failwith "NodeData: implement sexp_of_t"
  end
  include T
  include Hashable.Make (T)
end

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
  move_list          : temp_move NodeData.Table.t;
  alias              : IG.nodedata NodeData.Table.t;
  nodestate          : IG.nodestate NodeData.Table.t;
  (* TODO: make color type, change int to color type *)
  color_map          : int NodeData.Table.t;
}

let empty_ctx num_moves num_nodes = {
  (* IG node lists *)
  precolored         = [];
  initial            = [];
  simplify_wl        = [];
  freeze_wl          = [];
  spill_wl           = [];
  spilled_nodes      = [];
  coalesced_nodes    = [];
  colored_nodes      = [];
  select_stack       = [];
  (* move lists *)
  coalesced_moves    = [];
  constrained_moves  = [];
  frozen_moves       = [];
  worklist_moves     = [];
  active_moves       = [];
  (* other data structures *)
  move_list          = NodeData.Table.create () ~size:num_moves;
  alias              = NodeData.Table.create () ~size:num_nodes;
  nodestate          = NodeData.Table.create () ~size:num_nodes;
  color_map          = NodeData.Table.create () ~size:num_nodes;
}

(* moves between two temps can be coalesced
 * TODO: is this a correct criterion? *)
let coalescable_move (stmt: AsmCfg.V.t) : temp_move option =  
  match stmt with
  | Start | Exit -> None
  | Node { num; asm; } -> begin
    match asm with
    | Op (instr, op1 :: op2 :: []) when instr = "movq"  ->
      begin
      match fakes_of_operand op1, fakes_of_operand op2 with
      | fake1 :: [], fake2 :: [] ->
          Some { src = Fake fake1; dest = Fake fake2; move = {num; asm;}}
      | _ -> None 
      end
    | _ -> None
  end

module Cfg = AsmWithLiveVar.CFG

let build (asms : abstract_asm list) : alloc_context * IG.t = 
  let cfg = Cfg.create_cfg asms in
  let inter_graph = IG.create () in

  let livevars : Cfg.vertex -> LiveVariableAnalysis.CFGL.data =
    let _livevars = LiveVariableAnalysis.worklist () cfg in
    fun node -> 
      match Cfg.succ_e cfg node with
      | [] -> failwith "regalloc:build:livevars cfg node has no successors"
      | edge :: _ -> _livevars edge in

  (* make edges for nodes interfering with each other in one statement *)
  let create_inter_edges (temps : AbstractRegSet.t) =
    let g1 regnode1 = 
      let g2 regnode2 =
        IG.add_edge inter_graph regnode1 regnode2 in
      AbstractRegSet.iter ~f:g2 temps in
    AbstractRegSet.iter ~f:g1 temps in

  let work (cfg_node : Cfg.vertex) regctx =
    match cfg_node with 
    (* TODO: should we handle procedure entry/exit differently? *)
    | Start -> regctx
    | Exit -> regctx
    | Node _ -> begin
        (* create interference graph edges *)
        create_inter_edges (livevars cfg_node);
        (* populate worklist_moves and move_list *)
        match coalescable_move cfg_node with
        | None -> regctx
        | Some tempmove ->
            Hashtbl.add regctx.move_list ~key:tempmove.src ~data:tempmove |> ignore;
            Hashtbl.add regctx.move_list ~key:tempmove.dest ~data:tempmove |> ignore;
            { regctx with worklist_moves = (tempmove :: regctx.worklist_moves) }
      end in

  (AsmCfg.fold_vertex work cfg (empty_ctx 100 100), inter_graph)

(* k is the number of registers available for coloring *)
let k = 14

(* Remove non-move-related nodes of low degree *)
let _simplify context =
  (* Pick a non-move-related vertex that has <k degree *)
  match context.simplify_wl with
  | [] -> context
  | n::t -> (*IG.remove_vertex context.inter_graph n;*)
      { context with simplify_wl = t; select_stack = n::context.select_stack }

let get_alias _n _context = failwith "TODO"
let add_wl _u _context = failwith "TODO"
let ok _t _r = failwith "TODO"
let combine _u _v _context = failwith "TODO"

(* Coalesce move-related nodes *)
let _coalesce context = 
  match context.worklist_moves with
  | [] -> context
  | m::t ->
    let x = get_alias m.src context in
    let y = get_alias m.dest context in
    let u, v = if List.mem context.precolored y then (y, x) else (x, y) in
    let context' = { context with worklist_moves = t } in
    let new_context =
      if u = v then
        let context'' = { context' with
                          coalesced_moves = m::context'.coalesced_moves } in
        add_wl u context''        
      else if List.mem context'.precolored v (*||
              IG.mem_edge context'.inter_graph u v*) then
        let context'' = { context' with
                          constrained_moves = m::context'.constrained_moves } in
        context'' |> add_wl u |> add_wl v
      else if List.mem context'.precolored u (*&&
              List.fold_left ~init:true (IG.succ context'.inter_graph v)
                ~f:(fun acc n -> acc && (ok n u))*) ||
              not (List.mem context'.precolored u) (*&&
              conservative ((IG.succ context'.inter_graph u) @
                (IG.succ context'.inter_graph v))*) then
        let context'' = { context' with
                          coalesced_moves = m::context'.coalesced_moves } in
        context'' |> combine u v |> add_wl u
      else
        { context' with active_moves = m::context'.active_moves }
    in
    new_context

(* Remove a move-related node of low degree *)
let _freeze _g _stack = failwith "TODO"

(* Spill a >=k degree node onto stack *)
let _spill _g _stack = failwith "TODO"

(* Pop nodes from the stack and assign a color *)
let _select _stack = failwith "TODO"

let reg_alloc _ =
  failwith "finish reg alloc!"
