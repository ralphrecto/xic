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
  nodestate          : IG.nodestate NodeData.Table.t;
  (* TODO: make color type, change int to color type *)
  color_map          : color NodeData.Table.t;
  (* number of times a temp appears; used for spill heuristic *)
  node_occurrences   : int NodeData.Table.t;
  inter_graph        : IG.t;
  (* number of available machine registers for allocation *)
  num_colors         : int;
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
  (* initialized to the node's self *)
  alias              = NodeData.Table.create () ~size:num_nodes;
  nodestate          = NodeData.Table.create () ~size:num_nodes;
  color_map          = NodeData.Table.create () ~size:num_nodes;
  node_occurrences   = NodeData.Table.create () ~size:num_nodes;
  (* the interference graph *)
  inter_graph        = IG.create ();
  num_colors         = 14;
}

module Cfg = AsmWithLiveVar.CFG

(* generic helper methods *)
let get_next_color (colors : color list) : color option =
  let colorlist = [
    Reg1;
    Reg2;
    Reg3;
    Reg4;
    Reg5;
    Reg6;
    Reg7;
    Reg8;
    Reg9;
    Reg10;
    Reg11;
    Reg12;
    Reg13;
    Reg14;
  ] in
  let f acc x =
    match acc with
    | Some _ -> acc
    | None ->
        if List.mem colors x then Some x
        else None in
  List.fold_left ~f ~init:None colorlist

(* remove x from lst, if it exists *)
let remove (lst : 'a list) (x : 'a) : 'a list =
  let f y = x <> y in
  List.filter ~f lst

(* conses x onto list unless x is already in list *)
let unduped_cons (lst : 'a list) (x : 'a) : 'a list =
  if List.mem lst x then lst
  else x :: lst

(* regalloc helper predicates *)
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

(* possibly coalescable moves involving the node *)
let node_moves (regctx : alloc_context) (node : IG.nodedata) : temp_move list =
  match NodeData.Table.find regctx.move_list node with
  | Some nodemoves ->
      let f move =
        List.mem regctx.active_moves move ||
        List.mem regctx.worklist_moves move in
      List.filter ~f nodemoves
  | None -> []

(* is a interference graph node still move related? *)
let move_related (regctx : alloc_context) (node: IG.nodedata) : bool =
  List.length (node_moves regctx node) > 0

(* build initializes data structures used by regalloc
 * this corresponds to Build() and MakeWorklist() in Appel *)
let build (asms : abstract_asm list) : alloc_context =
  let cfg = Cfg.create_cfg asms in

  let livevars : Cfg.vertex -> LiveVariableAnalysis.CFGL.data =
    let _livevars = LiveVariableAnalysis.worklist () cfg in
    fun node ->
      match Cfg.succ_e cfg node with
      | [] -> failwith "regalloc:build:livevars cfg node has no successors"
      | edge :: _ -> _livevars edge in

  (* make edges for nodes interfering with each other in one statement *)
  let create_inter_edges (regctx: alloc_context) (temps : AbstractRegSet.t) =
    let g1 regnode1 =
      let g2 regnode2 =
        (* don't add self loops in inter_graph *)
        if regnode1 <> regnode2 then
          IG.add_edge regctx.inter_graph regnode1 regnode2
        else () in
      AbstractRegSet.iter ~f:g2 temps in
    AbstractRegSet.iter ~f:g1 temps in

  (* initialize regctx with move_list and worklist_moves *)
  let init1 (cfg_node : Cfg.vertex) regctx =
    match cfg_node with
    (* TODO: should we handle procedure entry/exit differently? *)
    | Start -> regctx
    | Exit -> regctx
    | Node _ -> begin
        (* create interference graph edges *)
        let liveset = livevars cfg_node in
        create_inter_edges regctx liveset;
        (* update node occurrences *)
        let occurrence_update reg =
          let occur_num =
            NodeData.Table.find_or_add
            ~default:(fun () -> 0)
            regctx.node_occurrences reg in
          NodeData.Table.set
            regctx.node_occurrences
            ~key:reg
            ~data:(occur_num + 1) in
        AbstractRegSet.iter ~f:occurrence_update liveset;
        (* populate worklist_moves and move_list *)
        match coalescable_move cfg_node with
        | None -> regctx
        | Some tempmove ->
            let srcmoves =
              NodeData.Table.find_exn regctx.move_list tempmove.src in
            let destmoves =
              NodeData.Table.find_exn regctx.move_list tempmove.dest in
            NodeData.Table.add
              regctx.move_list
              ~key:tempmove.src
              ~data:(tempmove :: srcmoves) |> ignore;
            NodeData.Table.add
              regctx.move_list
              ~key:tempmove.dest
              ~data:(tempmove :: destmoves) |> ignore;
            { regctx with worklist_moves = (tempmove :: regctx.worklist_moves) }
      end in

  (* add all temps into either precolored or initial worklists *)
  let init2 (reg : IG.nodedata) regctx =
    match reg with
    | Fake _ ->
        (* assumption: our graph is actually directed and outdeg = indeg!!! *)
        if IG.in_degree regctx.inter_graph reg >= regctx.num_colors then
          { regctx with spill_wl = reg :: regctx.spill_wl }
        else if move_related regctx reg then
          { regctx with freeze_wl = reg :: regctx.freeze_wl }
        else
          { regctx with simplify_wl = reg :: regctx.simplify_wl }
    | Real _ ->
      { regctx with precolored = reg :: regctx.precolored } in

  AsmCfg.fold_vertex init1 cfg (empty_ctx 100 100) |> fun regctx ->
  IG.fold_vertex init2 regctx.inter_graph regctx

(* k is the number of registers available for coloring *)
let k = 14

(* Remove non-move-related nodes of low degree *)
let _simplify regctx =
  (* Pick a non-move-related vertex that has <k degree *)
  match regctx.simplify_wl with
  | [] -> regctx
  | n::t -> (*IG.remove_vertex regctx.inter_graph n;*)
      { regctx with simplify_wl = t; select_stack = n::regctx.select_stack }

(* return node alias after coalescing; if node has not been coalesced,
 * reduces to identity function *)
let get_alias (node : IG.nodedata) (regctx : alloc_context) : IG.nodedata =
  match NodeData.Table.find regctx.alias node with
  | Some alias -> alias
  | None -> node

(* potentially add a new node to simplify_wl; see Appel for details *)
let add_wl (node : IG.nodedata) (regctx : alloc_context) : alloc_context =
  if (not (List.mem regctx.precolored node) &&
      not (move_related regctx node) &&
      IG.in_degree regctx.inter_graph node >= regctx.num_colors) then
      begin
        { regctx with
          freeze_wl = remove regctx.freeze_wl node;
          simplify_wl = unduped_cons regctx.simplify_wl node; }
      end
  else regctx

let ok _t _r = failwith "TODO"
let combine _u _v _regctx = failwith "TODO"

(* Coalesce move-related nodes *)
let _coalesce (regctx : alloc_context) : alloc_context =
  match regctx.worklist_moves with
  | [] -> regctx
  | m::t ->
    let x = get_alias m.src regctx in
    let y = get_alias m.dest regctx in
    let u, v = if List.mem regctx.precolored y then (y, x) else (x, y) in
    let regctx' = { regctx with worklist_moves = t } in
    let _regctx' = { regctx with worklist_moves = t } in
    let new_regctx =
      if u = v then
        let regctx'' = { regctx' with
                          coalesced_moves = m::regctx'.coalesced_moves } in
        add_wl u regctx''
      else if List.mem regctx'.precolored v (*||
              IG.mem_edge regctx'.inter_graph u v*) then
        let regctx'' = { regctx' with
                          constrained_moves = m::regctx'.constrained_moves } in
        regctx'' |> add_wl u |> add_wl v
      else if List.mem regctx'.precolored u (*&&
              List.fold_left ~init:true (IG.succ regctx'.inter_graph v)
                ~f:(fun acc n -> acc && (ok n u))*) ||
              not (List.mem regctx'.precolored u) (*&&
              conservative ((IG.succ regctx'.inter_graph u) @
                (IG.succ regctx'.inter_graph v))*) then
        let regctx'' = { regctx' with
                          coalesced_moves = m::regctx'.coalesced_moves } in
        regctx'' |> combine u v |> add_wl u
      else
        { regctx' with active_moves = m::regctx'.active_moves }
    in
    new_regctx

(* freeze: remove a move-related node of low degree *)
let freeze_moves (regctx : alloc_context) (node: IG.nodedata) : alloc_context =
  let f ctxacc tempmove =
    let { src; dest; _; } = tempmove in
    let v =
      if get_alias dest ctxacc = get_alias node ctxacc then
        get_alias src ctxacc
      else
        get_alias dest ctxacc in
    let active_moves' = remove ctxacc.active_moves tempmove in
    let frozen_moves' = unduped_cons ctxacc.frozen_moves tempmove in
    let freeze_wl', simplify_wl' =
      if (List.mem ctxacc.freeze_wl v) && not (move_related ctxacc v) then
        remove ctxacc.freeze_wl v, unduped_cons ctxacc.simplify_wl v
      else
        ctxacc.freeze_wl, ctxacc.simplify_wl in
    { ctxacc with
      active_moves = active_moves';
      frozen_moves = frozen_moves';
      freeze_wl = freeze_wl';
      simplify_wl = simplify_wl'; } in
  List.fold_left ~f ~init:regctx (node_moves regctx node)

let _freeze (regctx : alloc_context) : alloc_context =
  match regctx.freeze_wl with
  | [] -> regctx
  | fnode :: _ ->
    let regctx' = {
      regctx with
      freeze_wl = remove regctx.freeze_wl fnode;
      simplify_wl = unduped_cons regctx.simplify_wl fnode;
    } in
    freeze_moves regctx' fnode

(* spill: spill a >=k degree node onto stack *)

(* select a node to be spilled; heuristic used is
 * number of program points on which the temp is live on *)
let _select_spill (regctx : alloc_context) =
  let f reg =
    let occur_num =
      NodeData.Table.find_or_add
      ~default:(fun () -> 0)
      regctx.node_occurrences reg in
    (reg, occur_num) in
  let cmp (_, num1) (_, num2) =
    Pervasives.compare num1 num2 in
  match List.map ~f regctx.spill_wl |> List.sort ~cmp with
  | [] -> regctx
  | (chosenreg, _) :: _ ->
      let regctx' = {
        regctx with
        spill_wl = remove regctx.spill_wl chosenreg;
        simplify_wl = unduped_cons regctx.simplify_wl chosenreg;
      } in
      freeze_moves regctx' chosenreg

(* Pop nodes from the stack and assign a color *)
let assign_colors (regctx : alloc_context) : alloc_context =

  let select_assign ctxacc select_node =
    let neighbors = IG.succ ctxacc.inter_graph select_node in
    let neighbor_colors =
      let f acc neighbor =
        let alias = get_alias neighbor ctxacc in
        if (List.mem ctxacc.colored_nodes alias ||
            List.mem ctxacc.precolored alias) then
          begin
            match NodeData.Table.find ctxacc.color_map alias with
            | Some c -> c :: acc
            | None -> acc
          end
        else acc in
      List.fold_left ~f ~init:[] neighbors in
    begin
    match get_next_color neighbor_colors with
    | None ->
        { ctxacc with spilled_nodes = select_node :: ctxacc.spilled_nodes; }
    | Some c ->
        NodeData.Table.set ctxacc.color_map ~key:select_node ~data:c;
        { ctxacc with colored_nodes = select_node :: ctxacc.colored_nodes; }
    end in

  List.fold_left ~f:select_assign ~init:regctx regctx.select_stack |> fun regctx' ->
    let f coalesced_node =
      match NodeData.Table.find regctx'.alias coalesced_node with
      | None -> failwith "assign_colors: coalesced node has no alias"
      | Some alias -> begin
        match NodeData.Table.find regctx'.color_map alias with
        | None -> failwith "assign_colors: coalesced node's alias is not colored"
        | Some c ->
            NodeData.Table.set regctx'.color_map ~key:coalesced_node ~data:c
      end in
    List.iter ~f regctx'.coalesced_nodes;
    regctx

let rewrite_program
  (regctx : alloc_context)
  (asms : abstract_asm list)
  : alloc_context * abstract_asm list =

  (* a lot of this logic is already in tiling.ml; see comments
   * there for design decisions e.g. why 15, etc. *)

  let spilled_names = 
    let f reg =
      match reg with
      | Real _ -> failwith "rewrite_program: Real register in spilled_nodes!"
      | Fake str -> str in
    List.map ~f regctx.spilled_nodes in
  
  (* a mapping between abstract reg -> offset index *)
  let spill_env : int String.Table.t = 
    let f i spill_name = (spill_name, i + 15) in
    spilled_names
    |> List.mapi ~f
    |> String.Table.of_alist_exn in

  (* returns memory address on stack of a given abstract reg *)
  let spill_address (spillname : string) : abstract_reg operand =
    let i = String.Table.find_exn spill_env spillname in
    let offset = Int64.of_int (-8 * i) in
    Mem (Base (Some offset, Real Rbp)) in

  (* Recursively applies f to all the abstract_registers in asm. *)
  let abstract_reg_map (f: abstract_reg -> abstract_reg) (asm: abstract_asm) =
    match asm with
    | Op (s, operands) ->
        Op (s, List.map operands ~f:(fun operand ->
          match operand with
          | Reg r -> Reg (f r)
          | Mem (Base (n, base)) -> Mem (Base (n, f base))
          | Mem (Off (n, off, scale)) -> Mem (Off (n, f off, scale))
          | Mem (BaseOff (n, base, off, scale)) -> Mem (BaseOff (n, f base, f off, scale))
          | Label l -> Label l
          | Const c -> Const c
        ))
    | Lab l -> Lab l
    | Directive (d, args) -> Directive (d, args)
    | Comment s -> Comment s
  in

  (* Translate fake registers using the register environment and leave real
   * registers alone. *)
  let translate_reg (reg_env: abstract_reg String.Map.t) (r: abstract_reg) : abstract_reg =
    match r with
    | Fake s -> String.Map.find_exn reg_env s
    | Real _ -> r
  in

  (* returns rewritten asms, list of new temps *)
  let allocate
    (asm: abstract_asm)
    : (abstract_asm list * abstract_reg list) =
    match asm with
    | Lab _ | Directive _ | Comment _ -> ([asm], [])
    | Op (_, operands) ->
      let spilled_fakes =
        let op_fakes = fakes_of_operands operands in
        let f fake = List.mem op_fakes fake in
        List.filter ~f spilled_names in
      let spilled_to_fresh =
        let f acc spilled = 
          (spilled, Fake (FreshReg.fresh ())) :: acc in
        List.fold_left ~f ~init:[] spilled_fakes in
      let spill_env = String.Map.of_alist_exn spilled_to_fresh in
      let spill_to_op spill = 
        Reg (String.Map.find_exn spill_env spill) in

      let pre =
        let f spill = movq (spill_address spill) (spill_to_op spill) in
        List.map ~f spilled_fakes in
      let translation =
        [abstract_reg_map (translate_reg spill_env) asm] in
      let post =
        let f spill = movq (spill_to_op spill) (spill_address spill) in
        List.map ~f spilled_fakes in
      (pre @ translation @ post, List.map ~f:snd spilled_to_fresh) in

  let rewritten, new_temps = List.unzip (List.map ~f:allocate asms) in
  let rewritten, new_temps = List.concat rewritten, List.concat new_temps in
  let regctx' = {
    regctx with
    spilled_nodes = [];
    initial = regctx.colored_nodes @ regctx.coalesced_nodes @ new_temps;
    colored_nodes = [];
    coalesced_nodes = [];
  } in
  (regctx', rewritten)

let reg_alloc _ =
  failwith "finish reg alloc!"
