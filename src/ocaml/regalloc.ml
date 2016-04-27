open Core.Std
open Graph
open Cfg
open Dataflow
open Asm
open Fresh

(* remove x from lst, if it exists *)
let remove (lst : 'a list) (x : 'a) : 'a list =
  let f y = x <> y in
  List.filter ~f lst

(* conses x onto list unless x is already in list *)
let unduped_cons (lst : 'a list) (x : 'a) : 'a list =
  if List.mem lst x then lst
  else x :: lst

let empty (l: 'a list) = List.length l = 0

let ( >>| ) (x : 'a) (debug : string) : 'a =
  print_endline debug;
  x

let ( >>= ) (x : 'a) (debug : 'a -> string) : 'a =
  print_endline (debug x);
  x

module AbstractRegSet : Set.S with type Elt.t = abstract_reg = Set.Make (
  struct
  type t = abstract_reg [@@deriving compare,sexp]
  end
)

module AdjacencySet : Set.S with type Elt.t = abstract_reg * abstract_reg =
  Set.Make (struct type t = (abstract_reg * abstract_reg) [@@deriving compare,sexp] end)

let _set_to_string (set : AbstractRegSet.t) : string =
  let f acc reg = (string_of_abstract_reg reg) ^ ", " ^ acc in
  "{ " ^ (AbstractRegSet.fold ~f ~init:"" set) ^ " }"

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

module UseDefs : UseDefsT = struct
  type usedefs = AbstractRegSet.t * AbstractRegSet.t

  type usedef_pattern =
    | Binop of string list * (abstract_reg operand -> abstract_reg operand -> usedefs)
    | Unop of string list * (abstract_reg operand -> usedefs)
    | Zeroop of string list * usedefs

  type usedef_val =
    | BinopV of string * abstract_reg operand * abstract_reg operand
    | UnopV of string * abstract_reg operand
    | ZeroopV of string

  (* we do not include Rbp and Rsp at all in our regalloc algo *)
  let no_rbp_or_rsp (regs : abstract_reg list) =
    remove regs (Real Rbp) |> fun regs' ->
    remove regs' (Real Rsp)

  (* grabs all temps that appear in an operand *)
  let set_of_arg (arg: abstract_reg operand) : AbstractRegSet.t =
    let regs_list = no_rbp_or_rsp (regs_of_operand arg) in
    AbstractRegSet.of_list regs_list

  let usedef_match (patterns : usedef_pattern list) (v : usedef_val) =
    let f acc (pat : usedef_pattern) =
      if acc <> None then acc else
      begin
        match pat, v with
        | Binop (names, f), BinopV (n, lhs, rhs) ->
            if List.mem names n then Some (f lhs rhs) else None
        | Unop (names, f), UnopV (n, op) ->
            if List.mem names n then Some (f op) else None
        | Zeroop (names, usedef), ZeroopV n ->
            if List.mem names n then Some usedef else None
        | _ -> None
      end in
    match List.fold_left ~f ~init:None patterns with
    | Some usedef -> usedef
    | None -> AbstractRegSet.empty, AbstractRegSet.empty

  let binops_use_plus_def =
    let instr = [
      "addq"; "subq"; "andq"; "orq"; "xorq"; "shlq";
      "shrq"; "sarq"; "leaq";
    ] in
    let f op1 op2 =
      let set1 = set_of_arg op1 in
      let set2 = set_of_arg op2 in
      match op2 with
      | Reg _ ->
        (AbstractRegSet.union set1 set2, set2)
      | Mem _ ->
        (AbstractRegSet.union set1 set2, AbstractRegSet.empty)
      | _ -> AbstractRegSet.empty, AbstractRegSet.empty in
    Binop (instr, f)

  let binops_move =
    let instr = ["movq"; "mov"] in
    let f op1 op2 =
      let set1 = set_of_arg op1 in
      let set2 = set_of_arg op2 in
      match op2 with
      | Reg _ -> (set1, set2)
      | Mem _ ->
        (AbstractRegSet.union set1 set2, AbstractRegSet.empty)
      | _ -> AbstractRegSet.empty, AbstractRegSet.empty in
    Binop (instr, f)

  let binops_use =
    let instr = [ "bt"; "cmpq"; "test" ] in
    let f op1 op2 =
      let uses = AbstractRegSet.union (set_of_arg op1) (set_of_arg op2) in
      (uses, AbstractRegSet.empty) in
    Binop (instr, f)

  let unops_use_plus_def =
    let instr = [ "incq"; "decq"; "negq"; ] in
    let f op =
      let opregs = set_of_arg op in
      match op with
      | Reg _ -> (opregs, opregs)
      | Mem _ -> (opregs, AbstractRegSet.empty)
      | _ -> (AbstractRegSet.empty, AbstractRegSet.empty) in
    Unop (instr, f)

  (* TODO: these should go in as a special case since we se CL for the
   * instructions right now.
   * Although for register allocation we probably want to do something smarter
   * and not default to CL but any other 8 bit register *)
  let unops_def =
    let instr = [
      "asete"; "asetne"; "asetl"; "asetg"; "asetle"; "asetge"; "asetz";
      "asetnz"; "asets"; "asetns"; "asetc"; "asetnc"; "pop"
    ] in
    let f op =
      let opregs = set_of_arg op in
      match op with
      | Reg _ -> (AbstractRegSet.empty, opregs)
      | Mem _ -> (opregs, AbstractRegSet.empty)
      | _ -> (AbstractRegSet.empty, AbstractRegSet.empty) in
    Unop (instr, f)

  let unops_use =
    let instr = [ "push" ; "pushq" ] in
    let f op = (set_of_arg op, AbstractRegSet.empty) in
    Unop (instr, f)

  let unops_mul_div =
    let instr = [ "imulq"; "idivq"; ] in
    let f op =
      let useset = AbstractRegSet.add (set_of_arg op) (Real Rax) in
      let defset = AbstractRegSet.of_list [Real Rax; Real Rdx;] in
      (useset, defset) in
    Unop (instr, f)

  let unops_call =
    let instr = [ "call"; ] in
    let f op =
      let useset = set_of_arg op in
      let defset =
        caller_saved_regs |>
        List.map ~f:(fun reg -> Real reg) |>
        no_rbp_or_rsp |>
        AbstractRegSet.of_list in
      (useset, defset) in
    Unop (instr, f)

  let zeroop_ret =
    let instr = ["retq"] in
    let useset =
      callee_saved_regs |>
      List.map ~f:(fun reg -> Real reg) |>
      no_rbp_or_rsp |>
      AbstractRegSet.of_list in
    Zeroop (instr, (useset, AbstractRegSet.empty))

  let asm_match =
    let patterns = [
      binops_use_plus_def;
      binops_move;
      binops_use;
      unops_use_plus_def;
      unops_def;
      unops_use;
      unops_mul_div;
      unops_call;
      zeroop_ret;
    ] in
    usedef_match patterns

  (* returns a sets of vars used and defd, respectively *)
  let usedvars : abstract_asm -> AbstractRegSet.t * AbstractRegSet.t =
    function
      | Op (name, []) ->
          asm_match (ZeroopV name)
      | Op (name, arg :: []) ->
          asm_match (UnopV (name, arg))
      | Op (name, arg1 :: arg2 :: []) ->
          asm_match (BinopV (name, arg1, arg2))
      | _ -> (AbstractRegSet.empty, AbstractRegSet.empty)

end


module LiveVariableLattice : LowerSemilattice with type data = AbstractRegSet.t with
  type data = AbstractRegSet.t = struct

  type data = AbstractRegSet.t
  let ( ** ) = AbstractRegSet.union
  let ( === ) = AbstractRegSet.equal
  let to_string data =
    let f acc reg =
      string_of_abstract_reg reg ^ ", " ^ acc in
    AbstractRegSet.fold ~f ~init:"" data
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

  open UseDefs

  let init (_: extra_info) (_: graph) (n: AsmCfg.V.t)  =
    match n with
    | Start | Exit -> AbstractRegSet.empty
    | Node n_data -> fst (usedvars n_data.asm)

  let transfer (_: extra_info) (e: AsmCfg.E.t) (d: Lattice.data) =
    (* TODO: We use dest (??) because live variable analysis is backwards *)
    match E.dst e with
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

  val string_of_nodedata : nodedata -> string
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

  let string_of_nodedata n =
    "( " ^ (string_of_abstract_reg n) ^ " )"
end

module IG = InterferenceGraph

module NodeDataKey = struct
  type t = IG.nodedata
  let compare = compare
  let hash = Hashtbl.hash
  let t_of_sexp _ = failwith "NodeData: implement t_of_sexp"
  let sexp_of_t _ = failwith "NodeData: implement sexp_of_t"
end

module NodeData = struct
  include NodeDataKey
  include Hashable.Make (NodeDataKey)
end

module AbstrRegKey = struct
  type t = abstract_reg [@@deriving compare,sexp]
end

module AbstractReg = struct
  include Map.Make (AbstrRegKey)
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
  (* interference graph related *)
  degree             : int NodeData.Table.t;
  adj_list           : IG.nodedata NodeData.Table.t;
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

let _string_of_ctx (regctx : alloc_context) : string =
  let strflat (listname : string) (sl : string list) =
    let f acc s = s ^ ", " ^ acc in
    "[[[ " ^ listname ^ ": " ^ (List.fold_left ~f ~init:"" sl) ^ "]]]\n\n" in

  "{" ^
  strflat "precolored" (List.map ~f:IG.string_of_nodedata regctx.precolored) ^
  strflat "initial" (List.map ~f:IG.string_of_nodedata regctx.initial) ^
  strflat "simplify_wl" (List.map ~f:IG.string_of_nodedata regctx.simplify_wl) ^
  strflat "freeze_wl" (List.map ~f:IG.string_of_nodedata regctx.freeze_wl) ^
  strflat "spill_wl" (List.map ~f:IG.string_of_nodedata regctx.spill_wl) ^
  strflat "spilled_nodes" (List.map ~f:IG.string_of_nodedata regctx.spilled_nodes) ^
  strflat "coalesced_node" (List.map ~f:IG.string_of_nodedata regctx.coalesced_nodes) ^
  strflat "colored_nodes" (List.map ~f:IG.string_of_nodedata regctx.colored_nodes) ^
  strflat "select_stack" (List.map ~f:IG.string_of_nodedata regctx.select_stack)

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
  (* interference graph related *)
  degree             = NodeData.Table.create () ~size:num_nodes;
  adj_list           = NodeData.Table.create () ~size:num_nodes;
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
        if not (List.mem colors x) then Some x
        else None in
  List.fold_left ~f ~init:None colorlist

let color_to_reg (c : color) : reg =
  match c with
  | Reg1 -> Rax
  | Reg2 -> Rbx
  | Reg3 -> Rcx
  | Reg4 -> Rdx
  | Reg5 -> Rsi
  | Reg6 -> Rdi
  | Reg7 -> R8
  | Reg8 -> R9
  | Reg9 -> R10
  | Reg10 -> R11
  | Reg11 -> R12
  | Reg12 -> R13
  | Reg13 -> R14
  | Reg14 -> R15

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
let node_moves (node : IG.nodedata) (regctx : alloc_context) : temp_move list =
  match NodeData.Table.find regctx.move_list node with
  | Some nodemoves ->
      let f move =
        List.mem regctx.active_moves move ||
        List.mem regctx.worklist_moves move in
      List.filter ~f nodemoves
  | None -> []

(* is a interference graph node still move related? *)
let move_related (node: IG.nodedata) (regctx : alloc_context) : bool =
  List.length (node_moves node regctx) > 0

(* build initializes data structures used by regalloc
 * this corresponds to Build() and MakeWorklist() in Appel *)
let build ?(init=false) (ctxarg : alloc_context) (asms : abstract_asm list) : alloc_context =
  let cfg = AsmCfg.create_cfg asms in
  let livevars_edge = LiveVariableAnalysis.worklist () cfg in

  let livevars : AsmCfg.vertex -> LiveVariableAnalysis.CFGL.data =
    function
      | Start | Exit -> AbstractRegSet.empty
      | Node _ as node ->
        begin
          match AsmCfg.succ_e cfg node with
          | [] -> AbstractRegSet.empty
          | edge :: _ -> livevars_edge edge
        end in

  let initctx =
    (* initialize initctx when first called from reg_alloc:
      * the algo assumes that on the first invocation, the context
      * will have initial and precolored nonempty *)
    if init then
      begin
      let f ctxacc reg =
        match reg with
        | Fake _ -> { ctxacc with initial = reg :: ctxacc.initial }
        | Real _ -> { ctxacc with precolored = reg :: ctxacc.precolored } in
      let vars =
        let get_vars cfgnode varset =
          AbstractRegSet.union (livevars cfgnode) varset in
        AsmCfg.fold_vertex get_vars cfg AbstractRegSet.empty in
      AbstractRegSet.fold ~f ~init:ctxarg vars
      end
    else
      (* TODO: check other things that need to be freshened on recursive calls *)
      { ctxarg with
        inter_graph = IG.create (); } in

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
  let init1 (cfg_node : AsmCfg.vertex) regctx =
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
              NodeData.Table.find_or_add
                ~default:(fun () -> [])
                regctx.move_list tempmove.src in
            let destmoves =
              NodeData.Table.find_or_add
                ~default:(fun () -> [])
                regctx.move_list tempmove.dest in
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

  (* initialize other data structures,
   * add all initial (i.e. non-precolored) temps into appropriate worklists *)
  let init2 (regctx : alloc_context) (reg : IG.nodedata) =
    (* do other initializations here, e.g. of degree table, etc. *)
    let regdeg = IG.in_degree regctx.inter_graph reg in
    NodeData.Table.set regctx.degree ~key:reg ~data:regdeg;
    match reg with
    | Fake _ ->
        (* assumption: our graph is actually directed and outdeg = indeg!!! *)
        if IG.in_degree regctx.inter_graph reg >= regctx.num_colors then
          { regctx with spill_wl = reg :: regctx.spill_wl }
        else if move_related reg regctx then
          { regctx with freeze_wl = reg :: regctx.freeze_wl }
        else
          { regctx with simplify_wl = reg :: regctx.simplify_wl }
    (* precolored registers should not appear in initial! *)
    | Real _ -> regctx in

  AsmCfg.fold_vertex init1 cfg initctx |> fun regctx' ->
  let empty_init = { regctx' with initial = [] } in
  List.fold_left ~f:init2 ~init:empty_init regctx'.initial

(* TODO: is this function even necessary? IG.add_edge does not seem to check
 * for self-loop. *)
let add_edge u v regctx =
  (* TODO: can i compare nodes with <>? *)
  if (not (IG.mem_edge regctx.inter_graph u v)) && (u <> v) then
    (* TODO: should I copy the graph?
     * The graph is undirected right? But it seems like direction matters
     * in the Appel algorithm?! *)
    IG.add_edge regctx.inter_graph u v

(* Returns a list of nodes adjacent to n that are not selected or coalesced.
 * Does not update the context. *)
let adjacent n regctx =
  let used = regctx.select_stack @ regctx.coalesced_nodes in
  List.filter (IG.succ regctx.inter_graph n) ~f:(fun m -> not (List.mem used m))

let degree node regctx =
  NodeData.Table.find_exn regctx.degree node

let enable_moves nodes regctx =
  List.fold_left ~init:regctx nodes ~f:(fun regctx' n ->
    List.fold_left ~init:regctx' (node_moves n regctx') ~f:(fun regctx'' m ->
      if List.mem regctx''.active_moves m then
        { regctx'' with
          active_moves = remove regctx''.active_moves m;
          worklist_moves = m::regctx''.worklist_moves; }
      else
        regctx''))

let decrement_degree m regctx =
  let d = degree m regctx in
  print_endline ("whee" ^ (string_of_int d));
  NodeData.Table.set regctx.degree ~key:m ~data:(d-1);
  if d = regctx.num_colors then
    let regctx' = enable_moves (m::(adjacent m regctx)) regctx in
    let regctx' = { regctx' with spill_wl = remove regctx'.spill_wl m } in
    if move_related m regctx' then
      { regctx' with freeze_wl = m::regctx'.freeze_wl }
    else
      { regctx' with simplify_wl = m::regctx'.simplify_wl }
  else
    regctx

(* Remove non-move-related nodes of low degree *)
let simplify regctx =
  print_endline "I'm a dumbassssssssssssssssssss \n also i'm in simplify";
  (* Pick a non-move-related vertex that has <k degree *)
  match regctx.simplify_wl with
  | [] -> regctx
  | n :: t ->
    let regctx' = {
      regctx with
      simplify_wl = t;
      select_stack = n :: regctx.select_stack
    } in
    List.fold_left
      ~f:(fun regctx'' m -> decrement_degree m regctx'')
      ~init:regctx'
      (adjacent n regctx')

(* return node alias after coalescing; if node has not been coalesced,
 * reduces to identity function *)
let get_alias (node : IG.nodedata) (regctx : alloc_context) : IG.nodedata =
  match NodeData.Table.find regctx.alias node with
  | Some alias -> alias
  | None -> node

(* potentially add a new node to simplify_wl; see Appel for details *)
let add_wl (node : IG.nodedata) (regctx : alloc_context) : alloc_context =
  if (not (List.mem regctx.precolored node) &&
      not (move_related node regctx) &&
      IG.in_degree regctx.inter_graph node >= regctx.num_colors) then
      begin
        { regctx with
          freeze_wl = remove regctx.freeze_wl node;
          simplify_wl = unduped_cons regctx.simplify_wl node; }
      end
  else regctx

let ok t r regctx =
  (degree t regctx) < regctx.num_colors ||
  List.mem regctx.precolored t ||
  IG.mem_edge regctx.inter_graph t r

let conservative nodes regctx =
  let k' = 0 in
  let result = List.fold_left ~init:k' nodes ~f:(fun acc n ->
    if (degree n regctx) >= (regctx.num_colors) then acc + 1 else acc) in
  result < (regctx.num_colors)

let combine u v regctx =
  let set t k d = NodeData.Table.set t ~key:k ~data:d in
  let find_exn t k = NodeData.Table.find_exn t k in

  let regctx' =
    if List.mem regctx.freeze_wl v then
      { regctx with freeze_wl = remove regctx.freeze_wl v }
    else
      { regctx with spill_wl = remove regctx.spill_wl v }
  in
  let regctx' = { regctx' with coalesced_nodes = v::regctx'.coalesced_nodes } in
  set regctx'.alias v u;
  let move_list = (find_exn regctx'.move_list u) @ (find_exn regctx'.move_list v) in
  set regctx'.move_list u move_list;
  let regctx' = enable_moves [v] regctx' in
  let regctx' = List.fold_left ~init:regctx' (adjacent v regctx')
    ~f:(fun regctx'' t -> add_edge t u regctx''; decrement_degree t regctx'') in
  if (degree u regctx') >= (regctx.num_colors) && List.mem regctx'.freeze_wl u then
    { regctx' with
      freeze_wl = remove regctx'.freeze_wl u;
      spill_wl = u::regctx'.spill_wl }
  else
    regctx'

(* Coalesce move-related nodes *)
let coalesce (regctx : alloc_context) : alloc_context =
  match regctx.worklist_moves with
  | [] -> regctx
  | m::t ->
    let x = get_alias m.src regctx in
    let y = get_alias m.dest regctx in
    let u, v = if List.mem regctx.precolored y then (y, x) else (x, y) in
    let regctx' = { regctx with worklist_moves = t } in
    if u = v then
      let regctx'' = { regctx' with
                       coalesced_moves = m::regctx'.coalesced_moves } in
      add_wl u regctx''
    else if List.mem regctx'.precolored v ||
            IG.mem_edge regctx'.inter_graph u v then
      let regctx'' = { regctx' with
                       constrained_moves = m::regctx'.constrained_moves } in
      regctx'' |> add_wl u |> add_wl v
    else if List.mem regctx'.precolored u &&
            List.fold_left ~init:true (adjacent v regctx')
              ~f:(fun acc t -> acc && (ok t u regctx')) ||
            not (List.mem regctx'.precolored u) &&
            conservative ((adjacent u regctx') @ (adjacent v regctx')) regctx' then
      let regctx'' = { regctx' with
                       coalesced_moves = m::regctx'.coalesced_moves } in
      regctx'' |> combine u v |> add_wl u
    else
      { regctx' with active_moves = m::regctx'.active_moves }

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
      if (List.mem ctxacc.freeze_wl v) && not (move_related v ctxacc) then
        remove ctxacc.freeze_wl v, unduped_cons ctxacc.simplify_wl v
      else
        ctxacc.freeze_wl, ctxacc.simplify_wl in
    { ctxacc with
      active_moves = active_moves';
      frozen_moves = frozen_moves';
      freeze_wl = freeze_wl';
      simplify_wl = simplify_wl'; } in
  List.fold_left ~f ~init:regctx (node_moves node regctx)

let freeze (regctx : alloc_context) : alloc_context =
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
let select_spill (regctx : alloc_context) =
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
  print_endline "assigning colors!";

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
      List.fold_left ~f ~init:[] neighbors >>= fun l -> (string_of_int (List.length l)) in
    begin
    match get_next_color neighbor_colors with
    | None ->
        print_endline "no color found!";
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
    regctx' >>| "colored!" >>| (_string_of_ctx regctx')

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

let get_real_reg (regctx : alloc_context) (reg : abstract_reg) : reg =
  match reg with
  | Real r -> r
  | Fake _ -> NodeData.Table.find_exn regctx.color_map reg |> color_to_reg

let translate_operand (regctx : alloc_context) (op : abstract_reg operand) : reg operand =
  match op with
  | Reg reg -> Reg (get_real_reg regctx reg)
  | Mem (Base (c, reg)) -> Mem (Base (c, get_real_reg regctx reg))
  | Mem (Off (c, reg, s)) -> Mem (Off (c, get_real_reg regctx reg, s))
  | Mem (BaseOff (c, reg1, reg2, s)) ->
      Mem (BaseOff (c, get_real_reg regctx reg1, get_real_reg regctx reg2, s))
  | Label l -> Label l
  | Const c -> Const c

let translate_asm (regctx : alloc_context) (asm : abstract_asm) : asm =
  match asm with
  | Op (instr, operands) ->
      Op (instr, List.map ~f:(translate_operand regctx) operands)
  | Lab l -> Lab l
  | Directive (s, l) -> Directive (s, l)
  | Comment s -> Comment s

let reg_alloc ?(debug=false) (given_asms : abstract_asm list) =
  (* TODO use debug *)
  ignore debug;

  let rec main
    ?(init = false)
    (regctx : alloc_context)
    (asms : abstract_asm list)
    : alloc_context * abstract_asm list =

    let rec loop (innerctx : alloc_context) =
      print_endline (_string_of_ctx innerctx);
      if (empty innerctx.simplify_wl &&
          empty innerctx.worklist_moves &&
          empty innerctx.freeze_wl &&
          empty innerctx.spill_wl) then
         innerctx >>| "empty"
      else
        begin
        let innerctx' =
          if not (empty innerctx.simplify_wl) then
            (print_endline "calling simplify"; simplify innerctx)
          else if not (empty innerctx.worklist_moves) then
            (print_endline "coalescing"; coalesce innerctx)
          else if not (empty innerctx.freeze_wl) then
            (print_endline "freezing"; freeze innerctx)
          else
            (print_endline "spilling"; select_spill innerctx) in
        loop innerctx'
        end in
      build ~init regctx asms |> loop |> assign_colors |> fun regctx' ->
        if not (empty regctx'.spilled_nodes) then
          begin
            let newctx, newasms = rewrite_program regctx' asms in
            main newctx newasms
          end
        else regctx', asms in

  (* lol 100 is an empirically determined number *)
  let finctx, finasms = main ~init:true (empty_ctx 100 100) given_asms in
  (* translate asms with allocated nodes *)
  print_endline "reg alloc done!";
  List.map ~f:(translate_asm finctx) finasms
