open Core.Std
open Graph
open Cfg
open Dataflow
open Asm
open Fresh
open Util

(**** Invariant Checks ****)
let degree_ok _regctx = failwith "TODO"

let simplify_ok _regctx = failwith "TODO"

let freeze_ok _regctx = failwith "TODO"

let spill_ok _regctx = failwith "TODO"

(* remove x from lst, if it exists *)
let remove (lst : 'a list) (x : 'a) : 'a list =
  let f y = x <> y in
  List.filter ~f lst

(* conses x onto list unless x is already in list *)
let unduped_cons (lst : 'a list) (x : 'a) : 'a list =
  if List.mem lst x then lst
  else x :: lst

let empty (l: 'a list) = List.length l = 0

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
      "shrq"; "sarq";
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

  let binops_leaq =
    let instr = ["leaq"] in
    let f op1 op2 =
      let set1 = set_of_arg op1 in
      let set2 = set_of_arg op2 in
      match op2 with
      | Reg _ -> (set1, set2)
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
      binops_leaq;
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

  (* handle reg aliases *)
  let reg_alias (reg : abstract_reg) =
    match reg with
    | Real Cl -> Real Rcx
    | _ -> reg

  let transfer (_: extra_info) (e: AsmCfg.E.t) (d: Lattice.data) =
    (* We use dest because live variable analysis is backwards *)
    match E.dst e with
    | Start | Exit -> AbstractRegSet.empty
    | Node n_data ->
        let use_n, def_n =
          let f = AbstractRegSet.map ~f:reg_alias in
          let l, r = usedvars n_data.asm in
          f l, f r in
        AbstractRegSet.union use_n (AbstractRegSet.diff d def_n)

end

module LiveVariableAnalysis = GenericAnalysis (AsmWithLiveVar)

module ARegKey = struct
  type t = abstract_reg [@@deriving compare,sexp]
end

module ARegPairKey = struct
  type t = (abstract_reg * abstract_reg) [@@deriving compare,sexp]
end

module AReg = struct
  module Map = Map.Make (ARegKey)
  module Set = Set.Make (ARegKey)
end

module ARegPair = struct
  module Set = Set.Make (ARegPairKey)
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

let reg_of_color (c : color) : reg =
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

let color_of_reg (r : reg) : color =
  match r with
  | Rax -> Reg1
  | Rbx -> Reg2
  | Rcx -> Reg3
  | Cl -> Reg3
  | Rdx -> Reg4
  | Rsi -> Reg5
  | Rdi -> Reg6
  | R8 -> Reg7
  | R9 -> Reg8
  | R10 -> Reg9
  | R11 -> Reg10
  | R12 -> Reg11
  | R13 -> Reg12
  | R14 -> Reg13
  | R15 -> Reg14
  | _ -> failwith "color_of_reg: no color for rbp/rsp"

let string_of_color (c : color) =
  c |> reg_of_color |> string_of_reg

let _string_of_colors (l : color list) =
  l |> List.map ~f:string_of_color |> List.fold_left ~f:( ^ ) ~init:""

let get_next_color (colors : color list) : color option =
  let colorlist = [ Reg1; Reg2; Reg3; Reg4; Reg5; Reg6;
    Reg7; Reg8; Reg9; Reg10; Reg11; Reg12; Reg13; Reg14;] in
  let f acc x =
    match acc with
    | Some _ -> acc
    | None -> if not (List.mem colors x) then Some x
        else None in
  List.fold_left ~f ~init:None colorlist

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

(* return node alias after coalescing; if node has not been coalesced,
 * reduces to identity function *)
let rec get_alias (node : abstract_reg) (regctx : alloc_context) : abstract_reg =
  if List.mem regctx.coalesced_nodes node then
    match AReg.Map.find regctx.alias node with
    | Some a -> get_alias a regctx
    | None -> node
  else
    node

let string_of_abstract_regs regs =
  sprintf "[%s]" (String.concat ~sep:"," (List.map regs ~f:string_of_abstract_reg))

let string_of_coalesced_nodes = string_of_abstract_regs

let string_of_reg_set reg_set =
  let regs = AReg.Set.to_list reg_set in
  sprintf "{%s}" (String.concat ~sep:"," (List.map regs ~f:string_of_abstract_reg))

let string_of_reg_map (f: 'a -> string) (m: 'a AReg.Map.t) =
  AReg.Map.to_alist m
  |> List.map ~f:(fun (areg, x) ->
      sprintf "  %s --> %s"
        (string_of_abstract_reg areg)
        (f x)
  )
  |> String.concat ~sep:",\n"
  |> fun s -> "{\n" ^ s ^ "\n}"

let string_of_adj_list adj_list =
  string_of_reg_map string_of_reg_set adj_list

let string_of_color_map color_map =
  string_of_reg_map string_of_color color_map

let valid_coloring ({adj_list; color_map; spilled_nodes; coalesced_nodes; _} as c) =
  AReg.Map.for_alli adj_list ~f:(fun ~key ~data ->
    if List.mem spilled_nodes (get_alias key c) then begin
      assert (not (AReg.Map.mem color_map key));
      true
    end else
      let get_color areg =
        match AReg.Map.find color_map areg with
        | Some c -> c
        | None -> begin
            let s = (string_of_abstract_reg areg) in
            printf "adj list = %s\n" (string_of_adj_list adj_list);
            printf "color map = %s\n" (string_of_color_map color_map);
            printf "coalesced nodes = %s\n" (string_of_coalesced_nodes coalesced_nodes);
            failwith (sprintf "register %s has no color" s)
        end
      in
      let my_color = get_color key in
      let neighbors = AReg.Set.to_list data in
      let neighbor_colors = List.map neighbors ~f:get_color in
      not (List.mem neighbor_colors my_color)
  )

let string_of_temp_move ({src; dest; move} : temp_move) =
  let srcstr = string_of_abstract_reg src in
  let deststr = string_of_abstract_reg dest in
  let movestr = (string_of_int move.num) ^ ": " ^ (string_of_abstract_asm move.asm) in
  "{ src = " ^ srcstr ^ "; dest = " ^ deststr ^ "; move = " ^ movestr ^ "}"

let _string_of_abstract_regs (regs : abstract_reg list) : string =
  let f acc reg =
    (string_of_abstract_reg reg) ^ ", " ^ acc in
  List.fold_left ~f ~init:"" regs

let areg_map_to_str
  (val_strf : 'a -> string)
  (map : 'a AReg.Map.t)
  (mapname : string) : string list =
  let f ~key ~data acc =
    ((string_of_abstract_reg key) ^ " -> " ^ (val_strf data) ^ ";") :: acc in
  let pre = "{{ " ^ mapname ^ ": " in
  let post = " }}" in
  let body = AReg.Map.fold ~f ~init:[post] map in
  pre :: body

let list_to_str (strf : 'a -> string) (lst : 'a list) (listname : string) : string =
  let f acc s = (strf s) ^ ", " ^ acc in
  "[[ " ^ listname ^ ": " ^ (List.fold_left ~f ~init:"" lst) ^ "]]"

let _string_of_ctx (regctx : alloc_context) : string list =

  let reglist_to_str (l: abstract_reg list) (name : string) : string =
    list_to_str string_of_abstract_reg l name in

  let movelist_to_str (l: temp_move list) (name : string) : string =
    list_to_str string_of_temp_move l name in

  let str_reglists (l : ((abstract_reg list) * string) list) : string list =
    let f acc (lst, name) = (reglist_to_str lst name) :: acc in
    List.fold_left ~f ~init:[] l in

  let str_movelists (l : ((temp_move list) * string) list) : string list =
    let f acc (lst, name) = (movelist_to_str lst name) :: acc in
    List.fold_left ~f ~init:[] l in

  let adjlist_to_str aregset =
    reglist_to_str (AReg.Set.to_list aregset) "neighbors" in

  ["BEGIN CONTEXT"] @
  str_reglists [
    (regctx.precolored, "precolored");
    (regctx.initial, "initial");
    (regctx.simplify_wl, "simplify_wl");
    (regctx.freeze_wl, "freeze_wl");
    (regctx.spill_wl, "spill_wl");
    (regctx.spilled_nodes, "spilled_nodes");
    (regctx.coalesced_nodes, "coalesced_nodes");
    (regctx.colored_nodes, "colored_nodes");
    (regctx.select_stack, "select_stack");
  ] @
  str_movelists [
    (regctx.coalesced_moves, "coalesced_moves");
    (regctx.constrained_moves, "constrained_moves");
    (regctx.frozen_moves, "frozen_moves");
    (regctx.worklist_moves, "worklist_moves");
    (regctx.active_moves, "active_moves");
  ] @
  areg_map_to_str string_of_color regctx.color_map "color_map" @
  areg_map_to_str adjlist_to_str regctx.adj_list "adj_list" @
  areg_map_to_str string_of_abstract_reg regctx.alias "alias" @
  ["END CONTEXT"]

let empty_ctx = {
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
  coalesced_spills   = [];

  (* move lists *)
  coalesced_moves    = [];
  constrained_moves  = [];
  frozen_moves       = [];
  worklist_moves     = [];
  active_moves       = [];

  (* interference graph / node related *)
  degree             = AReg.Map.empty;
  adj_list           = AReg.Map.empty;
  adj_set            = ARegPair.Set.empty;
  move_list          = AReg.Map.empty;
  alias              = AReg.Map.empty;
  color_map          = AReg.Map.empty;

  (* other data structures *)
  (* number of times a temp appears; used for spill heuristic *)
  node_occurrences   = AReg.Map.empty;
  (* number of available machine registers for allocation *)
  num_colors         = 14;
}

(* data structure helpers *)
let adj_list_add
  (key: abstract_reg)
  (dest : abstract_reg)
  (adj_list : AReg.Set.t AReg.Map.t) : AReg.Set.t AReg.Map.t =
    AReg.Map.update adj_list key
      ~f:(function Some s -> AReg.Set.add s dest | None -> AReg.Set.singleton dest)

(* assumes map starts from 0 on all keys *)
let diff_int_map
  (key : abstract_reg)
  (diff : int -> int)
  (map : int AReg.Map.t) : int AReg.Map.t =
  AReg.Map.update map key ~f:(function Some i -> diff i | None -> diff 0)

(* moves between two registers can be coalesced *)
let coalescable_move (stmt: AsmCfg.V.t) : temp_move option =
  match stmt with
  | Start | Exit -> None
  | Node { num; asm; } ->
    begin
    match asm with
    | Op (instr, ((Reg (_ as op1)) :: (Reg (_ as op2)) :: [])) ->
      begin
      if instr = "movq" || instr = "mov" then
          Some { src = op1; dest = op2; move = {num; asm;}}
      else
        None
      end
    | _ -> None
    end

(* possibly coalescable moves involving the node *)
let node_moves (node : abstract_reg) (regctx : alloc_context) : temp_move list =
  match AReg.Map.find regctx.move_list node with
  | Some nodemoves ->
      let f move =
        List.mem regctx.active_moves move ||
        List.mem regctx.worklist_moves move in
      List.filter ~f nodemoves
  | None -> []

(* is a interference graph node still move related? *)
let move_related (node: abstract_reg) (regctx : alloc_context) : bool =
  List.length (node_moves node regctx) > 0

let add_edge (u : abstract_reg) (v : abstract_reg) regctx : alloc_context =
  if (not (ARegPair.Set.mem regctx.adj_set (u, v)) && (u <> v)) then
    begin
    let adj_set' =
      ARegPair.Set.union regctx.adj_set (ARegPair.Set.of_list [(u,v); (v,u)]) in

    let adj_list', degree' =
      if not (List.mem regctx.precolored u) then
        adj_list_add u v regctx.adj_list, diff_int_map u succ regctx.degree
      else
        regctx.adj_list, regctx.degree in

    let adj_list'', degree'' =
      if not (List.mem regctx.precolored v) then
        adj_list_add v u adj_list', diff_int_map v succ degree'
      else
        adj_list', degree' in
    { regctx with
      adj_set = adj_set';
      adj_list = adj_list'';
      degree = degree''; }
    end
  else regctx

let cfgnode_sort (nodes : AsmCfg.V.t list) =
  let cmp (node1 : AsmCfg.V.t) (node2 : AsmCfg.V.t) : int =
    match node1, node2 with
    | Start, Start | Exit, Exit -> 0
    | Start, _ -> -1
    | Exit, _ -> 1
    | Node _ , Start -> 1
    | Node _ , Exit -> -1
    | Node n1, Node n2 -> compare n1.num n2.num in
  List.sort ~cmp nodes

(* build initializes data structures used by regalloc
 * this corresponds to Build() and MakeWorklist() in Appel *)
let build
  (initctx : alloc_context)
  (asms : abstract_asm list)
  : alloc_context * (AsmCfg.vertex -> LiveVariableAnalysis.CFGL.data) =

  let cfg = AsmCfg.create_cfg asms in

  (*print_endline (AsmCfg.to_dot cfg);*)

  let livevars_edge = LiveVariableAnalysis.worklist () cfg in

  let livevars (v : AsmCfg.vertex) : LiveVariableAnalysis.CFGL.data =
    match v with
    | Start | Exit -> AbstractRegSet.empty
    | Node _ as node ->
      begin
        match AsmCfg.succ_e cfg node with
        | [] -> AbstractRegSet.empty
        | edge :: _ -> livevars_edge edge
      end in

  (* populate precolored, initial work lists *)
  let init0 (regctx : alloc_context) (reg : abstract_reg) =
    match reg with
    | Fake _ -> { regctx with initial = reg :: regctx.initial }
    | Real _ -> { regctx with precolored = reg :: regctx.precolored } in

  (* make edges for nodes interfering with each other in one statement *)
  let create_inter_edges (temps : AbstractRegSet.t) (regctx: alloc_context) : alloc_context =
    let g1 ctxacc1 reg1 =
      let g2 ctxacc2 reg2 =
        if reg1 <> reg2 then add_edge reg1 reg2 ctxacc2
        else ctxacc2 in
      AbstractRegSet.fold ~f:g2 ~init:ctxacc1 temps in
    AbstractRegSet.fold ~f:g1 ~init:regctx temps in

  (* initialize regctx with move_list and worklist_moves *)
  let init1 (regctx : alloc_context) (cfg_node : AsmCfg.vertex) : alloc_context =
    match cfg_node with
    (* TODO: should we handle procedure entry/exit differently? *)
    | Start | Exit -> regctx
    | Node { asm; _; } ->
      begin
      (* create interference graph edges *)
      let liveset = livevars cfg_node in
      (* add interferences between defs and liveset *)
      let _, defs = UseDefs.usedvars asm in
      let regctx' = create_inter_edges (AbstractRegSet.union liveset defs) regctx in

      (* update node occurrences *)
      let node_occurrences' =
        let f acc livevar =
          diff_int_map livevar succ acc in
        AbstractRegSet.fold ~f ~init:regctx'.node_occurrences liveset in

      (* populate worklist_moves and move_list *)
      let worklist_moves', move_list' =
        match coalescable_move cfg_node with
        | None -> regctx'.worklist_moves, regctx'.move_list
        | Some tempmove ->
            begin
              let ml =
                AReg.Map.update regctx'.move_list tempmove.src
                ~f:(function Some l -> tempmove :: l | None -> [tempmove]) in
              let ml' =
                AReg.Map.update ml tempmove.dest
                ~f:(function Some l -> tempmove :: l | None -> [tempmove]) in
              (tempmove :: regctx.worklist_moves, ml')
            end in

      { regctx' with
        node_occurrences = node_occurrences';
        worklist_moves = worklist_moves';
        move_list = move_list'; }
      end in

  (* add all initial (i.e. non-precolored) temps into appropriate worklists *)
  let init2 (regctx : alloc_context) (reg : abstract_reg) : alloc_context =
    let regdeg = AReg.Map.find regctx.degree reg |>
      function Some i -> i | None -> 0 in
    match reg with
    | Fake _ ->
        if regdeg >= regctx.num_colors then
          { regctx with spill_wl = reg :: regctx.spill_wl }
        else if move_related reg regctx then
          { regctx with freeze_wl = reg :: regctx.freeze_wl }
        else
          { regctx with simplify_wl = reg :: regctx.simplify_wl }
    (* precolored registers should not appear in initial! *)
    | Real _ -> regctx in

  (* add colors of precolored regs in color map *)
  let color_precoloreds (precoloreds : AbstractRegSet.t) regctx =
    let f ctxacc key =
      match key with
      | Fake _ -> ctxacc
      | Real r ->
        let data = color_of_reg r in
        { ctxacc with
          color_map = AReg.Map.add ctxacc.color_map ~key ~data } in
    AbstractRegSet.fold precoloreds ~f ~init:regctx in

  (* all fake temps in the program *)
  let fakes_set =
    let f fake = Fake fake in
    List.map ~f (fakes_of_asms asms) |> AbstractRegSet.of_list in

  (* set of all precolored nodes. we use this to create a precolored
   * clique in the interference graph. *)
  let precolored_set =
    let f r = Real r in
    List.map ~f [
      Rax; Rbx; Rcx; Rdx; Rsi; Rdi; R8;
      R9; R10; R11; R12; R13; R14; R15;
    ] |> AbstractRegSet.of_list in

  let all_vars_set = AbstractRegSet.union fakes_set precolored_set in

  (* put all vars into either precoloreds or initial worklist *)
  AbstractRegSet.fold ~f:init0 ~init:initctx all_vars_set |>
  (* create interferences between all precolored nodes *)
  create_inter_edges precolored_set |>
  (* set colors of precolored nodes in color map *)
  color_precoloreds precolored_set |> fun regctx' ->
  (* populate move worklists *)
  let nodes = AsmCfg.VertexSet.to_list (AsmCfg.vertex_set cfg) in
  let sorted_nodes = cfgnode_sort nodes in
  List.fold_left ~f:init1 ~init:regctx' sorted_nodes |> fun regctx' ->
  (* populate node worklists *)
  let finctx =
    List.fold_left ~f:init2 ~init:{ regctx' with initial = []} regctx'.initial in
  (finctx, livevars)

(* Returns a list of nodes adjacent to n that are not selected or coalesced.
 * Does not update the context. *)
let adjacent (reg : abstract_reg) regctx : abstract_reg list =
  let used = regctx.select_stack @ regctx.coalesced_nodes in
  AReg.Map.find regctx.adj_list reg |>
    function Some s ->
      List.filter ~f:(fun m -> not (List.mem used m)) (AReg.Set.to_list s)
    | None -> []

let degree (reg : abstract_reg) regctx : int =
  AReg.Map.find regctx.degree reg |> function Some n -> n | None -> 0

let enable_moves (nodes : abstract_reg list) regctx : alloc_context =
  let f' regctx2 m =
    if List.mem regctx2.active_moves m then
      { regctx2 with
        active_moves = remove regctx2.active_moves m;
        worklist_moves = unduped_cons regctx2.worklist_moves m;
      }
    else
      regctx2
  in
  let f regctx1 n =
    List.fold_left ~init:regctx1 (node_moves n regctx1) ~f:f'
  in
  List.fold_left ~init:regctx nodes ~f

let decrement_degree (m : abstract_reg) regctx : alloc_context =
  let d' = (degree m regctx) - 1 in
  let regctx1 = { regctx with degree = diff_int_map m pred regctx.degree } in
  if d' = regctx1.num_colors - 1 then
    let regctx2 = enable_moves (unduped_cons (adjacent m regctx1) m) regctx1 in
    let regctx3 = { regctx2 with spill_wl = remove regctx2.spill_wl m } in
    if move_related m regctx3 then
      { regctx3 with freeze_wl = unduped_cons regctx3.freeze_wl m }
    else
      { regctx3 with simplify_wl = unduped_cons regctx3.simplify_wl m }
  else
    regctx1

(* Remove non-move-related nodes of low degree *)
let simplify regctx : alloc_context =
  (* Pick a non-move-related vertex that has <k degree *)
  match regctx.simplify_wl with
  | [] -> regctx
  | n :: t ->
    let regctx1 =
      {
        regctx with
        simplify_wl = t;
        select_stack = unduped_cons regctx.select_stack n;
      }
    in
    List.fold_left
      ~f:(fun regctx2 m -> decrement_degree m regctx2)
      ~init:regctx1
      (adjacent n regctx1)

(* potentially add a new node to simplify_wl; see Appel for details *)
let add_wl (node : abstract_reg) (regctx : alloc_context) : alloc_context =
  if (not (List.mem regctx.precolored node) &&
      not (move_related node regctx) &&
      degree node regctx < regctx.num_colors) then
      begin
        { regctx with
          freeze_wl = remove regctx.freeze_wl node;
          simplify_wl = unduped_cons regctx.simplify_wl node;
        }
      end
  else
    regctx

let ok (t : abstract_reg) (r : abstract_reg) regctx : bool =
  (degree t regctx) < regctx.num_colors ||
  List.mem regctx.precolored t ||
  ARegPair.Set.mem regctx.adj_set (t, r)

let conservative (nodes : abstract_reg list) regctx : bool =
  let k' = 0 in
  let result = List.fold_left ~init:k' nodes ~f:(fun acc n ->
    if (degree n regctx) >= (regctx.num_colors) then acc + 1 else acc)
  in
  result < (regctx.num_colors)

let combine u v regctx =
  let find map k = AReg.Map.find map k |> function Some l -> l | None -> [] in

  let regctx1 =
    if List.mem regctx.freeze_wl v then
      { regctx with freeze_wl = remove regctx.freeze_wl v }
    else
      { regctx with spill_wl = remove regctx.spill_wl v } in

  let coalesced_nodes' =
    unduped_cons regctx1.coalesced_nodes v in
  let alias' =
    AReg.Map.add regctx1.alias ~key:v ~data:u in
  let move_list' =
    let new_lst = (find regctx1.move_list u) @ (find regctx1.move_list v) in
    let new_lst' = List.dedup new_lst in
    AReg.Map.add regctx1.move_list ~key:u ~data:new_lst'
  in

  let regctx2 =
    { regctx1 with
      coalesced_nodes = coalesced_nodes';
      alias = alias';
      move_list = move_list';
    }
  in

  let regctx3 = enable_moves [v] regctx2 in

  let f ctxacc t =
    add_edge t u ctxacc |> decrement_degree t
  in
  let regctx4 = List.fold_left ~f ~init:regctx3 (adjacent v regctx3) in

  if (degree u regctx4) >= (regctx4.num_colors) && List.mem regctx4.freeze_wl u then
    { regctx4 with
      freeze_wl = remove regctx4.freeze_wl u;
      spill_wl = unduped_cons regctx4.spill_wl u
    }
  else
    regctx4

(* Coalesce move-related nodes *)
let coalesce (regctx : alloc_context) : alloc_context =
  match regctx.worklist_moves with
  | [] -> regctx
  | m::t ->
    let x = get_alias m.src regctx in
    let y = get_alias m.dest regctx in
    let u, v = if List.mem regctx.precolored y then (y, x) else (x, y) in
    let regctx1 = { regctx with worklist_moves = t } in
    if u = v then
      let regctx2 = { regctx1 with
                       coalesced_moves = unduped_cons regctx1.coalesced_moves m
                    }
      in
      add_wl u regctx2
    else if List.mem regctx1.precolored v ||
            ARegPair.Set.mem regctx1.adj_set (u, v) then
      let regctx2 = { regctx1 with
                       constrained_moves = unduped_cons regctx1.constrained_moves m
                     }
      in
      regctx2 |> add_wl u |> add_wl v
    else if
      (List.mem regctx1.precolored u &&
       List.fold_left ~init:true (adjacent v regctx1)
       ~f:(fun acc t -> acc && (ok t u regctx1)))
       ||
       (not (List.mem regctx1.precolored u) &&
       conservative ((adjacent u regctx1) @ (adjacent v regctx1)) regctx1) then
      let regctx2 = { regctx1 with
                       coalesced_moves = unduped_cons regctx1.coalesced_moves m }
      in
      regctx2 |> combine u v |> add_wl u
    else
      { regctx1 with active_moves = unduped_cons regctx1.active_moves m }

(* freeze: remove a move-related node of low degree *)
let freeze_moves (regctx : alloc_context) (node: abstract_reg) : alloc_context =
  let f ctxacc tempmove =
    let { src; dest; _; } = tempmove in
    let v =
      if get_alias dest ctxacc = get_alias node ctxacc then
        get_alias src ctxacc
      else
        get_alias dest ctxacc
    in
    let active_moves' = remove ctxacc.active_moves tempmove in
    let frozen_moves' = unduped_cons ctxacc.frozen_moves tempmove in
    let ctxacc' = {
      ctxacc with
      active_moves = active_moves';
      frozen_moves = frozen_moves';
    }
    in
    let freeze_wl', simplify_wl' =
      if (List.mem ctxacc'.freeze_wl v) && not (move_related v ctxacc') then
        remove ctxacc'.freeze_wl v, unduped_cons ctxacc'.simplify_wl v
      else
        ctxacc'.freeze_wl, ctxacc'.simplify_wl in
    { ctxacc' with
      freeze_wl = freeze_wl';
      simplify_wl = simplify_wl';
    }
    in
    List.fold_left ~f ~init:regctx (node_moves node regctx)

let freeze (regctx : alloc_context) : alloc_context =
  match regctx.freeze_wl with
  | [] -> regctx
  | fnode :: t ->
    let regctx' = {
      regctx with
      freeze_wl = t;
      simplify_wl = unduped_cons regctx.simplify_wl fnode;
    }
  in
  freeze_moves regctx' fnode

(* select a node to be spilled; heuristic used is
 * number of program points on which the temp is live on *)
let select_spill (regctx : alloc_context) : alloc_context =
  let f reg =
    let occur_num =
      AReg.Map.find regctx.node_occurrences reg |>
      function Some n -> n | None -> 0 in
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
      }
      in
      freeze_moves regctx' chosenreg

(* Pop nodes from the stack and assign a color *)
let assign_colors (regctx : alloc_context) : alloc_context =
  let select_assign ctxacc select_node =
    let neighbors =
      match AReg.Map.find ctxacc.adj_list select_node with
      | Some s -> AReg.Set.to_list s
      | None -> []
    in
    let neighbor_colors =
      let f acc neighbor =
        let alias = get_alias neighbor ctxacc in
        if (List.mem ctxacc.colored_nodes alias ||
            List.mem ctxacc.precolored alias) then
          begin
            AReg.Map.find ctxacc.color_map alias |>
            function Some c -> unduped_cons acc c | None -> acc
          end
        else acc
      in
      List.fold_left ~f ~init:[] neighbors in
    (*
    let () =
      match select_node with
      | Fake s when "__temp0" = s || "_asmreg14" = s ->
          begin
          print_endline ("have " ^ s);
          print_endline ("neighbors: " ^ (_string_of_abstract_regs neighbors));
          print_endline ("neighbor colors:" ^ (_string_of_colors neighbor_colors));
          end
      | _ -> () in
    *)
    begin
      match get_next_color neighbor_colors with
      | None ->
          { ctxacc with spilled_nodes = unduped_cons ctxacc.spilled_nodes select_node; }
      | Some c ->
          let color_map' =
             AReg.Map.add ctxacc.color_map ~key:select_node ~data:c in
          { ctxacc with
            color_map = color_map';
            colored_nodes = unduped_cons ctxacc.colored_nodes select_node; }
    end
    in

    let regctx' =
      (*print_endline "before fold";*)
      (*List.iter ~f:print_endline (_string_of_ctx regctx);*)
      let deduped = regctx.select_stack in
      List.fold_left
        ~f:select_assign
        ~init:{regctx with select_stack = []}
        deduped in

    let f ctxacc coalesced_node =
      let alias = get_alias coalesced_node regctx' in
      match AReg.Map.find regctx'.color_map alias with
      (* Note: if a coalesced node's alias has been spilled, the color map
       * has no binding for the alias. In this case, we do not assign a color
       * to the coalesced node. Before translating colors to actual registers,
       * we take uncolored coalesced nodes and assign them to the same memory
       * location as their spilled alias. *)
      | None -> {
          ctxacc with
          coalesced_spills = unduped_cons ctxacc.coalesced_spills coalesced_node
        }
      | Some c -> {
          ctxacc with
          color_map = AReg.Map.add ctxacc.color_map ~key:coalesced_node ~data:c
      } in
    let regctx' =
      List.fold_left ~f ~init:{ regctx' with coalesced_nodes = [] } regctx'.coalesced_nodes in
    (*List.iter ~f:print_endline (_string_of_ctx regctx');*)
    printf "assign_colors valid coloring? %b\n" (valid_coloring regctx');
    regctx'

type trans_context =
  | None
  (* set instructions always use cl instead of rcx *)
  | Set

let get_real_reg
  (tctx : trans_context)
  (regctx : alloc_context)
  (reg : abstract_reg) : abstract_reg =
  match AReg.Map.find regctx.color_map reg, tctx with
    | Some color, Set ->
        let r = reg_of_color color in
        let r' = if r = Rcx then Cl else r in
        Real r'
    | Some color, None ->
        Real (reg_of_color color)
    | None, _ ->
        (* see note above in assign color regarding coalesced_spills *)
        if List.mem regctx.coalesced_spills reg then
          get_alias reg regctx
        else reg

let translate_operand
  (tctx : trans_context)
  (regctx : alloc_context)
  (op : abstract_reg operand) : abstract_reg operand =
  match op with
  | Reg reg -> Reg (get_real_reg tctx regctx reg)
  | Mem (Base (c, reg)) -> Mem (Base (c, get_real_reg tctx regctx reg))
  | Mem (Off (c, reg, s)) -> Mem (Off (c, get_real_reg tctx regctx reg, s))
  | Mem (BaseOff (c, reg1, reg2, s)) ->
      Mem (BaseOff (c, get_real_reg tctx regctx reg1, get_real_reg tctx regctx reg2, s))
  | Label l -> Label l
  | Const c -> Const c

let get_trans_context (instr : string) : trans_context =
  let set_instr = [
    "sete"; "setne"; "setl"; "setg"; "setle"; "setge"; "setz";
    "setnz"; "sets"; "setns"; "setc"; "setnc";
  ] in
  if List.mem set_instr instr then Set
  else None

let translate_asm (regctx : alloc_context) (asm : abstract_asm) : abstract_asm =
  match asm with
  | Op (instr, operands) ->
      let ctx = get_trans_context instr in
      Op (instr, List.map ~f:(translate_operand ctx regctx) operands)
  | Lab l -> Lab l
  | Directive (s, l) -> Directive (s, l)
  | Comment s -> Comment s

(* allocate spilled nodes to the stack *)
let spill_allocate ?(debug=false) asms =
  (* spill_env maps each fake name to an index, starting at 18, into the stack.
   * We start at 18 since the first 14 is reserved for callee save registers
   * plus 3 for spill space for shuttle registers.
   * For example, if the fake name "foo" is mapped to n in spill_env, then Reg
   * (Fake "foo") will be spilled to -8n(%rbp). *)
  let spill_env =
    fakes_of_asms asms
    |> List.mapi ~f:(fun i asm -> (asm, i + 18))
    |> String.Map.of_alist_exn
  in

  (* Given an environment and name, return the memory address that fake is
   * spilled into. For example, `spill_address {"foo": 4}` "foo" = -32(%rbp)` *)
  let spill_address (spill_env: int String.Map.t) (fake: string) : reg operand =
    let i = String.Map.find_exn spill_env fake in
    let offset = Int64.of_int (-8 * i) in
    Mem (Base (Some offset, Rbp))
  in

  (* Recursively applies f to all the abstract_registers in asm. *)
  let abstract_reg_map (f: abstract_reg -> reg)  (asm: abstract_asm) =
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

  (* Certain real registers are output into abstract assembly. For example,
   * multiplication and division use rax and rdx. These registers shouldn't be
   * present in abstract assembly. *)
  let unused_regs = [R13; R14; R15] in

  let shuttle_address (r : reg) =
    let offset_index =
      match r with
      | R13 -> 15
      | R14 -> 16
      | R15 -> 17
      | _ -> failwith "spill_allocate: impossible" in
    let offset = Int64.of_int (-8 * offset_index) in
    Mem (Base (Some offset, Rbp)) in

  (* Translate fake registers using the register environment and leave real
   * registers alone. *)
  let translate_reg (reg_env: reg String.Map.t) (r: abstract_reg) : reg =
    match r with
    | Fake s -> String.Map.find_exn reg_env s
    | Real r -> r
  in

  let allocate (env: int String.Map.t) (asm: abstract_asm) : asm list =
    let spill = spill_address env in
    match asm with
    | Op (_, operands) ->
      begin
      let fakes = fakes_of_operands operands in
      let unused_regs = List.take unused_regs (List.length fakes) in
      let fake_to_real = List.zip_exn fakes unused_regs in
      let reg_env = String.Map.of_alist_exn fake_to_real in
      let fake_to_op f = Reg (String.Map.find_exn reg_env f) in

      let prepre =
        let f fake =
          let real = String.Map.find_exn reg_env fake in
          movq (fake_to_op fake) (shuttle_address real) in
        List.map ~f fakes in
      let pre = List.map fakes ~f:(fun fake -> movq (spill fake) (fake_to_op fake)) in
      let translation = [abstract_reg_map (translate_reg reg_env) asm] in
      let post = List.map fakes ~f:(fun fake -> movq (fake_to_op fake) (spill fake)) in
      let postpost =
        let f fake =
          let real = String.Map.find_exn reg_env fake in
          movq (shuttle_address real) (fake_to_op fake) in
        List.map ~f fakes in
      prepre @ pre @ translation @ post @ postpost
      end
    | Lab l -> [Lab l]
    | Directive (d, args) -> [Directive (d, args)]
    | Comment s -> [Comment s]
  in

  let allocated = List.concat_map ~f:(allocate spill_env) asms in
  if debug then
    let mapping = [] in
    let mapping = mapping @ [Comment "----- begin register mapping"] in
    let mapping = mapping @ (
      String.Map.to_alist spill_env
      |> List.sort ~cmp:(fun (_, i1) (_, i2) -> compare i1 i2)
      |> List.map ~f:(fun (s, i) -> Comment (sprintf "-%d(%%rbp): %s" (i * 8) s))
    ) in
    let mapping = mapping @ [Comment "----- end register mapping"] in
    mapping @ allocated
  else
    allocated


let reg_alloc ?(debug=false) (given_asms : abstract_asm list) : asm list =
  let main
    (regctx : alloc_context)
    (asms : abstract_asm list)
    : alloc_context * (AsmCfg.vertex -> LiveVariableAnalysis.CFGL.data) =

    ignore debug;

    let rec loop (innerctx : alloc_context) =
      if (empty innerctx.simplify_wl &&
          empty innerctx.worklist_moves &&
          empty innerctx.freeze_wl &&
          empty innerctx.spill_wl) then
         innerctx
      else
        let innerctx' =
          if not (empty innerctx.simplify_wl) then
            simplify innerctx
          else if not (empty innerctx.worklist_moves) then
            coalesce innerctx
          else if not (empty innerctx.freeze_wl) then
            freeze innerctx
          else
            select_spill innerctx in
        loop innerctx' in

      let buildctx, livevars = build regctx asms in

      print_endline "start building";
      print_endline "------------------------------------";
      List.iter ~f:print_endline (_string_of_ctx buildctx);
      print_endline "------------------------------------";
      print_endline "end building";

      buildctx |> loop |> assign_colors |> fun finctx ->
      (finctx, livevars) in

  let finctx, livevars = main empty_ctx given_asms in
  let finctx_comments : abstract_asm list =
    let f str = Comment str in
    List.map ~f (_string_of_ctx finctx) in

  (* remove coalesced moves *)
  let finasms =
    let numbered : AsmData.t list =
      List.mapi ~f:(fun num asm -> { AsmData.num; AsmData.asm; }) given_asms in
    let coalesced_index =
      let f { move; _ } = move.num in
      List.map ~f finctx.coalesced_moves in
    let f node acc =
      let livevar_str : abstract_asm =
        let comment_str = livevars (Node node) |> _set_to_string in
        Comment ("<< livevars: " ^ comment_str ^ ">>") in
      let asm_str = Comment (string_of_abstract_asm node.asm) in
      if List.mem coalesced_index node.num then acc
      else node.asm :: asm_str :: livevar_str :: acc in
    List.fold_right ~f ~init:[] numbered in

  (* translate abstract_asms with allocated nodes, leaving spills.
   * stack allocate spill nodes with Tiling.register_allocate *)
  List.map ~f:(translate_asm finctx) finasms |> fun finasms' ->
    finctx_comments @ finasms' |> spill_allocate
