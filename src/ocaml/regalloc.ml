module U = Util
open Core.Std
open Graph
open Cfg
open Dataflow
open Asm
open Fresh

(* ************************************************************************** *)
(* Helpers                                                                    *)
(* ************************************************************************** *)
let printing_on = true

(* remove x from lst, if it exists *)
let remove (lst : 'a list) (x : 'a) : 'a list =
  let f y = x <> y in
  List.filter ~f lst

(* conses x onto list unless x is already in list *)
let unduped_cons (lst : 'a list) (x : 'a) : 'a list =
  if List.mem lst x then lst
  else x :: lst

(* ************************************************************************** *)
(* Maps and Sets                                                              *)
(* ************************************************************************** *)
module ARegKey = struct
  type t = abstract_reg [@@deriving compare,sexp]
end

module ARegPairKey = struct
  type t = (abstract_reg * abstract_reg) [@@deriving compare,sexp]
end

module AReg = struct
  module Set = Set.Make (ARegKey)
  module Map = struct
    include Map.Make (ARegKey)

    let find_exn (m: 'a t) (k: Key.t) =
      match find m k with
      | Some v -> v
      | None -> failwith (sprintf "reg %s not found" (string_of_abstract_reg k))
  end
end

module ARegPair = struct
  module Set = Set.Make (ARegPairKey)
  module Map = Map.Make (ARegPairKey)
end

let string_of_areg = Asm.string_of_abstract_reg
let string_of_areg_set s = U.string_of_set ~short:false s ~f:string_of_areg
let string_of_areg_set_short s = U.string_of_set ~short:true s ~f:string_of_areg
let string_of_areg_map m ~f = U.string_of_map ~short:false m ~k:string_of_areg ~v:f

let string_of_areg_pair (a, b) =
  sprintf "(%s, %s)" (string_of_areg a) (string_of_areg b)

let string_of_areg_pair_set s =
  U.string_of_set ~short:false s ~f:string_of_areg_pair

let string_of_areg_pair_map m ~f =
  U.string_of_map ~short:false m ~k:string_of_areg_pair ~v:f

let _set_to_string (set : AReg.Set.t) : string =
  let f acc reg = (string_of_abstract_reg reg) ^ ", " ^ acc in
  "{ " ^ (AReg.Set.fold ~f ~init:"" set) ^ " }"

(* ************************************************************************** *)
(* Live Variable Analysis                                                     *)
(* ************************************************************************** *)
module UseDefs = struct
  type usedefs = AReg.Set.t * AReg.Set.t

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
  let set_of_arg (arg: abstract_reg operand) : AReg.Set.t =
    let regs_list = no_rbp_or_rsp (regs_of_operand arg) in
    AReg.Set.of_list regs_list

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
    | None -> AReg.Set.empty, AReg.Set.empty

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
        (AReg.Set.union set1 set2, set2)
      | Mem _ ->
        (AReg.Set.union set1 set2, AReg.Set.empty)
      | _ -> AReg.Set.empty, AReg.Set.empty in
    Binop (instr, f)

  let binops_leaq =
    let instr = ["leaq"] in
    let f op1 op2 =
      let set1 = set_of_arg op1 in
      let set2 = set_of_arg op2 in
      match op2 with
      | Reg _ -> (set1, set2)
      | Mem _ ->
        (AReg.Set.union set1 set2, AReg.Set.empty)
      | _ -> AReg.Set.empty, AReg.Set.empty in
    Binop (instr, f)

  let binops_move =
    let instr = ["movq"; "mov"] in
    let f op1 op2 =
      let set1 = set_of_arg op1 in
      let set2 = set_of_arg op2 in
      match op2 with
      | Reg _ -> (set1, set2)
      | Mem _ ->
        (AReg.Set.union set1 set2, AReg.Set.empty)
      | _ -> AReg.Set.empty, AReg.Set.empty in
    Binop (instr, f)

  let binops_use =
    let instr = [ "bt"; "cmpq"; "test" ] in
    let f op1 op2 =
      let uses = AReg.Set.union (set_of_arg op1) (set_of_arg op2) in
      (uses, AReg.Set.empty) in
    Binop (instr, f)

  let unops_use_plus_def =
    let instr = [ "incq"; "decq"; "negq"; ] in
    let f op =
      let opregs = set_of_arg op in
      match op with
      | Reg _ -> (opregs, opregs)
      | Mem _ -> (opregs, AReg.Set.empty)
      | _ -> (AReg.Set.empty, AReg.Set.empty) in
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
      | Reg _ -> (AReg.Set.empty, opregs)
      | Mem _ -> (opregs, AReg.Set.empty)
      | _ -> (AReg.Set.empty, AReg.Set.empty) in
    Unop (instr, f)

  let unops_use =
    let instr = [ "push" ; "pushq" ] in
    let f op = (set_of_arg op, AReg.Set.empty) in
    Unop (instr, f)

  let unops_mul_div =
    let instr = [ "imulq"; "idivq"; ] in
    let f op =
      let useset = AReg.Set.add (set_of_arg op) (Real Rax) in
      let defset = AReg.Set.of_list [Real Rax; Real Rdx;] in
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
        AReg.Set.of_list in
      (useset, defset) in
    Unop (instr, f)

  let zeroop_ret =
    let instr = ["retq"] in
    let useset =
      callee_saved_regs |>
      List.map ~f:(fun reg -> Real reg) |>
      no_rbp_or_rsp |>
      AReg.Set.of_list in
    Zeroop (instr, (useset, AReg.Set.empty))

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
  let usedvars : abstract_asm -> AReg.Set.t * AReg.Set.t =
    function
      | Op (name, []) ->
          asm_match (ZeroopV name)
      | Op (name, arg :: []) ->
          asm_match (UnopV (name, arg))
      | Op (name, arg1 :: arg2 :: []) ->
          asm_match (BinopV (name, arg1, arg2))
      | _ -> (AReg.Set.empty, AReg.Set.empty)

end

module LiveVariableLattice = struct
  type data = AReg.Set.t
  let ( ** ) = AReg.Set.union
  let ( === ) = AReg.Set.equal
  let to_string data = string_of_areg_set data
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
    | Start | Exit -> AReg.Set.empty
    | Node n_data -> fst (usedvars n_data.asm)

  (* handle reg aliases *)
  let reg_alias (reg : abstract_reg) =
    match reg with
    | Real Cl -> Real Rcx
    | _ -> reg

  let transfer (_: extra_info) (e: AsmCfg.E.t) (d: Lattice.data) =
    (* We use dest because live variable analysis is backwards *)
    match E.dst e with
    | Start | Exit -> AReg.Set.empty
    | Node n_data ->
        let use_n, def_n =
          let f = AReg.Set.map ~f:reg_alias in
          let l, r = usedvars n_data.asm in
          f l, f r in
        AReg.Set.union use_n (AReg.Set.diff d def_n)

end

module LiveVariableAnalysis = GenericAnalysis (AsmWithLiveVar)

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

let reg_of_color = function
  | Reg1  -> Rax
  | Reg2  -> Rbx
  | Reg3  -> Rcx
  | Reg4  -> Rdx
  | Reg5  -> Rsi
  | Reg6  -> Rdi
  | Reg7  -> R8
  | Reg8  -> R9
  | Reg9  -> R10
  | Reg10 -> R11
  | Reg11 -> R12
  | Reg12 -> R13
  | Reg13 -> R14
  | Reg14 -> R15

let color_of_reg = function
  | Rax -> Reg1
  | Rbx -> Reg2
  | Rcx -> Reg3
  | Cl  -> Reg3
  | Rdx -> Reg4
  | Rsi -> Reg5
  | Rdi -> Reg6
  | R8  -> Reg7
  | R9  -> Reg8
  | R10 -> Reg9
  | R11 -> Reg10
  | R12 -> Reg11
  | R13 -> Reg12
  | R14 -> Reg13
  | R15 -> Reg14
  | _   -> failwith "color_of_reg: no color for rbp/rsp"

let string_of_color (c : color) =
  Asm.string_of_reg (reg_of_color c)

let get_next_color (colors : color list) : color option =
  let colorlist = [
    Reg1; Reg2; Reg3; Reg4; Reg5; Reg6; Reg7; Reg8; Reg9; Reg10; Reg11; Reg12;
    Reg13; Reg14;
  ] in
  let f acc x =
    match acc with
    | Some _ -> acc
    | None -> if not (List.mem colors x) then Some x else None
  in
  List.fold_left ~f ~init:None colorlist

type temp_move = {
  src:  abstract_reg;
  dest: abstract_reg;
  move: AsmData.t;
} [@@deriving sexp, compare]

module TempMoveSet = Set.Make(struct
  type t = temp_move [@@deriving sexp, compare]
end)

let string_of_temp_move ({src; dest; move} : temp_move) =
  let src_str  = string_of_areg src in
  let dest_str = string_of_areg dest in
  sprintf "{src=%s; dst=%s; move=%s}" src_str dest_str (AsmData.to_string move)

let string_of_temp_move_set s =
  U.string_of_set ~short:false s ~f:string_of_temp_move

let string_of_temp_move_set_short s =
  U.string_of_set ~short:true s ~f:string_of_temp_move

type alloc_context = {
  precolored         : AReg.Set.t;
  initial            : AReg.Set.t;
  simplify_wl        : AReg.Set.t;
  freeze_wl          : AReg.Set.t;
  spill_wl           : AReg.Set.t;
  spilled_nodes      : AReg.Set.t;
  coalesced_nodes    : AReg.Set.t;
  colored_nodes      : AReg.Set.t;
  select_stack       : abstract_reg list;
  coalesced_spills   : AReg.Set.t;
  coalesced_moves    : TempMoveSet.t;
  constrained_moves  : TempMoveSet.t;
  frozen_moves       : TempMoveSet.t;
  worklist_moves     : TempMoveSet.t;
  active_moves       : TempMoveSet.t;
  degree             : int AReg.Map.t;
  adj_list           : AReg.Set.t AReg.Map.t;
  adj_set            : ARegPair.Set.t;
  move_list          : TempMoveSet.t AReg.Map.t;
  alias              : abstract_reg AReg.Map.t;
  color_map          : color AReg.Map.t;
  node_occurrences   : int AReg.Map.t;
  num_colors         : int;
}

(* return node alias after coalescing; if node has not been coalesced,
 * reduces to identity function *)
let rec get_alias (node : abstract_reg) (regctx : alloc_context) : abstract_reg =
  if AReg.Set.mem regctx.coalesced_nodes node then
    match AReg.Map.find regctx.alias node with
    | Some a -> get_alias a regctx
    | None -> node
  else
    node

let string_of_precolored         = string_of_areg_set
let string_of_initial            = string_of_areg_set
let string_of_simplify_wl        = string_of_areg_set
let string_of_freeze_wl          = string_of_areg_set
let string_of_spill_wl           = string_of_areg_set
let string_of_spilled_nodes      = string_of_areg_set
let string_of_coalesced_nodes    = string_of_areg_set
let string_of_colored_nodes      = string_of_areg_set
let string_of_select_stack       = U.string_of_list ~short:false ~f:string_of_areg
let string_of_coalesced_spills   = string_of_areg_set
let string_of_coalesced_moves    = string_of_temp_move_set
let string_of_constrained_moves  = string_of_temp_move_set
let string_of_frozen_moves       = string_of_temp_move_set
let string_of_worklist_moves     = string_of_temp_move_set
let string_of_active_moves       = string_of_temp_move_set
let string_of_degree             = string_of_areg_map ~f:Int.to_string
let string_of_adj_list           = string_of_areg_map ~f:string_of_areg_set_short
let string_of_adj_set            = string_of_areg_pair_set
let string_of_move_list          = string_of_areg_map ~f:string_of_temp_move_set_short
let string_of_alias              = string_of_areg_map ~f:string_of_areg
let string_of_color_map          = string_of_areg_map ~f:string_of_color
let string_of_node_occurrences   = string_of_areg_map ~f:Int.to_string
let string_of_num_colors         = Int.to_string
let string_of_alloc_context c =
  let fields = [
    "precolored = "        ^ (string_of_precolored        (c.precolored));
    "initial = "           ^ (string_of_initial           (c.initial));
    "simplify_wl = "       ^ (string_of_simplify_wl       (c.simplify_wl));
    "freeze_wl = "         ^ (string_of_freeze_wl         (c.freeze_wl));
    "spill_wl = "          ^ (string_of_spill_wl          (c.spill_wl));
    "spilled_nodes = "     ^ (string_of_spilled_nodes     (c.spilled_nodes));
    "coalesced_nodes = "   ^ (string_of_coalesced_nodes   (c.coalesced_nodes));
    "colored_nodes = "     ^ (string_of_colored_nodes     (c.colored_nodes));
    "select_stack = "      ^ (string_of_select_stack      (c.select_stack));
    "coalesced_spills = "  ^ (string_of_coalesced_spills  (c.coalesced_spills));
    "coalesced_moves = "   ^ (string_of_coalesced_moves   (c.coalesced_moves));
    "constrained_moves = " ^ (string_of_constrained_moves (c.constrained_moves));
    "frozen_moves = "      ^ (string_of_frozen_moves      (c.frozen_moves));
    "worklist_moves = "    ^ (string_of_worklist_moves    (c.worklist_moves));
    "active_moves = "      ^ (string_of_active_moves      (c.active_moves));
    "degree = "            ^ (string_of_degree            (c.degree));
    "adj_list = "          ^ (string_of_adj_list          (c.adj_list));
    "adj_set = "           ^ (string_of_adj_set           (c.adj_set));
    "move_list = "         ^ (string_of_move_list         (c.move_list));
    "alias = "             ^ (string_of_alias             (c.alias));
    "color_map = "         ^ (string_of_color_map         (c.color_map));
    "node_occurrences = "  ^ (string_of_node_occurrences  (c.node_occurrences));
    "num_colors = "        ^ (string_of_num_colors        (c.num_colors));
  ] in
  sprintf "[\n%s\n]" (String.concat ~sep:"\n\n" fields)

(**** Invariant Checks ****)
let inter s1 s2 = AReg.Set.inter s1 s2

let union s1 s2 = AReg.Set.union s1 s2

let size s = Set.count s ~f:(fun _ -> true)

let disjoint_list_ok regctx =
  let l = [regctx.precolored; regctx.initial; regctx.simplify_wl; regctx.freeze_wl;
           regctx.spill_wl; regctx.spilled_nodes; regctx.coalesced_nodes;
           regctx.colored_nodes; AReg.Set.of_list regctx.select_stack] in
  List.for_alli l ~f:(fun i l' ->
    List.for_alli l ~f:(fun j l'' ->
      if i <> j then AReg.Set.is_empty (inter l' l'') else true))

let disjoint_set_ok regctx =
  let s = [regctx.coalesced_moves; regctx.frozen_moves; regctx.worklist_moves;
           regctx.active_moves; regctx.active_moves] in
  List.for_alli s ~f:(fun i s' ->
    List.for_alli s ~f:(fun j s'' ->
      if i <> j then TempMoveSet.is_empty (TempMoveSet.inter s' s'') else true))

(* TODO: The union of all of them forms exactly the entire list of nodes *)
let no_dups_ok regctx =
  not (List.contains_dup ~compare:(fun a b -> Asm.compare_abstract_reg a b)
    regctx.select_stack)

let degree_ok regctx =
  let (+) = AReg.Set.union in
  let (&) = AReg.Set.inter in
  let s = regctx.simplify_wl + regctx.freeze_wl + regctx.spill_wl in
  let s2 u = (AReg.Map.find_exn regctx.adj_list u) & (regctx.precolored +
    regctx.simplify_wl + regctx.freeze_wl + regctx.spill_wl) in
  AReg.Set.for_all s ~f:(fun u -> AReg.Map.find_exn regctx.degree u = size (s2 u))

let simplify_ok regctx =
  let (+) = TempMoveSet.union in
  let (&) = TempMoveSet.inter in
  let s = regctx.active_moves + regctx.worklist_moves in
  AReg.Set.for_all regctx.simplify_wl ~f:(fun u ->
    (AReg.Map.find_exn regctx.degree u < regctx.num_colors) &&
    (TempMoveSet.is_empty ((AReg.Map.find_exn regctx.move_list u) & s)))

let freeze_ok regctx =
  let (+) = TempMoveSet.union in
  let (&) = TempMoveSet.inter in
  let s = regctx.active_moves + regctx.worklist_moves in
  AReg.Set.for_all regctx.freeze_wl ~f:(fun u ->
    (AReg.Map.find_exn regctx.degree u) < regctx.num_colors &&
    not (TempMoveSet.is_empty ((AReg.Map.find_exn regctx.move_list u) & s)))

let spill_ok regctx =
  AReg.Set.for_all regctx.spill_wl ~f:(fun u ->
    (AReg.Map.find_exn regctx.degree u) >= regctx.num_colors)

let rep_ok =
  let num_ok = ref 0 in
  fun regctx ->
    incr num_ok;
    let invariants = [
      ("disjoint_list_ok = ", disjoint_list_ok regctx);
      ("disjoint_set_ok = ",  disjoint_set_ok regctx);
      ("no_dups_ok = ",       no_dups_ok regctx);
      ("degree_ok = ",        degree_ok regctx);
      ("simplify_ok = ",      simplify_ok regctx);
      ("freeze_ok = ",        freeze_ok regctx);
      ("spill_ok = ",         spill_ok regctx);
    ] in
    if List.for_all ~f:snd invariants then
      regctx
    else begin
      let strs = List.map invariants ~f:(fun (s, b) -> sprintf "%s%b" s b) in
      print_endline (string_of_alloc_context regctx);
      print_endline (U.join strs);
      failwith (sprintf "invariants not held on rep_ok %d" (!num_ok))
    end

let valid_coloring ({adj_list; color_map; spilled_nodes; coalesced_nodes; _} as c) =
  AReg.Map.for_alli adj_list ~f:(fun ~key ~data ->
    if AReg.Set.mem spilled_nodes (get_alias key c) then begin
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


let _string_of_abstract_regs (regs : abstract_reg list) : string =
  let f acc reg =
    (string_of_abstract_reg reg) ^ ", " ^ acc in
  List.fold_left ~f ~init:"" regs

let empty_ctx = {
  precolored         = AReg.Set.empty;
  initial            = AReg.Set.empty;
  simplify_wl        = AReg.Set.empty;
  freeze_wl          = AReg.Set.empty;
  spill_wl           = AReg.Set.empty;
  spilled_nodes      = AReg.Set.empty;
  coalesced_nodes    = AReg.Set.empty;
  colored_nodes      = AReg.Set.empty;
  select_stack       = [];
  coalesced_spills   = AReg.Set.empty;
  coalesced_moves    = TempMoveSet.empty;
  constrained_moves  = TempMoveSet.empty;
  frozen_moves       = TempMoveSet.empty;
  worklist_moves     = TempMoveSet.empty;
  active_moves       = TempMoveSet.empty;
  degree             = AReg.Map.empty;
  adj_list           = AReg.Map.empty;
  adj_set            = ARegPair.Set.empty;
  move_list          = AReg.Map.empty;
  alias              = AReg.Map.empty;
  color_map          = AReg.Map.empty;
  node_occurrences   = AReg.Map.empty;
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
let node_moves (node : abstract_reg) (regctx : alloc_context) : TempMoveSet.t =
  match AReg.Map.find regctx.move_list node with
  | Some nodemoves ->
      let f move =
        TempMoveSet.mem regctx.active_moves move ||
        TempMoveSet.mem regctx.worklist_moves move in
      TempMoveSet.filter ~f nodemoves
  | None -> TempMoveSet.empty

(* is a interference graph node still move related? *)
let move_related (node: abstract_reg) (regctx : alloc_context) : bool =
  size (node_moves node regctx) > 0

let add_edge (u : abstract_reg) (v : abstract_reg) regctx : alloc_context =
  if (not (ARegPair.Set.mem regctx.adj_set (u, v)) && (u <> v)) then
    begin
    let adj_set' =
      ARegPair.Set.union regctx.adj_set (ARegPair.Set.of_list [(u,v); (v,u)]) in

    let adj_list', degree' =
      if not (AReg.Set.mem regctx.precolored u) then
        adj_list_add u v regctx.adj_list, diff_int_map u succ regctx.degree
      else
        regctx.adj_list, regctx.degree in

    let adj_list'', degree'' =
      if not (AReg.Set.mem regctx.precolored v) then
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
    let module L = LiveVariableLattice in
    match v with
    | Start | Exit -> AReg.Set.empty
    | Node _ ->
        AsmCfg.fold_succ_e (fun e a -> L.(livevars_edge e ** a)) cfg v (AReg.Set.empty)
  in

  (* populate precolored, initial work lists *)
  let init0 (regctx : alloc_context) (reg : abstract_reg) =
    match reg with
    | Fake _ -> { regctx with initial = AReg.Set.add regctx.initial reg }
    | Real _ -> { regctx with precolored = AReg.Set.add regctx.precolored reg } in

  (* make edges for nodes interfering with each other in one statement *)
  let create_inter_edges (temps : AReg.Set.t) (regctx: alloc_context) : alloc_context =
    let g1 ctxacc1 reg1 =
      let g2 ctxacc2 reg2 =
        if reg1 <> reg2 then add_edge reg1 reg2 ctxacc2
        else ctxacc2 in
      AReg.Set.fold ~f:g2 ~init:ctxacc1 temps in
    AReg.Set.fold ~f:g1 ~init:regctx temps in

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
      let regctx' = create_inter_edges (AReg.Set.union liveset defs) regctx in

      (* update node occurrences *)
      let node_occurrences' =
        let f acc livevar =
          diff_int_map livevar succ acc in
        AReg.Set.fold ~f ~init:regctx'.node_occurrences liveset in

      (* populate worklist_moves and move_list *)
      let worklist_moves', move_list' =
        match coalescable_move cfg_node with
        | None -> regctx'.worklist_moves, regctx'.move_list
        | Some tempmove ->
            begin
              let ml =
                AReg.Map.update regctx'.move_list tempmove.src
                ~f:(function Some l -> TempMoveSet.add l tempmove
                           | None -> TempMoveSet.singleton tempmove) in
              let ml' =
                AReg.Map.update ml tempmove.dest
                ~f:(function Some l -> TempMoveSet.add l tempmove
                           | None -> TempMoveSet.singleton tempmove) in
              (TempMoveSet.add regctx.worklist_moves tempmove, ml')
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
          { regctx with spill_wl = AReg.Set.add regctx.spill_wl reg }
        else if move_related reg regctx then
          { regctx with freeze_wl = AReg.Set.add regctx.freeze_wl reg }
        else
          { regctx with simplify_wl = AReg.Set.add regctx.simplify_wl reg }
    (* precolored registers should not appear in initial! *)
    | Real _ -> regctx in

  (* add colors of precolored regs in color map *)
  let color_precoloreds (precoloreds : AReg.Set.t) regctx =
    let f ctxacc key =
      match key with
      | Fake _ -> ctxacc
      | Real r ->
        let data = color_of_reg r in
        { ctxacc with
          color_map = AReg.Map.add ctxacc.color_map ~key ~data } in
    AReg.Set.fold precoloreds ~f ~init:regctx in

  (* all fake temps in the program *)
  let fakes_set =
    let f fake = Fake fake in
    List.map ~f (fakes_of_asms asms) |> AReg.Set.of_list in

  (* set of all precolored nodes. we use this to create a precolored
   * clique in the interference graph. *)
  let precolored_set =
    let f r = Real r in
    List.map ~f [
      Rax; Rbx; Rcx; Rdx; Rsi; Rdi; R8;
      R9; R10; R11; R12; R13; R14; R15;
    ] |> AReg.Set.of_list in

  let all_vars_set = AReg.Set.union fakes_set precolored_set in

  let data_init (ctxacc : alloc_context) (key : abstract_reg) =
    { ctxacc with
      degree = AReg.Map.add ctxacc.degree ~key ~data:0;
      adj_list = AReg.Map.add ctxacc.adj_list ~key ~data:AReg.Set.empty;
      move_list = AReg.Map.add ctxacc.move_list ~key ~data:TempMoveSet.empty;
      alias = AReg.Map.add ctxacc.alias ~key ~data:key;
      node_occurrences = AReg.Map.add ctxacc.node_occurrences ~key ~data:0; } in

  (* initialize non-worklist data structures *)
  AReg.Set.fold ~f:data_init ~init:initctx all_vars_set |> fun regctx0 ->
  (* put all vars into either precoloreds or initial worklist *)
  AReg.Set.fold ~f:init0 ~init:regctx0 all_vars_set |>
  (* create interferences between all precolored nodes *)
  create_inter_edges precolored_set |>
  (* set colors of precolored nodes in color map *)
  color_precoloreds precolored_set |> fun regctx1 ->
  (* populate move worklists *)
  let nodes = AsmCfg.VertexSet.to_list (AsmCfg.vertex_set cfg) in
  let sorted_nodes = cfgnode_sort nodes in
  List.fold_left ~f:init1 ~init:regctx1 sorted_nodes |> fun regctx2 ->
  (* populate node worklists *)
  let finctx =
    AReg.Set.fold ~f:init2 ~init:{ regctx2 with initial = AReg.Set.empty} regctx2.initial in
  (finctx, livevars)

(* Returns a list of nodes adjacent to n that are not selected or coalesced.
 * Does not update the context. *)
let adjacent (reg : abstract_reg) regctx : AReg.Set.t =
  let used = AReg.Set.(union (of_list regctx.select_stack) regctx.coalesced_nodes) in
  AReg.Map.find regctx.adj_list reg |>
    function Some s ->
      AReg.Set.filter ~f:(fun m -> not (AReg.Set.mem used m)) s
    | None -> AReg.Set.empty

let degree (reg : abstract_reg) regctx : int =
  AReg.Map.find regctx.degree reg |> function Some n -> n | None -> 0

let enable_moves (nodes : AReg.Set.t) regctx : alloc_context =
  let f' regctx2 m =
    if TempMoveSet.mem regctx2.active_moves m then
      { regctx2 with
        active_moves = TempMoveSet.remove regctx2.active_moves m;
        worklist_moves = TempMoveSet.add regctx2.worklist_moves m;
      }
    else
      regctx2
  in
  let f regctx1 n =
    TempMoveSet.fold ~init:regctx1 (node_moves n regctx1) ~f:f'
  in
  AReg.Set.fold ~init:regctx nodes ~f

let decrement_degree (m : abstract_reg) regctx : alloc_context =
  let d' = (degree m regctx) - 1 in
  let regctx1 = { regctx with degree = diff_int_map m pred regctx.degree } in
  if d' = regctx1.num_colors - 1 then
    let regctx2 = enable_moves (union (adjacent m regctx1) (AReg.Set.singleton m))
      regctx1 in
    let regctx3 = { regctx2 with spill_wl = AReg.Set.remove regctx2.spill_wl m } in
    if move_related m regctx3 then
      { regctx3 with freeze_wl = AReg.Set.add regctx3.freeze_wl m }
    else
      { regctx3 with simplify_wl = AReg.Set.add regctx3.simplify_wl m }
  else
    regctx1

(* Remove non-move-related nodes of low degree *)
let simplify regctx : alloc_context =
  (* Pick a non-move-related vertex that has <k degree *)
  match AReg.Set.to_list regctx.simplify_wl with
  | [] -> regctx
  | n :: t ->
    let regctx1 =
      {
        regctx with
        simplify_wl = AReg.Set.of_list t;
        select_stack = n::regctx.select_stack;
      }
    in
    AReg.Set.fold
      ~f:(fun regctx2 m -> decrement_degree m regctx2)
      ~init:regctx1
      (adjacent n regctx1)

(* potentially add a new node to simplify_wl; see Appel for details *)
let add_wl (node : abstract_reg) (regctx : alloc_context) : alloc_context =
  if (not (AReg.Set.mem regctx.precolored node) &&
      not (move_related node regctx) &&
      degree node regctx < regctx.num_colors) then
      begin
        { regctx with
          freeze_wl = AReg.Set.remove regctx.freeze_wl node;
          simplify_wl = AReg.Set.add regctx.simplify_wl node;
        }
      end
  else
    regctx

let ok (t : abstract_reg) (r : abstract_reg) regctx : bool =
  (degree t regctx) < regctx.num_colors ||
  AReg.Set.mem regctx.precolored t ||
  ARegPair.Set.mem regctx.adj_set (t, r)

let conservative (nodes : AReg.Set.t) regctx : bool =
  let k' = 0 in
  let result = AReg.Set.fold ~init:k' nodes ~f:(fun acc n ->
    if (degree n regctx) >= (regctx.num_colors) then acc + 1 else acc)
  in
  result < (regctx.num_colors)

let combine u v regctx =
  let find map k = AReg.Map.find map k |> function Some l -> l
                                                 | None -> TempMoveSet.empty in

  let regctx1 =
    if AReg.Set.mem regctx.freeze_wl v then
      { regctx with freeze_wl = AReg.Set.remove regctx.freeze_wl v }
    else
      { regctx with spill_wl = AReg.Set.remove regctx.spill_wl v } in

  let coalesced_nodes' =
    AReg.Set.add regctx1.coalesced_nodes v in
  let alias' =
    AReg.Map.add regctx1.alias ~key:v ~data:u in
  let move_list' =
    let new_set = TempMoveSet.union (find regctx1.move_list u) (find regctx1.move_list v) in
    AReg.Map.add regctx1.move_list ~key:u ~data:new_set
  in

  let regctx2 =
    { regctx1 with
      coalesced_nodes = coalesced_nodes';
      alias = alias';
      move_list = move_list';
    }
  in

  let regctx3 = enable_moves (AReg.Set.singleton v) regctx2 in

  let f ctxacc t =
    add_edge t u ctxacc |> decrement_degree t
  in
  let regctx4 = AReg.Set.fold ~f ~init:regctx3 (adjacent v regctx3) in

  if (degree u regctx4) >= (regctx4.num_colors) && AReg.Set.mem regctx4.freeze_wl u then
    { regctx4 with
      freeze_wl = AReg.Set.remove regctx4.freeze_wl u;
      spill_wl = AReg.Set.add regctx4.spill_wl u
    }
  else
    regctx4

(* Coalesce move-related nodes *)
let coalesce (regctx : alloc_context) : alloc_context =
  match TempMoveSet.to_list regctx.worklist_moves with
  | [] -> regctx
  | m::t ->
    let x = get_alias m.src regctx in
    let y = get_alias m.dest regctx in
    let u, v = if AReg.Set.mem regctx.precolored y then (y, x) else (x, y) in
    let regctx1 = { regctx with worklist_moves = TempMoveSet.of_list t } in
    if u = v then
      let regctx2 = { regctx1 with
                       coalesced_moves = TempMoveSet.add regctx1.coalesced_moves m
                    }
      in
      add_wl u regctx2
    else if AReg.Set.mem regctx1.precolored v ||
            ARegPair.Set.mem regctx1.adj_set (u, v) then
      let regctx2 = { regctx1 with
                       constrained_moves = TempMoveSet.add regctx1.constrained_moves m
                     }
      in
      regctx2 |> add_wl u |> add_wl v
    else if
      (AReg.Set.mem regctx1.precolored u &&
       AReg.Set.fold ~init:true (adjacent v regctx1)
       ~f:(fun acc t -> acc && (ok t u regctx1)))
       ||
       (not (AReg.Set.mem regctx1.precolored u) &&
       conservative (union (adjacent u regctx1) (adjacent v regctx1)) regctx1) then
      let regctx2 = { regctx1 with
                       coalesced_moves = TempMoveSet.add regctx1.coalesced_moves m }
      in
      regctx2 |> combine u v |> add_wl u
    else
      { regctx1 with active_moves = TempMoveSet.add regctx1.active_moves m }

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
    let active_moves' = TempMoveSet.remove ctxacc.active_moves tempmove in
    let frozen_moves' = TempMoveSet.add ctxacc.frozen_moves tempmove in
    let ctxacc' = {
      ctxacc with
      active_moves = active_moves';
      frozen_moves = frozen_moves';
    }
    in
    let freeze_wl', simplify_wl' =
      if (AReg.Set.mem ctxacc'.freeze_wl v) && not (move_related v ctxacc') then
        AReg.Set.remove ctxacc'.freeze_wl v, AReg.Set.add ctxacc'.simplify_wl v
      else
        ctxacc'.freeze_wl, ctxacc'.simplify_wl in
    { ctxacc' with
      freeze_wl = freeze_wl';
      simplify_wl = simplify_wl';
    }
    in
    TempMoveSet.fold ~f ~init:regctx (node_moves node regctx)

let freeze (regctx : alloc_context) : alloc_context =
  match AReg.Set.to_list regctx.freeze_wl with
  | [] -> regctx
  | fnode :: t ->
    let regctx' = {
      regctx with
      freeze_wl = AReg.Set.of_list t;
      simplify_wl = AReg.Set.add regctx.simplify_wl fnode;
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
  match List.map ~f (AReg.Set.to_list regctx.spill_wl) |> List.sort ~cmp with
  | [] -> regctx
  | (chosenreg, _) :: _ ->
      let regctx' = {
        regctx with
        spill_wl = AReg.Set.remove regctx.spill_wl chosenreg;
        simplify_wl = AReg.Set.add regctx.simplify_wl chosenreg;
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
        if (AReg.Set.mem ctxacc.colored_nodes alias ||
            AReg.Set.mem ctxacc.precolored alias) then
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
          { ctxacc with spilled_nodes = AReg.Set.add ctxacc.spilled_nodes select_node; }
      | Some c ->
          let color_map' =
             AReg.Map.add ctxacc.color_map ~key:select_node ~data:c in
          { ctxacc with
            color_map = color_map';
            colored_nodes = AReg.Set.add ctxacc.colored_nodes select_node; }
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
          coalesced_spills = AReg.Set.add ctxacc.coalesced_spills coalesced_node
        }
      | Some c -> {
          ctxacc with
          color_map = AReg.Map.add ctxacc.color_map ~key:coalesced_node ~data:c
      } in
    let regctx' =
      AReg.Set.fold ~f ~init:{ regctx' with coalesced_nodes = AReg.Set.empty } regctx'.coalesced_nodes in
    (*List.iter ~f:print_endline (_string_of_ctx regctx');*)
    (* printf "assign_colors valid coloring? %b\n" (valid_coloring regctx'); *)
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
        if AReg.Set.mem regctx.coalesced_spills reg then
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
      if (AReg.Set.is_empty innerctx.simplify_wl &&
          TempMoveSet.is_empty innerctx.worklist_moves &&
          AReg.Set.is_empty innerctx.freeze_wl &&
          AReg.Set.is_empty innerctx.spill_wl) then
         rep_ok innerctx
      else
        let innerctx' =
          if not (AReg.Set.is_empty innerctx.simplify_wl) then
            rep_ok (simplify innerctx)
          else if not (TempMoveSet.is_empty innerctx.worklist_moves) then
            rep_ok (coalesce innerctx)
          else if not (AReg.Set.is_empty innerctx.freeze_wl) then
            rep_ok (freeze innerctx)
          else
            rep_ok (select_spill innerctx) in
        rep_ok (loop innerctx')
    in

    let (buildctx, livevars) = build regctx asms in
    let buildctx = rep_ok buildctx in
    let loopctx = rep_ok (loop buildctx) in
    let coloredctx = rep_ok (assign_colors loopctx) in

    if printing_on then begin
      printf "initial context = %s\n\n" (string_of_alloc_context buildctx);
      printf "looped context = %s\n\n" (string_of_alloc_context loopctx);
      printf "colored context = %s\n\n" (string_of_alloc_context coloredctx)
    end;

    (coloredctx, livevars)
  in

  let finctx, livevars = main empty_ctx given_asms in
  let finctx_comment = Comment (string_of_alloc_context finctx) in

  (* remove coalesced moves *)
  let numbered = List.mapi ~f:(fun num asm -> AsmData.{num; asm;}) given_asms in
  let comparator = Int.comparator in
  let f = fun {move; _} -> move.num in
  let coalesceds = Set.map ~comparator finctx.coalesced_moves ~f in
  let finasms = List.filter numbered ~f:(fun {num; _} ->
    not (Int.Set.mem coalesceds num)
  ) in

  (* add live variable comments *)
  let finasms = if debug then
    List.concat_map finasms ~f:(fun node ->
      let asm_str =
        sprintf "asm = %s" (string_of_abstract_asm node.asm) in
      let live_str =
        sprintf "live vars = %s" (string_of_areg_set (livevars (Node node))) in
      [Comment asm_str; Comment live_str; node.asm]
    )
  else
    List.map finasms ~f:(fun {asm; _} -> asm)
  in

  (* translate abstract_asms with allocated nodes, leaving spills.
   * stack allocate spill nodes with Tiling.register_allocate *)
  List.map ~f:(translate_asm finctx) finasms |> fun finasms' ->
    [finctx_comment] @ finasms' |> spill_allocate
