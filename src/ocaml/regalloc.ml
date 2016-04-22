open Core.Std
open Graph
open Cfg
open Dataflow
open Asm
open Tiling
open Fresh

module LiveVariableLattice : LowerSemilattice with
  type data = Int.Set.t = struct

  type data = Int.Set.t
  let top = Int.Set.empty
  let ( ** ) = Int.Set.union
  let ( === ) = Int.Set.equal
end

module AsmWithLiveVar : CFGWithLatticeT = struct
  module Lattice = LiveVariableLattice
  module CFG = AbstractAsmCfg
  open Lattice
  open CFG

  type graph = CFG.t
  type node = CFG.V.t
  type edge = CFG.E.t
  type data = Lattice.data

  (* returns a sets of vars used and defd, respectively *)
  let usedvars : abstract_asm -> Int.Set.t * Int.Set.t =
    let set_of_arg (arg: abstract_reg operand) : Int.Set.t =
      let fakes = fakes_of_operand arg in
      let f acc fake =
        (* TODO: need to fix not all the temporary registers will be of form __asmreg- *)
        match FreshReg.get fake with
        | None -> acc
        | Some x -> Int.Set.add acc x in
      List.fold_left ~f ~init:Int.Set.empty fakes in
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
          (Int.Set.empty, arg_set)
        else if List.mem unops_use name then
          (arg_set, Int.Set.empty)
          (* TODO: HANDLE SPECIAL CASES!!! *)
        else if List.mem unops_special name then
          (Int.Set.empty, Int.Set.empty)
        else (Int.Set.empty, Int.Set.empty)
      | Op (name, arg1 :: arg2 :: []) ->
        let arg1_set = set_of_arg arg1 in
        let arg2_set = set_of_arg arg2 in
        let arg_union = Int.Set.union arg1_set arg2_set in
        if List.mem binops_use_plus_def name then
          (arg_union, arg2_set)
        else if List.mem binops_use name then
          (arg_union, Int.Set.empty)
        else (Int.Set.empty, Int.Set.empty)
      | _ -> (Int.Set.empty, Int.Set.empty)

  let transfer (e: edge) (d: data) =
    let n_data = V.label (E.src e) in
    let use_n, def_n = usedvars n_data.asm in
    Int.Set.union use_n (Int.Set.diff d def_n)
end

module LiveVariableAnalysis = BackwardAnalysis (AsmWithLiveVar)

module type InterferenceGraphT = sig
  type nodedata = { temp: string; is_mov: bool }
  type edgedata = unit

  include Graph.Sig.I with
    type V.label = nodedata and
    type E.label = edgedata

  (*val create_interg : string list -> (string * string) list -> (string * string)
    -> String.Set -> t*)
end

module InterferenceGraph : InterferenceGraphT = struct
  type nodedata = { temp: string; is_mov: bool }
  type edgedata = unit

  module NodeLabel : Graph.Sig.ANY_TYPE with type t = nodedata = struct
    type t = nodedata
  end

  module EdgeLabel : Graph.Sig.ORDERED_TYPE_DFT with type t = edgedata = struct
    type t = edgedata

    let compare _ _ = 0
    let default = ()
  end

  include LiveVariableAnalysis
  include Imperative.Graph.AbstractLabeled (NodeLabel) (EdgeLabel)

  (* TODO: Remove or figure out how to integrate sets *)
  module Extended_T = struct
    include Tuple.Make (String) (String)
    include Tuple.Sexpable (String) (String)
    include Tuple.Comparable (String) (String)
  end
  module TS = Set.Make (Extended_T)

  let create_edges temps =
    let rec create_edges' ts edges =
      match ts with
      | [] -> edges
      | set::t ->
        (* Make edges for nodes interfering with each other in one statement *)
        let new_edges = String.Set.fold ~init:[] set ~f:(fun acci i ->
          (String.Set.fold ~init:[] set ~f:(fun accj j ->
            if i <> j then (i, j)::accj else accj)) @ acci)
        in
        create_edges' t (edges@new_edges)
    in
    create_edges' temps []

  let is_mov_related _node = false (* TODO: how to get mov info? *)

  let _create_interg nodes es f =
    let g = create () in

    let edges = create_edges (List.fold_left ~init:[] es ~f:(fun acc e ->
      (f e)::acc)) in

    (* Add vertices *)
    List.fold_left ~init:() nodes ~f:(fun _ n ->
      let node = V.create { temp = n; is_mov = is_mov_related n } in
      add_vertex g node);

    (* Add edges *)
    List.fold_left ~init:() edges ~f:(fun _ (n1, n2) ->
      let node1 = V.create { temp = n1; is_mov = is_mov_related n1 } in
      let node2 = V.create { temp = n2; is_mov = is_mov_related n2 } in
      add_edge g node1 node2);

    g
end

module IG = InterferenceGraph

(* k is the number of registers available for coloring *)
let k = 14

let reg_alloc g k =
  let _g' = IG.copy g in

  (* Remove non-move-related nodes of low degree *)
  let _simplify g stack =
    (* Get non-move-related nodes of <k degree from graph *)
    let non_mov_nodes = IG.fold_vertex (fun v acc -> if (IG.V.label v).is_mov then acc
      else v::acc) g [] in
    
    (* Push all nodes of <k degree onto stack *)
    let rec push acc =
      (* Pick a non-move-related vertex that has <k degree *)
      let node = List.fold_left ~init:None non_mov_nodes ~f:(fun acc v ->
        match acc with
        | None -> if IG.in_degree g v < k then Some v else None
        | Some _ -> acc)
      in
      match node with
      | None -> acc
      | Some v -> IG.remove_vertex g v;
        push (v::acc)
    in
  
    let stack' = push stack in
    stack'
  in
  
  (* Coalesce move-related nodes *)
  let _coalesce _g _stack = failwith "TODO" in
  
  (* Remove a move-related node of low degree *)
  let _freeze _g _stack = failwith "TODO" in
  
  (* Spill a >=k degree node onto stack *)
  let _spill _g _stack = failwith "TODO" in
  
  (* Pop nodes from the stack and assign a color *)
  let _select _stack = failwith "TODO" in
  
  failwith "finish reg alloc!"
