open Core.Std
open OUnit2
open Util

module IntData = struct
  type t = int
  let to_string = string_of_int
  let to_int x = x
end
module IntCfg = Cfg.Make(IntData)
module RandIntCfg = Graph.Rand.I(IntCfg)

module IntCfgEq = struct
  let (===) (a: IntCfg.t) (b: IntCfg.t) : unit =
    assert_equal ~cmp:IntCfg.equal ~printer:IntCfg.to_dot a b

  let (=/=) (a: IntCfg.t) (b: IntCfg.t) : unit =
    if IntCfg.equal a b then
        let a = IntCfg.to_dot a in
        let b = IntCfg.to_dot b in
        assert_failure (sprintf "These are equal, but shouldn't be:\n%s\n%s" a b)
end

(* false < true *)
module AndLattice = struct
  type data = bool
  let ( ** ) = (&&)
  let (===) = (=)
  let to_string = string_of_bool
end

(* true < false *)
module OrLattice = struct
  type data = bool
  let ( ** ) = (||)
  let (===) = (=)
  let to_string = string_of_bool
end

(* ... < -1 < 0 < 1 < ... *)
module IntLattice = struct
  type data = int
  let ( ** ) = min
  let (===) = (=)
  let to_string = string_of_int
end

(* Determines whether code is reachable. *)
module Reachable = struct
  module Lattice = OrLattice
  module CFG = IntCfg

  type graph = CFG.t
  type node = CFG.V.t
  type edge = CFG.E.t
  type data = Lattice.data

  type extra_info = unit

  let start = -1
  let exit = -2

  let transfer _ _ x = x
  let init _ v = CFG.V.label v = start || CFG.V.label v = exit
end
module ReachableForwards  = Dataflow.ForwardAnalysis(Reachable)
module BfsReachable = struct
  include Graph.Traverse.Bfs(IntCfg)
  let reachable g start =
    let s = Int.Hash_set.create () in
    iter_component (Hash_set.add s) g start;
    s
end

let test_dataflow _ =
  let test flow_algorithm =
    (* Given two vertices, completely ignore them and generate a random label. *)
    let random_edge_label _ _ =
      let edge_labels = Cfg.EdgeData.([True; False; Normal]) in
      let index = Random.int (List.length edge_labels) in
      List.nth_exn edge_labels index
    in

    List.iter (List.range 0 10) ~f:(fun _ ->
      (* generate a random graph *)
      let v = (Random.int 1000) + 1 in
      let average_degree = Random.float 2.0 in
      let prob = average_degree /. (float_of_int v) in
      let g = RandIntCfg.gnp_labeled ~loops:true random_edge_label ~v ~prob () in

      (* add a start and end *)
      IntCfg.add_vertex g Reachable.start;
      IntCfg.add_vertex g Reachable.exit;
      IntCfg.iter_vertex (fun v ->
        let i = IntCfg.V.label v in
        if i <> Reachable.start && i <> Reachable.exit then (
          if IntCfg.in_degree g v = 0 then (
            IntCfg.add_edge g (IntCfg.V.create Reachable.start) v
          );
          if IntCfg.out_degree g v = 0 then (
            IntCfg.add_edge g v (IntCfg.V.create Reachable.exit)
          )
        )
      ) g;

      (* reset the cache and run the dataflow *)
      let f = flow_algorithm g in

      (* make sure we've reached the correct fixpoint *)
      let reachables = BfsReachable.reachable g (IntCfg.V.create Reachable.start) in
      let open TestUtil.BoolEq in
      IntCfg.iter_edges_e (fun e ->
        let i = IntCfg.V.label (IntCfg.E.src e) in
        if i <> Reachable.start && i <> Reachable.exit then (
          Hash_set.mem reachables (IntCfg.E.src e) === (f e)
        )
      ) g
    )
  in

  test (ReachableForwards.iterative ());
  test (ReachableForwards.worklist ());
  ()

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* ! DON'T FORGET TO ADD YOUR TESTS HERE                                     ! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let main () =
    "suite" >::: [
      "test_dataflow" >:: test_dataflow;
    ] |> run_test_tt_main

let _ = main ()
