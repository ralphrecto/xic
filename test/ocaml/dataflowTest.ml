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

(* The transfer function f_i for a node i is a stateful function that ignores
 * all its arguments and reutrn 0 then 1 then 2 then ... then i. Thus, the
 * fixpoint of this dataflow has every node i with value i. *)
module Incrementing = struct
  module Lattice = IntLattice
  module CFG = IntCfg

  type graph = CFG.t
  type node = CFG.V.t
  type edge = CFG.E.t
  type data = Lattice.data

  let cache = Int.Table.create ()
  let transfer e _ =
    let i = CFG.E.src e  in
    match Int.Table.find cache i with
    | Some j -> begin
      (* printf "cache found %d\n" j; *)
      (Int.Table.set cache ~key:i ~data:(min i (j + 1)); j)
    end
    | None -> (Int.Table.set cache ~key:i ~data:(min i 1); 0)

  let init g _ = CFG.fold_vertex (fun v a -> max (CFG.V.label v) a) g 0
end
module IncrementingForward = Dataflow.ForwardAnalysis(Incrementing)

let test_dataflow _ =
  let test flow_algorithm =
    (* Given two vertices, completely ignore them and generate a random label. *)
    let random_edge_label _ _ =
      let edge_labels = Cfg.EdgeData.([True; False; Normal]) in
      let index = Random.int (List.length edge_labels) in
      List.nth_exn edge_labels index
    in

    List.iter (List.range 0 2) ~f:(fun _ ->
      (* generate a random graph *)
      let v = (Random.int 1000) + 1 in
      let average_degree = Random.float 0.2 in
      let prob = average_degree /. (float_of_int v) in
      let g = RandIntCfg.gnp_labeled random_edge_label ~v ~prob () in

      (* add a start and end *)
      let start = -1 in
      let exit = -2  in
      IntCfg.add_vertex g start;
      IntCfg.add_vertex g exit;
      IntCfg.iter_vertex (fun v ->
        let i = IntCfg.V.label v in
        if i <> start && i <> exit then (
          if IntCfg.in_degree g v = 0 then (
            IntCfg.add_edge g (IntCfg.V.create start) v
          );
          if IntCfg.out_degree g v = 0 then (
            IntCfg.add_edge g v (IntCfg.V.create exit)
          )
        )
      ) g;

      (* reset the cache and run the dataflow *)
      (* print_endline (IntCfg.to_dot g); *)
      Int.Table.clear Incrementing.cache;
      let f = flow_algorithm g in

      (* make sure we've reached the correct fixpoint *)
      let open TestUtil.IntEq in
      IntCfg.iter_edges_e (fun e ->
        let i = IntCfg.V.label (IntCfg.E.src e) in
        if i <> start && i <> exit then
          IntCfg.(V.label (E.src e)) === f e
      ) g
    )
  in

  test IncrementingForward.iterative;
  test IncrementingForward.worklist

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* ! DON'T FORGET TO ADD YOUR TESTS HERE                                     ! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let main () =
    "suite" >::: [
      "test_dataflow" >:: test_dataflow;
    ] |> run_test_tt_main

let _ = main ()
