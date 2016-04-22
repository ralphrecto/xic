open Core.Std
open OUnit2
open TestUtil
open Util

module IntData = struct
  type t = int
  let to_string = string_of_int
  let to_int x = x
end
module IntCfg = Cfg.Make(IntData)
module IntCfgEq = struct
  let (===) (a: IntCfg.t) (b: IntCfg.t) : unit =
    assert_equal ~cmp:IntCfg.equal ~printer:IntCfg.to_dot a b

  let (=/=) (a: IntCfg.t) (b: IntCfg.t) : unit =
    if IntCfg.equal a b then
        let a = IntCfg.to_dot a in
        let b = IntCfg.to_dot b in
        assert_failure (sprintf "These are equal, but shouldn't be:\n%s\n%s" a b)
end

let test_cfg_equal _ =
  let open IntCfg in
  let open IntCfgEq in

  let tru  = Cfg.EdgeData.True in
  let fls  = Cfg.EdgeData.False in
  let norm = Cfg.EdgeData.Normal in

  let make_graph vertexes edges =
    let g = create () in
    List.iter vertexes ~f:(fun i -> add_vertex g (V.create i));
    List.iter edges ~f:(fun (i, l, j) -> add_edge_e g (E.create i l j));
    g
  in

  let v = [] in
  let e = [] in
  make_graph v e === make_graph v e;

  let v = [1] in
  let e = [] in
  make_graph v e === make_graph v e;

  let v = [1; 2] in
  let e = [] in
  make_graph v e === make_graph v e;

  let v = [1; 2; 3; 4; 5] in
  let e = [] in
  make_graph v e === make_graph v e;

  let v = [42; 624; 21] in
  let e = [] in
  make_graph v e === make_graph v e;

  let v = [1] in
  let e = [(1, tru, 1)] in
  make_graph v e === make_graph v e;

  let v = [1] in
  let e = [(1, fls, 1)] in
  make_graph v e === make_graph v e;

  let v = [1] in
  let e = [(1, norm, 1)] in
  make_graph v e === make_graph v e;

  let v = [1; 2; 3; 4; 5] in
  let e = [(1, tru, 2); (1, norm, 3); (4, tru, 5)] in
  make_graph v e === make_graph v e;

  let v1 = [1; 1] in
  let e1 = [] in
  let v2 = [1] in
  let e2 = [] in
  make_graph v1 e1 === make_graph v2 e2;

  let v1 = [1] in
  let e1 = [(1, norm, 1); (1, norm, 1)] in
  let v2 = [1] in
  let e2 = [(1, norm, 1)] in
  make_graph v1 e1 === make_graph v2 e2;

  let v1 = [1] in
  let e1 = [] in
  let v2 = [] in
  let e2 = [] in
  make_graph v1 e1 =/= make_graph v2 e2;

  let v1 = [] in
  let e1 = [] in
  let v2 = [1] in
  let e2 = [] in
  make_graph v1 e1 =/= make_graph v2 e2;

  let v1 = [1] in
  let e1 = [] in
  let v2 = [2] in
  let e2 = [] in
  make_graph v1 e1 =/= make_graph v2 e2;

  let v1 = [1] in
  let e1 = [] in
  let v2 = [2] in
  let e2 = [] in
  make_graph v1 e1 =/= make_graph v2 e2;

  let v1 = [1; 2] in
  let e1 = [(1, tru, 2)] in
  let v2 = [1; 2] in
  let e2 = [(2, tru, 1)] in
  make_graph v1 e1 =/= make_graph v2 e2;

  ()

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* ! DON'T FORGET TO ADD YOUR TESTS HERE                                     ! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let main () =
    "suite" >::: [
      "test_cfg_equal" >:: test_cfg_equal;
    ] |> run_test_tt_main

let _ = main ()
