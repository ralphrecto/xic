open Core.Std
open OUnit2
open TestUtil
open Util

module IntData = struct
  type t = int [@@deriving sexp, compare]
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

module IrCfgEq = struct
  let (===) (a: Cfg.IrCfg.t) (b: Cfg.IrCfg.t) : unit =
    assert_equal ~cmp:Cfg.IrCfg.equal ~printer:Cfg.IrCfg.to_dot a b

  let (=/=) (a: Cfg.IrCfg.t) (b: Cfg.IrCfg.t) : unit =
    if Cfg.IrCfg.equal a b then
        let a = Cfg.IrCfg.to_dot a in
        let b = Cfg.IrCfg.to_dot b in
        assert_failure (sprintf "These are equal, but shouldn't be:\n%s\n%s" a b)
end

module AsmCfgEq = struct
  let (===) (a: Cfg.AsmCfg.t) (b: Cfg.AsmCfg.t) : unit =
    assert_equal ~cmp:Cfg.AsmCfg.equal ~printer:Cfg.AsmCfg.to_dot a b

  let (=/=) (a: Cfg.AsmCfg.t) (b: Cfg.AsmCfg.t) : unit =
    if Cfg.AsmCfg.equal a b then
        let a = Cfg.AsmCfg.to_dot a in
        let b = Cfg.AsmCfg.to_dot b in
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

let test_ir_cfg _ =
  let open Cfg.IrCfg in
  let open Cfg.IrData in
  let open IrCfgEq in
  let open Ir.Abbreviations in
  let open Ir.Infix in

  let start = Cfg.IrDataStartExit.Start in
  let exit = Cfg.IrDataStartExit.Exit in
  let node num ir = Cfg.IrDataStartExit.Node {num; ir} in

  let tru  = Cfg.EdgeData.True in
  let fls  = Cfg.EdgeData.False in
  let norm = Cfg.EdgeData.Normal in

  let make_graph vertexes edges =
    let g = create () in
    List.iter vertexes ~f:(fun i -> add_vertex g (V.create i));
    List.iter edges ~f:(fun (i, l, j) -> add_edge_e g (E.create i l j));
    g
  in

  (* *)
  let irs = [] in
  let v = [start; exit] in
  let e = [(start, norm, exit)] in
  make_graph v e === Cfg.IrCfg.create_cfg irs;

  (* *)
  let s0 = return in
  let n0 = node 0 s0 in

  let irs = [s0] in
  let v = [start; n0; exit] in
  let e = [
    (start, norm, n0);
    (n0, norm, exit);
  ] in
  make_graph v e === Cfg.IrCfg.create_cfg irs;

  (* *)
  let s0 = move (temp "s0") (zero) in
  let s1 = move (temp "s1") (one) in
  let s2 = move (temp "s2") (two) in
  let s3 = return in

  let n0 = node 0 s0 in
  let n1 = node 1 s1 in
  let n2 = node 2 s2 in
  let n3 = node 3 s3 in

  let irs = [s0; s1; s2; s3] in
  let v = [start; n0; n1; n2; n3; exit] in
  let e = [
    (start, norm, n0);
    (n0, norm, n1);
    (n1, norm, n2);
    (n2, norm, n3);
    (n3, norm, exit);
  ] in
  make_graph v e === Cfg.IrCfg.create_cfg irs;

  (* *)
  let s0 = cjumpone one "foo" in
  let s1 = label "baz" in
  let s2 = jump (name "bar") in
  let s3 = label "foo" in
  let s4 = label "bar" in
  let s11 = cjumpone (one) "baz" in
  let s5 = move (temp "a") one in
  let s6 = return in
  let s7 = label "the" in
  let s8 = label "end" in
  let s9 = return in
  let s10 = jump (name "baz") in

  let n0 = node 0 s0 in
  let n1 = node 1 s1 in
  let n2 = node 2 s2 in
  let n3 = node 3 s3 in
  let n4 = node 4 s4 in
  let n11 = node 5 s11 in
  let n5 = node 6 s5 in
  let n6 = node 7 s6 in
  let n7 = node 8 s7 in
  let n8 = node 9 s8 in
  let n9 = node 10 s9 in
  let n10 = node 11 s10 in

  let irs = [s0; s1; s2; s3; s4; s11; s5; s6; s7; s8; s9; s10] in
  let v = [start; n0; n1; n2; n3; n4; n11; n5; n6; n7; n8; n9; n10; exit] in
  let e = [
    (start, norm, n0);

    (n0, fls, n1);
    (n1, norm, n2);
    (n2, norm, n4);

    (n0, tru, n3);
    (n3, norm, n4);

    (n4, norm, n11);
    (n11, fls, n5);
    (n5, norm, n6);

    (n11, tru, n1);

    (n7, norm, n8);
    (n8, norm, n9);

    (n10, norm, n1);

    (n6, norm, exit);
    (n9, norm, exit);
  ] in
  make_graph v e === Cfg.IrCfg.create_cfg irs;

  ()


let test_asm_cfg _ =
  let open Cfg.AsmCfg in
  let open Cfg.AsmData in
  let open AsmCfgEq in
  let open Asm in

  let start = Cfg.AsmDataStartExit.Start in
  let exit = Cfg.AsmDataStartExit.Exit in
  let node num asm = Cfg.AsmDataStartExit.Node {num; asm} in

  let tru  = Cfg.EdgeData.True in
  let fls  = Cfg.EdgeData.False in
  let norm = Cfg.EdgeData.Normal in

  let make_graph vertexes edges =
    let g = create () in
    List.iter vertexes ~f:(fun i -> add_vertex g (V.create i));
    List.iter edges ~f:(fun (i, l, j) -> add_edge_e g (E.create i l j));
    g
  in

  let _ = [tru; fls; norm] in
  let _ = node in

  (* *)
  let asms = [] in
  let v = [start; exit] in
  let e = [(start, norm, exit)] in
  make_graph v e === Cfg.AsmCfg.create_cfg asms;

  ()



(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* ! DON'T FORGET TO ADD YOUR TESTS HERE                                     ! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let main () =
    "suite" >::: [
      "test_cfg_equal" >:: test_cfg_equal;
      "test_ir_cfg"    >:: test_ir_cfg;
      (* "test_asm_cfg"   >:: test_asm_cfg; *)
    ] |> run_test_tt_main

let _ = main ()
