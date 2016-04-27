open Core.Std
open OUnit2
open TestUtil
open Util
open Regalloc
open Asm

module AsmCfgEq = struct
  let (===) (a: Cfg.AsmCfg.t) (b: Cfg.AsmCfg.t) : unit =
    assert_equal ~cmp:Cfg.AsmCfg.equal ~printer:Cfg.AsmCfg.to_dot a b

  let (=/=) (a: Cfg.AsmCfg.t) (b: Cfg.AsmCfg.t) : unit =
    if Cfg.AsmCfg.equal a b then
        let a = Cfg.AsmCfg.to_dot a in
        let b = Cfg.AsmCfg.to_dot b in
        assert_failure (sprintf "These are equal, but shouldn't be:\n%s\n%s" a b)
end

let use_defs_test _ =
  let open Cfg.AsmCfg in
  let open Cfg.AsmData in
  let open AsmCfgEq in
  let open Asm in
  let open Abbreviations in
  let open Regalloc in
  let open Fresh in

  let set_to_string (set : AbstractRegSet.t) : string =
    let f acc reg = (string_of_abstract_reg reg) ^ ", " ^ acc in
    "{ " ^ (AbstractRegSet.fold ~f ~init:"" set) ^ " }" in

  let set_of (regs : abstract_reg list) : AbstractRegSet.t =
    AbstractRegSet.of_list regs in

  let (===) (a: AbstractRegSet.t) (b: AbstractRegSet.t) : unit =
    assert_equal ~cmp:AbstractRegSet.equal ~printer:set_to_string a b in

  let fk i = Asm.Fake (FreshReg.gen i) in
  let fkr i = Asm.Reg (fk i) in
  let rr reg = Asm.Reg (Asm.Real reg) in

  let _ = set_to_string in
  let _ = fk in
  let _ = fkr in
  let _ = rr in

  (* remove x from lst, if it exists *)
  let remove (lst : 'a list) (x : 'a) : 'a list =
    let f y = x <> y in
    List.filter ~f lst in

  (* we do not include Rbp and Rsp at all in our regalloc algo *)
  let no_rbp_or_rsp (regs : abstract_reg list) =
    remove regs (Asm.Real Asm.Rbp) |> fun regs' ->
    remove regs' (Asm.Real Asm.Rsp) in

  (* grabs all temps that appear in an operand *)
  let set_of_arg (arg: abstract_reg operand) : AbstractRegSet.t =
    let regs_list = no_rbp_or_rsp (regs_of_operand arg) in
    AbstractRegSet.of_list regs_list in

  let _ = set_of_arg in

  let n0 = movq (const 5) (fkr 0) in
  let uses0, defs0 = UseDefs.usedvars n0 in
  (set_of []) === uses0;
  (set_of [fk 0]) === defs0;

  let n1 = movq (const 6) (fkr 1) in
  let uses1, defs1 = UseDefs.usedvars n1 in
  (set_of []) === uses1;
  (set_of [fk 1]) === defs1;

  let n2 = addq (fkr 0) (fkr 1) in
  let uses2, defs2 = UseDefs.usedvars n2 in
  (set_of [fk 0; fk 1]) === uses2;
  (set_of [fk 1]) === defs2


let live_vars_test _ =
  let open Cfg.AsmCfg in
  let open Cfg.AsmData in
  let open AsmCfgEq in
  let open Asm in
  let open Abbreviations in
  let open Regalloc in
  let open Fresh in

  let start = Cfg.AsmDataStartExit.Start in
  let exit = Cfg.AsmDataStartExit.Exit in
  let node num asm = Cfg.AsmDataStartExit.Node {num; asm} in

  let _tru  = Cfg.EdgeData.True in
  let _fls  = Cfg.EdgeData.False in
  let norm = Cfg.EdgeData.Normal in

  let make_graph vertexes edges =
    let g = create () in
    List.iter vertexes ~f:(fun i -> add_vertex g (V.create i));
    List.iter edges ~f:(fun (i, l, j) -> add_edge_e g (E.create i l j));
    g
  in

  let set_to_string (set : AbstractRegSet.t) : string =
    let f acc reg = (string_of_abstract_reg reg) ^ ", " ^ acc in
    "{ " ^ (AbstractRegSet.fold ~f ~init:"" set) ^ " }" in

  let set_of (regs : abstract_reg list) : AbstractRegSet.t =
    AbstractRegSet.of_list regs in

  let (===) (a: AbstractRegSet.t) (b: AbstractRegSet.t) : unit =
    assert_equal ~cmp:AbstractRegSet.equal ~printer:set_to_string a b in

  let assert_livevars
    (cfg : Cfg.AsmCfg.t)
    (expecteds : ((Cfg.AsmCfg.V.t * Cfg.AsmCfg.V.t) * AbstractRegSet.t) list)
    (livevars_f : Cfg.AsmCfg.E.t -> AbstractRegSet.t) =
      let f ((src, dest), expected) =
        let actual = Cfg.AsmCfg.find_edge cfg src dest |> livevars_f in
        expected === actual in
      List.iter ~f expecteds in

  let fk i = Asm.Fake (FreshReg.gen i) in
  let fkr i = Asm.Reg (fk i) in
  let rr reg = Asm.Reg (Asm.Real reg) in

  let _lab l = Asm.Label l in

  let _ = rr in

  (* test 1 *********************************************************)
  let n0 = movq (const 5) (fkr 0) |> node 0 in
  let n1 = movq (const 6) (fkr 1) |> node 1 in
  let n2 = addq (fkr 0) (fkr 1) |> node 2 in

  let v = [start; n0; n1; n2; exit] in
  let e = [
    (start, norm, n0);
    (n0, norm, n1);
    (n1, norm, n2);
    (n2, norm, exit);
  ] in
  let cfg = make_graph v e in

  let livevars = LiveVariableAnalysis.iterative () cfg in

  let expecteds = [
    ((start, n0), set_of []);
    ((n0, n1), set_of [fk 0]);
    ((n1, n2), set_of [fk 0; fk 1]);
    ((n2, exit), set_of []);
  ] in

  assert_livevars cfg expecteds livevars;

  (* worklist test *)
  let livevars = LiveVariableAnalysis.worklist () cfg in
  assert_livevars cfg expecteds livevars;

  (* test 2 *********************************************************)
  let n0 = movq (const 5) (fkr 0) |> node 0 in
  let n1 = movq (const 6) (fkr 1) |> node 1 in
  let n2 = cmpq (fkr 0) (fkr 1) |> node 2 in
  let v = [start; n0; n1; n2; exit] in
  let e = [
    (start, norm, n0);
    (n0, norm, n1);
    (n1, norm, n2);
    (n2, norm, exit);
  ] in
  let cfg = make_graph v e in

  let livevars = LiveVariableAnalysis.worklist () cfg in

  let expecteds = [
    ((start, n0), set_of []);
    ((n0, n1), set_of [fk 0]);
    ((n1, n2), set_of [fk 0; fk 1]);
    ((n2, exit), set_of []);
  ] in
  
  assert_livevars cfg expecteds livevars;

  (* test 3 *********************************************************)
  let n0 = movq (const 5) (fkr 0) |> node 0 in
  let n1 = movq (const 6) (fkr 1) |> node 1 in
  let n2 = cmpq (fkr 0) (fkr 1) |> node 2 in
  let n3 = movq (const 7) (fkr 1) |> node 3 in
  let n4 = addq (fkr 1) (fkr 1) |> node 4 in
  let v = [start; n0; n1; n2; n3; n4; exit] in
  let e = [
    (start, norm, n0);
    (n0, norm, n1);
    (n1, norm, n2);
    (n2, norm, n3);
    (n3, norm, n4);
    (n4, norm, exit);
  ] in
  let cfg = make_graph v e in

  let livevars = LiveVariableAnalysis.worklist () cfg in

  let expecteds = [
    ((start, n0), set_of []);
    ((n0, n1), set_of [fk 0]);
    ((n1, n2), set_of [fk 0; fk 1]);
    ((n2, n3), set_of []);
    ((n3, n4), set_of [fk 1]);
    ((n4, exit), set_of []);
  ] in
  
  assert_livevars cfg expecteds livevars;
  (*
  let n0 = movq (const 5) (fkr 0) |> node 0 in
  let n1 = movq (const 6) (fkr 1) |> node 1 in
  let n2 = cmpq (fkr 0) (fkr 1) |> node 2 in
  let n3 = jge (lab "tru") |> node 3 in
  let n4 = movq (const 7) (fkr 1) |> node 4 in
  let n5 = node 5 ret in
  let n6 = label_op "tru" |> node 6 in
  let n7 = movq (const 10) (fkr 1) |> node 7 in
  let n8 = node 8 ret in

  let v = [start; n0; n1; n2; n3; n4; n5; n6; n7; n8; exit] in
  let e = [
    (start, norm, n0);
    (n0, norm, n1);
    (n1, norm, n2);
    (n2, norm, n3);
    (n3, norm, n4);
    (n4, norm, n5);
    (n5, norm, exit);
    (n3, norm, n6);
    (n6, norm, n7);
    (n7, norm, n8);
    (n8, norm, exit);
  ] in
  let cfg = make_graph v e in

  let livevars = LiveVariableAnalysis.worklist () cfg in

  let expecteds = [
    ((start, n0), set_of []);
    ((n0, n1), set_of [fk 0]);
    ((n1, n2), set_of [fk 0; fk 1]);
    ((n2, n3), set_of []);
    ((n3, n4), set_of []);
    ((n4, n5), set_of []);
    ((n5, exit), set_of []);
    ((n3, n6), set_of []);
    ((n6, n7), set_of []);
    ((n7, n8), set_of []);
    ((n8, exit), set_of []);
  ] in
*)

  ()

let reg_alloc_test _ =
  let open Asm in
  let open Abbreviations in
  let open Regalloc in
  let open Fresh in

  let fk i = Asm.Fake (FreshReg.gen i) in
  let fkr i = Asm.Reg (fk i) in
  let rr reg = Asm.Reg (Asm.Real reg) in

  let _ = fk in
  let _ = fkr in
  let _ = rr in

  let n0 = movq (const 5) (fkr 0) in
  let n1 = movq (const 6) (fkr 1) in
  let n2 = addq (fkr 0) (fkr 1) in

  let res = reg_alloc [n0; n1; n2] in
  print_endline (string_of_asms res);

  ()



(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* ! DON'T FORGET TO ADD YOUR TESTS HERE                                     ! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let main () =
    "suite" >::: [
      "use_defs_test" >:: use_defs_test;
      "live_vars_test" >:: live_vars_test;
      "reg_alloc_test" >:: reg_alloc_test;
    ] |> run_test_tt_main

let _ = main ()

