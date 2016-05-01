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

  let set_to_string (set : AReg.Set.t) : string =
    let f acc reg = (string_of_abstract_reg reg) ^ ", " ^ acc in
    "{ " ^ (AReg.Set.fold ~f ~init:"" set) ^ " }" in

  let set_of (regs : abstract_reg list) : AReg.Set.t =
    AReg.Set.of_list regs in

  let (===) (a: AReg.Set.t) (b: AReg.Set.t) : unit =
    assert_equal ~cmp:AReg.Set.equal ~printer:set_to_string a b in

  let fk i = Asm.Fake (FreshReg.gen i) in
  let fkr i = Asm.Reg (fk i) in
  let _rr reg = Asm.Reg (Asm.Real reg) in

  (* remove x from lst, if it exists *)
  let remove (lst : 'a list) (x : 'a) : 'a list =
    let f y = x <> y in
    List.filter ~f lst in

  (* we do not include Rbp and Rsp at all in our regalloc algo *)
  let no_rbp_or_rsp (regs : abstract_reg list) =
    remove regs (Asm.Real Asm.Rbp) |> fun regs' ->
    remove regs' (Asm.Real Asm.Rsp) in

  (* grabs all temps that appear in an operand *)
  let _set_of_arg (arg: abstract_reg operand) : AReg.Set.t =
    let regs_list = no_rbp_or_rsp (regs_of_operand arg) in
    AReg.Set.of_list regs_list in

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

  let set_to_string (set : AReg.Set.t) : string =
    let f acc reg = (string_of_abstract_reg reg) ^ ", " ^ acc in
    "{ " ^ (AReg.Set.fold ~f ~init:"" set) ^ " }" in

  let set_of (regs : abstract_reg list) : AReg.Set.t =
    AReg.Set.of_list regs in

  let (===) (a: AReg.Set.t) (b: AReg.Set.t) : unit =
    assert_equal ~cmp:AReg.Set.equal ~printer:set_to_string a b in

  let assert_livevars
    (cfg : Cfg.AsmCfg.t)
    (expecteds : ((Cfg.AsmCfg.V.t * Cfg.AsmCfg.V.t) * AReg.Set.t) list)
    (livevars_f : Cfg.AsmCfg.E.t -> AReg.Set.t) =
      let f ((src, dest), expected) =
        let actual = Cfg.AsmCfg.find_edge cfg src dest |> livevars_f in
        expected === actual in
      List.iter ~f expecteds in

  let fk i = Asm.Fake (FreshReg.gen i) in
  let fkr i = Asm.Reg (fk i) in
  let r reg = Asm.Real reg in
  let rr reg = Asm.Reg (r reg) in

  let lab l = Asm.Label l in
  let dummy = const 1 in
  let m0 x = movq dummy (rr Asm.R15) |> node x in
  let m1 x = movq dummy (rr Asm.R14) |> node x in
  let m2 x = movq dummy (rr Asm.R13) |> node x in
  let m3 x = movq dummy (rr Asm.R12) |> node x in
  let m4 x = movq dummy (rr Asm.Rbx) |> node x in
  (* Return nodes for 2 branches *)
  let r00 = m0 6 in
  let r01 = m1 7 in
  let r02 = m2 8 in
  let r03 = m3 9 in
  let r04 = m4 10 in
  let r10 = m0 6 in
  let r11 = m1 7 in
  let r12 = m2 8 in
  let r13 = m3 9 in
  let r14 = m4 10 in



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

  (* test 4 *********************************************************)
  let n0 = movq (const 5) (fkr 0) |> node 0 in
  let n1 = movq (const 6) (fkr 1) |> node 1 in
  let n2 = cmpq (fkr 0) (fkr 1) |> node 2 in
  let n3 = jge (lab "tru") |> node 3 in
  let n4 = ret |> node 4 in
  let n5 = (label_op "tru") |> node 5 in
  let n6 = ret |> node 6 in

  let v = [start; n0; n1; n2; n3; n4; r00; r01; r02; r03; r04;
           r10; r11; r12; r13; r14; n5; n6; exit] in
  let e = [
    (start, norm, n0);
    (n0, norm, n1);
    (n1, norm, n2);
    (n2, norm, n3);
    (n3, norm, r00);
    (r00, norm, r01);
    (r01, norm, r02);
    (r02, norm, r03);
    (r03, norm, r04);
    (r04, norm, n4);
    (n4, norm, exit);
    (n3, norm, n5);
    (n5, norm, r10);
    (r10, norm, r11);
    (r11, norm, r12);
    (r12, norm, r13);
    (r13, norm, r14);
    (r14, norm, n6);
    (n6, norm, exit);
  ] in
  let cfg = make_graph v e in

  let livevars = LiveVariableAnalysis.worklist () cfg in

  let expecteds = [
    ((start, n0), set_of []);
    ((n0, n1), set_of [fk 0]);
    ((n1, n2), set_of [fk 0; fk 1]);
    ((n2, n3), set_of []);
    ((n3, r00), set_of []);
    ((r00, r01), set_of [r Asm.R15]);
    ((r01, r02), set_of [r Asm.R14; r Asm.R15]);
    ((r02, r03), set_of [r Asm.R13; r Asm.R14; r Asm.R15]);
    ((r03, r04), set_of [r Asm.R12; r Asm.R13; r Asm.R14; r Asm.R15]);
    ((r04, n4), set_of [r Asm.Rbx; r Asm.R12; r Asm.R13; r Asm.R14; r Asm.R15]);
    ((n4, exit), set_of []);
    ((n3, n5), set_of []);
    ((n5, r10), set_of []);
    ((r10, r11), set_of [r Asm.R15]);
    ((r11, r12), set_of [r Asm.R14; r Asm.R15]);
    ((r12, r13), set_of [r Asm.R13; r Asm.R14; r Asm.R15]);
    ((r13, r14), set_of [r Asm.R12; r Asm.R13; r Asm.R14; r Asm.R15]);
    ((r14, n6), set_of [r Asm.Rbx; r Asm.R12; r Asm.R13; r Asm.R14; r Asm.R15]);
    ((n6, exit), set_of []);
  ] in

  assert_livevars cfg expecteds livevars;

  (* test 5 *********************************************************)
  let n0 = movq (const 5) (fkr 0) |> node 0 in
  let n1 = movq (const 6) (fkr 1) |> node 1 in
  let n2 = cmpq (fkr 0) (fkr 1) |> node 2 in
  let n3 = jge (lab "tru") |> node 3 in
  let n4 = movq (const 7) (fkr 1) |> node 4 in
  let n5 = node 5 ret in
  let n6 = label_op "tru" |> node 6 in
  let n7 = movq (const 10) (fkr 1) |> node 7 in
  let n8 = node 8 ret in

  let v = [start; n0; n1; n2; n3; n4; n5; r00; r01; r02; r03; r04;
           r10; r11; r12; r13; r14; n6; n7; n8; exit] in
  let e = [
    (start, norm, n0);
    (n0, norm, n1);
    (n1, norm, n2);
    (n2, norm, n3);
    (n3, norm, n4);
    (n4, norm, r00);
    (r00, norm, r01);
    (r01, norm, r02);
    (r02, norm, r03);
    (r03, norm, r04);
    (r04, norm, n5);
    (n5, norm, exit);
    (n3, norm, n6);
    (n6, norm, n7);
    (n7, norm, r10);
    (r10, norm, r11);
    (r11, norm, r12);
    (r12, norm, r13);
    (r13, norm, r14);
    (r14, norm, n8);
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
    ((n4, r00), set_of []);
    ((r00, r01), set_of [r Asm.R15]);
    ((r01, r02), set_of [r Asm.R14; r Asm.R15]);
    ((r02, r03), set_of [r Asm.R13; r Asm.R14; r Asm.R15]);
    ((r03, r04), set_of [r Asm.R12; r Asm.R13; r Asm.R14; r Asm.R15]);
    ((r04, n5), set_of [r Asm.Rbx; r Asm.R12; r Asm.R13; r Asm.R14; r Asm.R15]);
    ((n5, exit), set_of []);
    ((n3, n6), set_of []);
    ((n6, n7), set_of []);
    ((n7, r10), set_of []);
    ((r10, r11), set_of [r Asm.R15]);
    ((r11, r12), set_of [r Asm.R14; r Asm.R15]);
    ((r12, r13), set_of [r Asm.R13; r Asm.R14; r Asm.R15]);
    ((r13, r14), set_of [r Asm.R12; r Asm.R13; r Asm.R14; r Asm.R15]);
    ((r14, n8), set_of [r Asm.Rbx; r Asm.R12; r Asm.R13; r Asm.R14; r Asm.R15]);
    ((n8, exit), set_of []);
  ] in

  assert_livevars cfg expecteds livevars;

  ()

let reg_alloc_test _ =
  let open Asm in
  let open Abbreviations in
  let open Regalloc in
  let open Fresh in

  let fk i = Asm.Fake (FreshReg.gen i) in
  let temp i = Asm.Reg (fk i) in
  let rr reg = Asm.Reg (Asm.Real reg) in

  let _ = fk in
  let _ = temp in
  let _ = rr in

  let label s = Asm.Label s in

  let abstr_asms = [
    movq (const 1) (temp 0);
    movq (const 1) (temp 1);
    movq (const 1) (temp 2);
    cmpq (temp 0) (temp 1);
    je (label "label0");
    movq (const 3) (temp 0);
    jmp (label "label1");
    label_op "label0";
    movq (const 5) (temp 0);
    label_op "label1";
  ] in
  let asms = reg_alloc abstr_asms in
  print_endline (string_of_asms asms);

  assert_true false



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

