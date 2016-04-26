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

let live_variables_test _ =
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

  let livevars = LiveVariableAnalysis.worklist () cfg in

  let expecteds = [
    ((start, n0), set_of []);
    ((n0, n1), set_of [fk 0]);
    ((n1, n2), set_of [fk 0; fk 1]);
    ((n2, exit), set_of []);
  ] in

  assert_livevars cfg expecteds livevars;

  ()


let reg_alloc_test _ =
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

  let livevars = LiveVariableAnalysis.worklist () cfg in

  let expecteds = [
    ((start, n0), set_of []);
    ((n0, n1), set_of [fk 0]);
    ((n1, n2), set_of [fk 0; fk 1]);
    ((n2, exit), set_of []);
  ] in

  assert_livevars cfg expecteds livevars;

  (* test 2 *********************************************************)

    (*.globl _Imain_paai*)
   let _n0 = globl "_Imain_paai" in
    (*.align 4*)
   let _n1 = align 4 in
(*_Imain_paai:*)
   let _n2 = label_op "_Imain_paai" in
    (*enter $128, $0*)
   let _n3 = enter (const 128) (const 0) in
    (*movq %rbx, -16(%rbp)*)
   let _n4 = movq (rr Asm.Rbx) (const 0) in
    (*movq %r12, -88(%rbp)*)
    (*movq %r13, -96(%rbp)*)
    (*movq %r14, -104(%rbp)*)
    (*movq %r15, -112(%rbp)*)
(*__label0:*)
    (*movq -120(%rbp), %r13*)
    (*movq $5, %r13*)
    (*movq %r13, -120(%rbp)*)
    (*movq -120(%rbp), %r13*)
    (*movq -128(%rbp), %r14*)
    (*movq %r13, %r14*)
    (*movq %r13, -120(%rbp)*)
    (*movq %r14, -128(%rbp)*)
    (*movq -16(%rbp), %rbx*)
    (*movq -88(%rbp), %r12*)
    (*movq -96(%rbp), %r13*)
    (*movq -104(%rbp), %r14*)
    (*movq -112(%rbp), %r15*)
    (*leave*)
    (*retq*)



  ()



(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* ! DON'T FORGET TO ADD YOUR TESTS HERE                                     ! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let main () =
    "suite" >::: [
      "live_variables_test" >:: live_variables_test;
      "reg_alloc_test" >:: reg_alloc_test;
    ] |> run_test_tt_main

let _ = main ()

