open Core.Std
open OUnit2
open TestUtil
open Util

module AsmsEq = struct
  let (===) (a: Asm.asm list) (b: Asm.asm list) : unit =
    assert_equal ~printer:(fun a -> "\n" ^ Asm.string_of_asms a ^ "\n") a b
end

module AbstrAsmsEq = struct
  let (===) (a: Asm.abstract_asm list) (b: Asm.abstract_asm list) : unit =
    assert_equal ~printer:(fun a -> "\n" ^ Asm.string_of_abstract_asms a ^ "\n") a b
end

module MunchExprEq = struct
  let (===) ((r1, a1): (Asm.fake * Asm.abstract_asm list))
            ((r2, a2): (Asm.fake * Asm.abstract_asm list)) : unit =
    let printer (r, a) = sprintf "\n%s\n%s\n" r (Asm.string_of_abstract_asms a) in
    assert_equal ~printer (r1, a1) (r2, a2)
end

module Dummy = struct
  open Func_context
  let dummy_ctx = {num_args = 0; num_rets = 0; max_args = 0; max_rets = 0;}
  let dummy_fcontexts = String.Map.empty
end

let test_munch_expr _ =
  let open Ir in
  let open Ir.Abbreviations in
  let module IRA = Ir.Abbreviations in
  let open Asm in
  let open Asm.Abbreviations in
  let open Ir.Infix in
  let open MunchExprEq in
  let open Tiling in
  let open Dummy in

  (* helpers *)
  let gen i = FreshReg.gen i in
  let fakereg i = Fake (gen i) in
  let fakeop i = fake (gen i) in
  let mem_rsp = Asm.Mem (Base (None, Real Rsp)) in
  let mem_rbp = Asm.Mem (Base (None, Real Rbp)) in

  let test ?(ctx=dummy_ctx) ?(ctxs=dummy_fcontexts) expected input =
    FreshReg.reset ();
    expected === munch_expr ctx ctxs input;
  in

  (* binops *)
  (* simple math *)
  let simple_binop_test irop asmop =
    let input = irop one two in
    let expected = ((gen 0), [
      movq (const 1) (fakeop 0);
      movq (const 2) (fakeop 1);
      asmop (fakeop 1) (fakeop 0);
    ]) in
    test expected input
  in
  simple_binop_test (+)  addq;
  simple_binop_test (-)  subq;
  simple_binop_test (&)  andq;
  simple_binop_test (||) orq;
  simple_binop_test (^)  xorq;

  (* shifts *)
  let shift_binop_test irop asmop =
    let input = irop one two in
    let expected = ((gen 0), [
      movq (const 1) (fakeop 0);
      movq (const 2) (fakeop 1);
      movq (fakeop 1) arcx;
      asmop acl (fakeop 0);
    ]) in
    test expected input
  in
  shift_binop_test (<<)  shlq;
  shift_binop_test (>>)  shrq;
  shift_binop_test (>>>) sarq;

  (* cmps *)
  let cmp_binop_test irop asmop =
    let input = irop one two in
    let expected = ((gen 1), [
      movq (const 1) (fakeop 0);
      movq (const 2) (fakeop 1);
      cmpq (fakeop 1) (fakeop 0);
      asmop (Reg (Real Cl));
      movq (Reg (Real Rcx)) (fakeop 1);
    ]) in
    test expected input
  in
  cmp_binop_test (==) asete;
  cmp_binop_test (!=) asetne;
  cmp_binop_test (< ) asetl;
  cmp_binop_test (> ) asetg;
  cmp_binop_test (<=) asetle;
  cmp_binop_test (>=) asetge;

  (* mul/highmul *)
  let mul_binop_test irop asmreg =
    let input = irop one two in
    let expected = ((gen 1), [
      movq (const 1) (fakeop 0);
      movq (const 2) (fakeop 1);
      movq (fakeop 0) arax;
      imulq (fakeop 1);
      movq asmreg (fakeop 1);
    ]) in
    test expected input
  in
  mul_binop_test ( *   ) arax;
  mul_binop_test ( *>> ) ardx;

  (* div/rem *)
  let divrem_binop_test irop asmreg =
    let input = irop one two in
    let expected = ((gen 1), [
      movq (const 1) (fakeop 0);
      movq (const 2) (fakeop 1);
      xorq ardx ardx;
      movq (fakeop 0) arax;
      idivq (fakeop 1);
      movq asmreg (fakeop 1);
    ]) in
    test expected input
  in
  divrem_binop_test (/) arax;
  divrem_binop_test (%) ardx;

  (* consts *)
  let input = three in
  let expected = ((gen 0), [
    movq (const 3) (fakeop 0)
  ]) in
  test expected input;

  let input = zero in
  let expected = ((gen 0), [
    movq (const 0) (fakeop 0)
  ]) in
  test expected input;

  (* mem *)
  let input = mem ~typ:NORMAL one in
  let expected = ((gen 0), [
    movq (const 1) (fakeop 0);
    movq (Asm.Mem (Base (None, fakereg 0))) (fakeop 0);
  ]) in
  test expected input;

  let input = mem ~typ:IMMUTABLE one in
  let expected = ((gen 0), [
    movq (const 1) (fakeop 0);
    movq (Asm.Mem (Base (None, fakereg 0))) (fakeop 0);
  ]) in
  test expected input;

  (* temps *)
  (* basic temps *)
  let input = temp "foo" in
  let expected = ((gen 0), [
    movq (fake "foo") (fakeop 0)
  ]) in
  test expected input;

  let input = temp "bar" in
  let expected = ((gen 0), [
    movq (fake "bar") (fakeop 0)
  ]) in
  test expected input;

  (* return temps *)
  let ret_temp_test i =
    let input = temp (Ir_generation.FreshRetReg.gen i) in
    let expected = ((FreshAsmRet.gen i), []) in
    test expected input;
  in

  ret_temp_test 0;
  ret_temp_test 1;
  ret_temp_test 2;
  ret_temp_test 3;
  ret_temp_test 4;

  (* arg temps *)
  let arg_temp_test num_rets i asmoperand =
    let fc = Func_context.({dummy_ctx with num_rets}) in
    let input = temp (Ir_generation.FreshArgReg.gen i) in
    let expected = ((gen 0), [
      movq asmoperand (fakeop 0)
    ]) in
    test ~ctx:fc expected input;
  in

  arg_temp_test 0 0 ardi;
  arg_temp_test 0 1 arsi;
  arg_temp_test 0 2 ardx;
  arg_temp_test 0 3 arcx;
  arg_temp_test 0 4 ar8;
  arg_temp_test 0 5 ar9;
  arg_temp_test 0 6 (16L $ mem_rbp);
  arg_temp_test 0 7 (24L $ mem_rbp);
  arg_temp_test 0 8 (32L $ mem_rbp);

  arg_temp_test 1 0 ardi;
  arg_temp_test 1 1 arsi;
  arg_temp_test 1 2 ardx;
  arg_temp_test 1 3 arcx;
  arg_temp_test 1 4 ar8;
  arg_temp_test 1 5 ar9;
  arg_temp_test 1 6 (16L $ mem_rbp);
  arg_temp_test 1 7 (24L $ mem_rbp);
  arg_temp_test 1 8 (32L $ mem_rbp);

  arg_temp_test 2 0 ardi;
  arg_temp_test 2 1 arsi;
  arg_temp_test 2 2 ardx;
  arg_temp_test 2 3 arcx;
  arg_temp_test 2 4 ar8;
  arg_temp_test 2 5 ar9;
  arg_temp_test 2 6 (16L $ mem_rbp);
  arg_temp_test 2 7 (24L $ mem_rbp);
  arg_temp_test 2 8 (32L $ mem_rbp);

  arg_temp_test 3 0 arsi;
  arg_temp_test 3 1 ardx;
  arg_temp_test 3 2 arcx;
  arg_temp_test 3 3 ar8;
  arg_temp_test 3 4 ar9;
  arg_temp_test 3 5 (16L $ mem_rbp);
  arg_temp_test 3 6 (24L $ mem_rbp);
  arg_temp_test 3 7 (32L $ mem_rbp);
  arg_temp_test 3 8 (40L $ mem_rbp);

  (* call *)
  let ctx = Func_context.({dummy_ctx with max_args = 8}) in
  let call_ctx = Func_context.({dummy_ctx with num_args = 8; num_rets = 2}) in
  let ctxs = String.Map.singleton "foo" call_ctx in
  let input = IRA.call (name "foo") [one; two; three; four; five; six; seven; eight] in
  let expected = (FreshAsmRet.gen 0, [
    movq (Reg (Real Rax)) ( -8L $ mem_rbp);
    movq (Reg (Real Rcx)) (-24L $ mem_rbp);
    movq (Reg (Real Rdx)) (-32L $ mem_rbp);
    movq (Reg (Real Rsi)) (-40L $ mem_rbp);
    movq (Reg (Real Rdi)) (-48L $ mem_rbp);
    movq (Reg (Real R8))  (-56L $ mem_rbp);
    movq (Reg (Real R9))  (-64L $ mem_rbp);
    movq (Reg (Real R10)) (-72L $ mem_rbp);
    movq (Reg (Real R11)) (-80L $ mem_rbp);

    movq (const 1) (fakeop 0);
    movq (const 2) (fakeop 1);
    movq (const 3) (fakeop 2);
    movq (const 4) (fakeop 3);
    movq (const 5) (fakeop 4);
    movq (const 6) (fakeop 5);
    movq (const 7) (fakeop 6);
    movq (const 8) (fakeop 7);

    movq (fakeop 0) ardi;
    movq (fakeop 1) arsi;
    movq (fakeop 2) ardx;
    movq (fakeop 3) arcx;
    movq (fakeop 4) ar8;
    movq (fakeop 5) ar9;
    movq (fakeop 6) (0L  $ mem_rsp);
    movq (fakeop 7) (8L  $ mem_rsp);

    call (Asm.Label "foo");

    movq arax (Reg (Fake (FreshAsmRet.gen 0)));
    movq ardx (Reg (Fake (FreshAsmRet.gen 1)));

    movq ( -8L $ mem_rbp) (Reg (Real Rax));
    movq (-24L $ mem_rbp) (Reg (Real Rcx));
    movq (-32L $ mem_rbp) (Reg (Real Rdx));
    movq (-40L $ mem_rbp) (Reg (Real Rsi));
    movq (-48L $ mem_rbp) (Reg (Real Rdi));
    movq (-56L $ mem_rbp) (Reg (Real R8));
    movq (-64L $ mem_rbp) (Reg (Real R9));
    movq (-72L $ mem_rbp) (Reg (Real R10));
    movq (-80L $ mem_rbp) (Reg (Real R11));
  ]) in
  test ~ctx ~ctxs expected input;

  let ctx = Func_context.({dummy_ctx with max_args = 9}) in
  let call_ctx = Func_context.({dummy_ctx with num_args = 9; num_rets = 4}) in
  let ctxs = String.Map.singleton "foo" call_ctx in
  let input = IRA.call (name "foo") [one; two; three; four; five; six; seven; eight] in
  let expected = (FreshAsmRet.gen 0, [
    movq (Reg (Real Rax)) ( -8L $ mem_rbp);
    movq (Reg (Real Rcx)) (-24L $ mem_rbp);
    movq (Reg (Real Rdx)) (-32L $ mem_rbp);
    movq (Reg (Real Rsi)) (-40L $ mem_rbp);
    movq (Reg (Real Rdi)) (-48L $ mem_rbp);
    movq (Reg (Real R8))  (-56L $ mem_rbp);
    movq (Reg (Real R9))  (-64L $ mem_rbp);
    movq (Reg (Real R10)) (-72L $ mem_rbp);
    movq (Reg (Real R11)) (-80L $ mem_rbp);

    movq (const 1) (fakeop 0);
    movq (const 2) (fakeop 1);
    movq (const 3) (fakeop 2);
    movq (const 4) (fakeop 3);
    movq (const 5) (fakeop 4);
    movq (const 6) (fakeop 5);
    movq (const 7) (fakeop 6);
    movq (const 8) (fakeop 7);

    leaq (24L $ mem_rsp) (fakeop 8);

    movq (fakeop 8) ardi;
    movq (fakeop 0) arsi;
    movq (fakeop 1) ardx;
    movq (fakeop 2) arcx;
    movq (fakeop 3) ar8;
    movq (fakeop 4) ar9;
    movq (fakeop 5) (0L  $ mem_rsp);
    movq (fakeop 6) (8L  $ mem_rsp);
    movq (fakeop 7) (16L $ mem_rsp);

    call (Asm.Label "foo");

    movq arax (Reg (Fake (FreshAsmRet.gen 0)));
    movq ardx (Reg (Fake (FreshAsmRet.gen 1)));
    movq (24L $ mem_rsp) (Reg (Fake (FreshAsmRet.gen 2)));
    movq (32L $ mem_rsp) (Reg (Fake (FreshAsmRet.gen 3)));

    movq ( -8L $ mem_rbp) (Reg (Real Rax));
    movq (-24L $ mem_rbp) (Reg (Real Rcx));
    movq (-32L $ mem_rbp) (Reg (Real Rdx));
    movq (-40L $ mem_rbp) (Reg (Real Rsi));
    movq (-48L $ mem_rbp) (Reg (Real Rdi));
    movq (-56L $ mem_rbp) (Reg (Real R8));
    movq (-64L $ mem_rbp) (Reg (Real R9));
    movq (-72L $ mem_rbp) (Reg (Real R10));
    movq (-80L $ mem_rbp) (Reg (Real R11));
  ]) in
  test ~ctx ~ctxs expected input;
  ()

let test_munch_stmt _ =
  let open Ir in
  let open Ir.Abbreviations in
  let module IRA = Ir.Abbreviations in
  let open Asm in
  let open Asm.Abbreviations in
  let open Ir.Infix in
  let open AbstrAsmsEq in
  let open Tiling in
  let open Dummy in

  (* helpers *)
  let gen i = FreshReg.gen i in
  let fakeop i = fake (gen i) in
  let mem_rbp = Asm.Mem (Base (None, Real Rbp)) in

  let test ?(ctx=dummy_ctx) ?(ctxs=dummy_fcontexts) expected input =
    FreshReg.reset ();
    expected === munch_stmt ctx ctxs input;
  in

  (* cjumpone *)
  let input = cjumpone one "tru" in
  let expected = [
    movq (const 1) (fakeop 0);
    cmpq (const 0) (fakeop 0);
    jnz (Asm.Label "tru");
  ] in
  test expected input;

  let input = cjumpone zero "tru" in
  let expected = [
    movq (const 0) (fakeop 0);
    cmpq (const 0) (fakeop 0);
    jnz (Asm.Label "tru");
  ] in
  test expected input;

  let input = cjumpone (one == two) "tru" in
  let expected = [
    movq (const 1) (fakeop 0);
    movq (const 2) (fakeop 1);
    cmpq (fakeop 1) (fakeop 0);
    asete acl;
    movq arcx (fakeop 1);
    cmpq (const 0) (fakeop 1);
    jnz (Asm.Label "tru");
  ] in
  test expected input;

  (* jump *)
  let input = jump (name "foo") in
  let expected = [jmp (Asm.Label "foo")] in
  test expected input;

  (* exp *)
  let input = exp one in
  let expected = [movq (const 1) (fakeop 0)] in
  test expected input;

  (* label *)
  let input = label "foo" in
  let expected = [Lab "foo"] in
  test expected input;

  (* move *)
  let input = move (temp "foo") one in
  let expected = [
    movq (const 1) (fakeop 0);
    movq (fakeop 0) (Reg (Fake "foo"));
  ] in
  test expected input;

  let ret_mov_test i asmreg =
    let input = move (temp (Ir_generation.FreshRetReg.gen i)) one in
    let expected = [
      movq (const 1) (fakeop 0);
      movq (fakeop 0) asmreg;
    ] in
    test expected input
  in
  ret_mov_test 0 arax;
  ret_mov_test 1 ardx;
  ret_mov_test 2 ( 0L $ (Asm.Mem (Base (None, ret_ptr_reg))));
  ret_mov_test 3 ( 8L $ (Asm.Mem (Base (None, ret_ptr_reg))));
  ret_mov_test 4 (16L $ (Asm.Mem (Base (None, ret_ptr_reg))));

  (* seq *)
  let x = temp "x" in
  let y = temp "y" in
  let input = seq [move x one; move y two; move x three] in
  let expected = [
    movq (const 1) (fakeop 0);
    movq (fakeop 0) (Reg (Fake "x"));
    movq (const 2) (fakeop 1);
    movq (fakeop 1) (Reg (Fake "y"));
    movq (const 3) (fakeop 2);
    movq (fakeop 2) (Reg (Fake "x"));
  ] in
  test expected input;

  (* return *)
  let input = return in
  let expected = [
    movq ( -16L $ mem_rbp) arbx;
    movq ( -88L $ mem_rbp) ar12;
    movq ( -96L $ mem_rbp) ar13;
    movq (-104L $ mem_rbp) ar14;
    movq (-112L $ mem_rbp) ar15;
    leave;
    ret;
  ] in
  test expected input;

  ()

let test_munch_func_decl _ =
  let open Ir in
  let open Ir.Abbreviations in
  let open Ir.Infix in
  let module IRA = Ir.Abbreviations in
  let open Asm in
  let open Asm.Abbreviations in
  let open AbstrAsmsEq in
  let open Tiling in
  let open Dummy in

  (* helpers *)
  let gen i = FreshReg.gen i in
  let fakeop i = fake (gen i) in
  let mem_rbp = Asm.Mem (Base (None, Real Rbp)) in

  let body_1 = seq [exp one] in
  let body_2 = seq [exp one; exp two] in
  let body_3 = seq [exp one; exp two; exp three] in

  let ctxs = String.Map.of_alist_exn Func_context.([
    "f0000", {num_args=0; num_rets=0; max_args=0; max_rets=0};
    "f6789", {num_args=6; num_rets=7; max_args=8; max_rets=9};
  ]) in

  let test ?(ctxs=ctxs) expected input =
    FreshReg.reset ();
    expected === munch_func_decl ctxs input;
  in

  (* f0000 *)
  let input = ("f0000", body_1, Typecheck.Expr.(UnitT, UnitT)) in
  let expected = [
    globl "f0000";
    align 4;
    Lab "f0000";
    enter (const 120) (const 0);
    pushq (const 0);
    movq arbx ( -16L $ mem_rbp);
    movq ar12 ( -88L $ mem_rbp);
    movq ar13 ( -96L $ mem_rbp);
    movq ar14 (-104L $ mem_rbp);
    movq ar15 (-112L $ mem_rbp);
    movq (const 1) (fakeop 0);
  ] in
  test expected input;

  let input = ("f0000", body_2, Typecheck.Expr.(UnitT, UnitT)) in
  let expected = [
    globl "f0000";
    align 4;
    Lab "f0000";
    enter (const 128) (const 0);
    movq arbx ( -16L $ mem_rbp);
    movq ar12 ( -88L $ mem_rbp);
    movq ar13 ( -96L $ mem_rbp);
    movq ar14 (-104L $ mem_rbp);
    movq ar15 (-112L $ mem_rbp);
    movq (const 1) (fakeop 0);
    movq (const 2) (fakeop 1);
  ] in
  test expected input;

  let input = ("f0000", body_3, Typecheck.Expr.(UnitT, UnitT)) in
  let expected = [
    globl "f0000";
    align 4;
    Lab "f0000";
    enter (const 136) (const 0);
    pushq (const 0);
    movq arbx ( -16L $ mem_rbp);
    movq ar12 ( -88L $ mem_rbp);
    movq ar13 ( -96L $ mem_rbp);
    movq ar14 (-104L $ mem_rbp);
    movq ar15 (-112L $ mem_rbp);
    movq (const 1) (fakeop 0);
    movq (const 2) (fakeop 1);
    movq (const 3) (fakeop 2);
  ] in
  test expected input;

  (* f6789 *)
  let input = ("f6789", body_1, Typecheck.Expr.(UnitT, UnitT)) in
  let expected = [
    globl "f6789";
    align 4;
    Lab "f6789";
    enter (const 256) (const 0);
    movq ardi (Reg ret_ptr_reg);
    movq arbx ( -16L $ mem_rbp);
    movq ar12 ( -88L $ mem_rbp);
    movq ar13 ( -96L $ mem_rbp);
    movq ar14 (-104L $ mem_rbp);
    movq ar15 (-112L $ mem_rbp);
    movq (const 1) (fakeop 0);
  ] in
  test expected input;

  let input = ("f6789", body_2, Typecheck.Expr.(UnitT, UnitT)) in
  let expected = [
    globl "f6789";
    align 4;
    Lab "f6789";
    enter (const 264) (const 0);
    pushq (const 0);
    movq ardi (Reg ret_ptr_reg);
    movq arbx ( -16L $ mem_rbp);
    movq ar12 ( -88L $ mem_rbp);
    movq ar13 ( -96L $ mem_rbp);
    movq ar14 (-104L $ mem_rbp);
    movq ar15 (-112L $ mem_rbp);
    movq (const 1) (fakeop 0);
    movq (const 2) (fakeop 1);
  ] in
  test expected input;

  let input = ("f6789", body_3, Typecheck.Expr.(UnitT, UnitT)) in
  let expected = [
    globl "f6789";
    align 4;
    Lab "f6789";
    enter (const 272) (const 0);
    movq ardi (Reg ret_ptr_reg);
    movq arbx ( -16L $ mem_rbp);
    movq ar12 ( -88L $ mem_rbp);
    movq ar13 ( -96L $ mem_rbp);
    movq ar14 (-104L $ mem_rbp);
    movq ar15 (-112L $ mem_rbp);
    movq (const 1) (fakeop 0);
    movq (const 2) (fakeop 1);
    movq (const 3) (fakeop 2);
  ] in
  test expected input;
  ()

let test_chomp _ =
  let open Ir in
  let open Ir.Infix in
  let open Ir.Abbreviations in
  let open Asm in
  let open AbstrAsmsEq in
  let open Tiling in
  let open Dummy in
  let module IA = Ir.Abbreviations in

  FreshReg.reset ();
  let fresh0 = FreshReg.fresh () in
  let fresh1 = FreshReg.fresh () in
  let fresh2 = FreshReg.fresh () in
  let fresh3 = FreshReg.fresh () in
  let fresh4 = FreshReg.fresh () in
  let fresh5 = FreshReg.fresh () in

  let reg0 = Reg (Fake (fresh0)) in
  let reg1 = Reg (Fake (fresh1)) in
  let _reg2 = Reg (Fake (fresh2)) in
  let _reg3 = Reg (Fake (fresh3)) in
  let _reg4 = Reg (Fake (fresh4)) in
  let _reg5 = Reg (Fake (fresh5)) in

  (* mod2 == 0 with no set destination *)
  FreshReg.reset ();
  let expr1 = ((temp "x") % (IA.const 2L)) == (IA.const 0L) in
  let fresh_reg = reg0 in
  let expected = [
    movq (Reg (Fake "x")) fresh_reg;
    bt (Asm.Const 0L) fresh_reg;
    asetnc (Reg (Real Cl));
    movq (Reg (Real Rcx)) fresh_reg;
  ]
  in
  expected === (snd (chomp_expr dummy_ctx dummy_fcontexts expr1));

  (* mod2 == 0 with set destination *)
  FreshReg.reset ();
  let stmt1 = move (temp "y") (((temp "x") % (IA.const 2L)) == (IA.const 0L)) in
  let fresh_reg = reg0 in
  let expected = [
    movq (Reg (Fake "x")) fresh_reg;
    bt (Asm.Const 0L) fresh_reg;
    asetnc (Reg (Real Cl));
    movq (Reg (Real Rcx)) (Reg (Fake "y"))
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* mod2 == 1 with no set destination *)
  FreshReg.reset ();
  let expr1 = ((temp "x") % (IA.const 2L)) == (IA.const 1L) in
  let expected = [
    movq (Reg (Fake "x")) fresh_reg;
    bt (Asm.Const 0L) fresh_reg;
    asetc (Reg (Real Cl));
    movq (Reg (Real Rcx)) fresh_reg;
  ]
  in
  expected === (snd (chomp_expr dummy_ctx dummy_fcontexts expr1));

  (* mod2 == 1 with set destination *)
  FreshReg.reset ();
  let stmt1 = move (temp "y") (((temp "x") % (IA.const 2L)) == (IA.const 1L)) in
  let expected = [
    movq (Reg (Fake "x")) fresh_reg;
    bt (Asm.Const 0L) fresh_reg;
    asetc (Reg (Real Cl));
    movq (Reg (Real Rcx)) (Reg (Fake "y"))
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* neg case with no set destination *)
  FreshReg.reset ();
  let expr1 = (IA.const 0L) - (temp "x") in
  let expected = [
    negq (Reg (Fake "x"))
  ]
  in
  expected === (snd (chomp_expr dummy_ctx dummy_fcontexts expr1));

  (* neg case with set destination and same as var negating *)
  FreshReg.reset ();
  let stmt1 = move (temp "x") ((IA.const 0L) - (temp "x")) in
  let expected = [
    negq (Reg (Fake "x"))
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* neg case with set destination but different from var negating *)
  FreshReg.reset ();
  let stmt1 = move (temp "y") ((IA.const 0L) - (temp "x")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    subq (Asm.Const 0L) reg0;
    movq reg0 (Reg (Fake "y"))
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* incr case with no set destination *)
  FreshReg.reset ();
  let expr1 = (IA.const 1L) + (temp "x") in
  let expected = [
    incq (Reg (Fake "x"))
  ]
  in
  expected === (snd (chomp_expr dummy_ctx dummy_fcontexts expr1));

  (* incr case with set destination and same as var negating *)
  FreshReg.reset ();
  let stmt1 = move (temp "x") ((IA.const 1L) + (temp "x")) in
  let expected = [
    incq (Reg (Fake "x"))
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* incr case with set destination but different from var negating *)
  FreshReg.reset ();
  let stmt1 = move (temp "y") ((IA.const 1L) + (temp "x")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    addq (Asm.Const 1L) reg0;
    movq reg0 (Reg (Fake "y"))
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* decr case with no set destination *)
  FreshReg.reset ();
  let expr1 = (temp "x") - (IA.const 1L) in
  let expected = [
    decq (Reg (Fake "x"))
  ]
  in
  expected === (snd (chomp_expr dummy_ctx dummy_fcontexts expr1));

  (* decr case with set destination and same as var negating *)
  FreshReg.reset ();
  let stmt1 = move (temp "x") ((temp "x") - (IA.const 1L)) in
  let expected = [
    decq (Reg (Fake "x"))
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* decr case with set destination but different from var negating *)
  FreshReg.reset ();
  let stmt1 = move (temp "y") ((temp "x") - (IA.const 1L)) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    subq (Asm.Const 1L) reg0;
    movq reg0 (Reg (Fake "y"))
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* leaq case 1 *)
  (* without destination *)
  (* c + r1 * {1,2,4,8} + r2 *)
  FreshReg.reset ();
  let expr1 = IA.const 1L + ((temp "x" * (IA.const 1L)) + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, One))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = IA.const 1L + ((temp "x" * (IA.const 2L)) + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Two))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = IA.const 1L + ((temp "x" * (IA.const 4L)) + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Four))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = IA.const 1L + ((temp "x" * (IA.const 8L)) + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Eight))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = IA.const 5L + ((temp "x" * (IA.const 1L)) + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, One))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = IA.const 5L + ((temp "x" * (IA.const 2L)) + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Two))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = IA.const 5L + ((temp "x" * (IA.const 4L)) + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Four))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = IA.const 5L + ((temp "x" * (IA.const 8L)) + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Eight))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  (* c + (r2 + r1 * {1,2,4,8}) *)
  FreshReg.reset ();
  let expr1 = IA.const 1L + ((temp "y") + (temp "x" * (IA.const 1L))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, One))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = IA.const 1L + ((temp "y") + (temp "x" * (IA.const 2L))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Two))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = IA.const 1L + ((temp "y") + (temp "x" * (IA.const 4L))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Four))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = IA.const 1L + ((temp "y") + (temp "x" * (IA.const 8L))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Eight))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = IA.const 5L + ((temp "y") + (temp "x" * (IA.const 1L))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, One))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = IA.const 5L + ((temp "y") + (temp "x" * (IA.const 2L))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Two))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = IA.const 5L + ((temp "y") + (temp "x" * (IA.const 4L))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Four))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = IA.const 5L + ((temp "y") + (temp "x" * (IA.const 8L))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Eight))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  (* (r1 * {1,2,4,8} + r2) +/- c *)
  FreshReg.reset ();
  let expr1 = ((temp "y") + (temp "x" * (IA.const 1L))) + IA.const 1L in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, One))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "y") + (temp "x" * (IA.const 2L))) + IA.const 1L in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Two))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "y") + (temp "x" * (IA.const 4L))) + IA.const 1L in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Four))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "y") + (temp "x" * (IA.const 8L))) + IA.const 1L in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Eight))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "y") + (temp "x" * (IA.const 1L))) + IA.const 5L in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, One))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "y") + (temp "x" * (IA.const 2L))) + IA.const 5L in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Two))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "y") + (temp "x" * (IA.const 4L))) + IA.const 5L in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Four))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "y") + (temp "x" * (IA.const 8L))) + IA.const 5L in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Eight))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "y") + (temp "x" * (IA.const 1L))) - IA.const 1L in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-1L), Fake fresh1, Fake fresh0, One))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "y") + (temp "x" * (IA.const 2L))) - IA.const 1L in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-1L), Fake fresh1, Fake fresh0, Two))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "y") + (temp "x" * (IA.const 4L))) - IA.const 1L in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-1L), Fake fresh1, Fake fresh0, Four))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "y") + (temp "x" * (IA.const 8L))) - IA.const 1L in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-1L), Fake fresh1, Fake fresh0, Eight))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "y") + (temp "x" * (IA.const 1L))) - IA.const 5L in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-5L), Fake fresh1, Fake fresh0, One))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "y") + (temp "x" * (IA.const 2L))) - IA.const 5L in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-5L), Fake fresh1, Fake fresh0, Two))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "y") + (temp "x" * (IA.const 4L))) - IA.const 5L in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-5L), Fake fresh1, Fake fresh0, Four))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "y") + (temp "x" * (IA.const 8L))) - IA.const 5L in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-5L), Fake fresh1, Fake fresh0, Eight))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  (* r2 + (c + r1 * {1,2,4,8}) *)
  FreshReg.reset ();
  let expr1 = temp "y" + ((IA.const 1L) + (temp "x" * (IA.const 1L))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, One))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = temp "y" + ((IA.const 1L) + (temp "x" * (IA.const 2L))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Two))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = temp "y" + ((IA.const 1L) + (temp "x" * (IA.const 4L))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Four))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = temp "y" + ((IA.const 1L) + (temp "x" * (IA.const 8L))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Eight))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = temp "y" + ((IA.const 5L) + (temp "x" * (IA.const 1L))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, One))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = temp "y" + ((IA.const 5L) + (temp "x" * (IA.const 2L))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Two))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = temp "y" + ((IA.const 5L) + (temp "x" * (IA.const 4L))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Four))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = temp "y" + ((IA.const 5L) + (temp "x" * (IA.const 8L))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Eight))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  (* (r1 * {1,2,4,8} +/- c) + r2 *)
  FreshReg.reset ();
  let expr1 = ((temp "x" * (IA.const 1L)) + IA.const 1L) + (temp "y")  in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, One))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "x" * (IA.const 2L)) + IA.const 1L) + (temp "y")  in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Two))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "x" * (IA.const 4L)) + IA.const 1L) + (temp "y")  in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Four))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "x" * (IA.const 8L)) + IA.const 1L) + (temp "y")  in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Eight))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "x" * (IA.const 1L)) + IA.const 5L) + (temp "y")  in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, One))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "x" * (IA.const 2L)) + IA.const 5L) + (temp "y")  in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Two))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "x" * (IA.const 4L)) + IA.const 5L) + (temp "y")  in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Four))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "x" * (IA.const 8L)) + IA.const 5L) + (temp "y")  in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Eight))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "x" * (IA.const 1L)) - IA.const 1L) + (temp "y")  in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-1L), Fake fresh1, Fake fresh0, One))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "x" * (IA.const 2L)) - IA.const 1L) + (temp "y")  in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-1L), Fake fresh1, Fake fresh0, Two))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "x" * (IA.const 4L)) - IA.const 1L) + (temp "y")  in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-1L), Fake fresh1, Fake fresh0, Four))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "x" * (IA.const 8L)) - IA.const 1L) + (temp "y")  in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-1L), Fake fresh1, Fake fresh0, Eight))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "x" * (IA.const 1L)) - IA.const 5L) + (temp "y")  in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-5L), Fake fresh1, Fake fresh0, One))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "x" * (IA.const 2L)) - IA.const 5L) + (temp "y")  in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-5L), Fake fresh1, Fake fresh0, Two))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "x" * (IA.const 4L)) - IA.const 5L) + (temp "y")  in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-5L), Fake fresh1, Fake fresh0, Four))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "x" * (IA.const 8L)) - IA.const 5L) + (temp "y")  in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-5L), Fake fresh1, Fake fresh0, Eight))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  (* (c + r1 * {1,2,4,8}) + r2 *)
  FreshReg.reset ();
  let expr1 = ((IA.const 1L) + (temp "x" * (IA.const 1L))) + temp "y" in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, One))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((IA.const 1L) + (temp "x" * (IA.const 2L))) + temp "y" in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Two))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((IA.const 1L) + (temp "x" * (IA.const 4L))) + temp "y" in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Four))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((IA.const 1L) + (temp "x" * (IA.const 8L))) + temp "y" in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Eight))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((IA.const 5L) + (temp "x" * (IA.const 1L))) + temp "y" in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, One))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((IA.const 5L) + (temp "x" * (IA.const 2L))) + temp "y" in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Two))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((IA.const 5L) + (temp "x" * (IA.const 4L))) + temp "y" in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Four))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((IA.const 5L) + (temp "x" * (IA.const 8L))) + temp "y" in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Eight))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  (* with destination *)
  (* c + r1 * {1,2,4,8} + r2 *)
  FreshReg.reset ();
  let stmt1 = move (temp "z") (IA.const 1L + (temp "x" * (IA.const 1L)) + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, One))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (IA.const 1L + ((temp "x" * (IA.const 2L)) + (temp "y"))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Two))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (IA.const 1L + ((temp "x" * (IA.const 4L)) + (temp "y"))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Four))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (IA.const 1L + ((temp "x" * (IA.const 8L)) + (temp "y"))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Eight))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (IA.const 5L + ((temp "x" * (IA.const 1L)) + (temp "y"))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, One))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (IA.const 5L + ((temp "x" * (IA.const 2L)) + (temp "y"))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Two))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (IA.const 5L + ((temp "x" * (IA.const 4L)) + (temp "y"))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Four))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (IA.const 5L + ((temp "x" * (IA.const 8L)) + (temp "y"))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Eight))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* c + (r2 + r1 * {1,2,4,8}) *)
  FreshReg.reset ();
  let stmt1 = move (temp "z") (IA.const 1L + ((temp "y") + (temp "x" * (IA.const 1L)))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, One))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (IA.const 1L + ((temp "y") + (temp "x" * (IA.const 2L)))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Two))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (IA.const 1L + ((temp "y") + (temp "x" * (IA.const 4L)))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Four))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (IA.const 1L + ((temp "y") + (temp "x" * (IA.const 8L)))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Eight))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (IA.const 5L + ((temp "y") + (temp "x" * (IA.const 1L)))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, One))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (IA.const 5L + ((temp "y") + (temp "x" * (IA.const 2L)))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Two))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (IA.const 5L + ((temp "y") + (temp "x" * (IA.const 4L)))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Four))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (IA.const 5L + ((temp "y") + (temp "x" * (IA.const 8L)))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Eight))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* (r1 * {1,2,4,8} + r2) +/- c *)
  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "y") + (temp "x" * (IA.const 1L))) + IA.const 1L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, One))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "y") + (temp "x" * (IA.const 2L))) + IA.const 1L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Two))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "y") + (temp "x" * (IA.const 4L))) + IA.const 1L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Four))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "y") + (temp "x" * (IA.const 8L))) + IA.const 1L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Eight))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "y") + (temp "x" * (IA.const 1L))) + IA.const 5L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, One))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "y") + (temp "x" * (IA.const 2L))) + IA.const 5L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Two))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "y") + (temp "x" * (IA.const 4L))) + IA.const 5L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Four))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "y") + (temp "x" * (IA.const 8L))) + IA.const 5L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Eight))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "y") + (temp "x" * (IA.const 1L))) - IA.const 1L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-1L), Fake fresh1, Fake fresh0, One))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "y") + (temp "x" * (IA.const 2L))) - IA.const 1L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-1L), Fake fresh1, Fake fresh0, Two))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "y") + (temp "x" * (IA.const 4L))) - IA.const 1L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-1L), Fake fresh1, Fake fresh0, Four))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "y") + (temp "x" * (IA.const 8L))) - IA.const 1L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-1L), Fake fresh1, Fake fresh0, Eight))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "y") + (temp "x" * (IA.const 1L))) - IA.const 5L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-5L), Fake fresh1, Fake fresh0, One))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "y") + (temp "x" * (IA.const 2L))) - IA.const 5L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-5L), Fake fresh1, Fake fresh0, Two))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "y") + (temp "x" * (IA.const 4L))) - IA.const 5L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-5L), Fake fresh1, Fake fresh0, Four))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "y") + (temp "x" * (IA.const 8L))) - IA.const 5L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-5L), Fake fresh1, Fake fresh0, Eight))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* r2 + (c + r1 * {1,2,4,8}) *)
  FreshReg.reset ();
  let stmt1 = move (temp "z") (temp "y" + ((IA.const 1L) + (temp "x" * (IA.const 1L)))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, One))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (temp "y" + ((IA.const 1L) + (temp "x" * (IA.const 2L)))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Two))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (temp "y" + ((IA.const 1L) + (temp "x" * (IA.const 4L)))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Four))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (temp "y" + ((IA.const 1L) + (temp "x" * (IA.const 8L)))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Eight))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (temp "y" + ((IA.const 5L) + (temp "x" * (IA.const 1L)))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, One))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (temp "y" + ((IA.const 5L) + (temp "x" * (IA.const 2L)))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Two))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (temp "y" + ((IA.const 5L) + (temp "x" * (IA.const 4L)))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Four))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (temp "y" + ((IA.const 5L) + (temp "x" * (IA.const 8L)))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Eight))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* (r1 * {1,2,4,8} +/- c) + r2 *)
  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "x" * (IA.const 1L)) + IA.const 1L) + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, One))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "x" * (IA.const 2L)) + IA.const 1L) + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Two))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "x" * (IA.const 4L)) + IA.const 1L) + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Four))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "x" * (IA.const 8L)) + IA.const 1L) + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Eight))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "x" * (IA.const 1L)) + IA.const 5L) + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, One))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "x" * (IA.const 2L)) + IA.const 5L) + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Two))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "x" * (IA.const 4L)) + IA.const 5L) + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Four))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "x" * (IA.const 8L)) + IA.const 5L) + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Eight))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "x" * (IA.const 1L)) - IA.const 1L) + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-1L), Fake fresh1, Fake fresh0, One))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "x" * (IA.const 2L)) - IA.const 1L) + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-1L), Fake fresh1, Fake fresh0, Two))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "x" * (IA.const 4L)) - IA.const 1L) + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-1L), Fake fresh1, Fake fresh0, Four))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "x" * (IA.const 8L)) - IA.const 1L) + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-1L), Fake fresh1, Fake fresh0, Eight))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "x" * (IA.const 1L)) - IA.const 5L) + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-5L), Fake fresh1, Fake fresh0, One))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "x" * (IA.const 2L)) - IA.const 5L) + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-5L), Fake fresh1, Fake fresh0, Two))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "x" * (IA.const 4L)) - IA.const 5L) + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-5L), Fake fresh1, Fake fresh0, Four))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((temp "x" * (IA.const 8L)) - IA.const 5L) + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-5L), Fake fresh1, Fake fresh0, Eight))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* (c + r1 * {1,2,4,8}) + r2 *)
  FreshReg.reset ();
  let stmt1 = move (temp "z") (((IA.const 1L) + (temp "x" * (IA.const 1L))) + temp "y") in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, One))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((IA.const 1L) + (temp "x" * (IA.const 2L))) + temp "y") in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Two))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((IA.const 1L) + (temp "x" * (IA.const 4L))) + temp "y") in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Four))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((IA.const 1L) + (temp "x" * (IA.const 8L))) + temp "y") in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh1, Fake fresh0, Eight))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((IA.const 5L) + (temp "x" * (IA.const 1L))) + temp "y") in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, One))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((IA.const 5L) + (temp "x" * (IA.const 2L))) + temp "y") in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Two))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((IA.const 5L) + (temp "x" * (IA.const 4L))) + temp "y") in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Four))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (((IA.const 5L) + (temp "x" * (IA.const 8L))) + temp "y") in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 5L, Fake fresh1, Fake fresh0, Eight))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* lea case 2 *)
  (* without a destination *)
  FreshReg.reset ();
  let expr1 = (temp "x" * (IA.const 1L)) + IA.const 1L in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (Off (Some 1L, Fake fresh0, One))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = (temp "x" * (IA.const 5L)) + IA.const 1L in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh0, Fake fresh0, Four))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = (temp "x" * (IA.const 1L)) - IA.const 1L in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (Off (Some (-1L), Fake fresh0, One))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = (temp "x" * (IA.const 5L)) - IA.const 1L in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (BaseOff (Some (-1L), Fake fresh0, Fake fresh0, Four))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((IA.const 1L) * temp "x") + IA.const 1L in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (Off (Some 1L, Fake fresh0, One))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((IA.const 5L) * temp "x") + IA.const 1L in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh0, Fake fresh0, Four))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((IA.const 1L) * temp "x") - IA.const 1L in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (Off (Some (-1L), Fake fresh0, One))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((IA.const 5L) * temp "x") - IA.const 1L in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (BaseOff (Some (-1L), Fake fresh0, Fake fresh0, Four))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = IA.const 1L + ((IA.const 1L) * temp "x") in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (Off (Some 1L, Fake fresh0, One))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = IA.const 1L + ((IA.const 5L) * temp "x") in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh0, Fake fresh0, Four))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = IA.const 1L + (temp "x" * (IA.const 1L)) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (Off (Some 1L, Fake fresh0, One))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = IA.const 1L + (temp "x" * (IA.const 5L)) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh0, Fake fresh0, Four))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  (* with destination *)
  FreshReg.reset ();
  let stmt1 = move (temp "y") ((temp "x" * (IA.const 1L)) + IA.const 1L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (Off (Some 1L, Fake fresh0, One))) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") ((temp "x" * (IA.const 5L)) + IA.const 1L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh0, Fake fresh0, Four))) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") ((temp "x" * (IA.const 1L)) - IA.const 1L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (Off (Some (-1L), Fake fresh0, One))) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") ((temp "x" * (IA.const 5L)) - IA.const 1L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (BaseOff (Some (-1L), Fake fresh0, Fake fresh0, Four))) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (((IA.const 1L) * temp "x") + IA.const 1L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (Off (Some 1L, Fake fresh0, One))) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (((IA.const 5L) * temp "x") + IA.const 1L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh0, Fake fresh0, Four))) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (((IA.const 1L) * temp "x") - IA.const 1L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (Off (Some (-1L), Fake fresh0, One))) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (((IA.const 5L) * temp "x") - IA.const 1L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (BaseOff (Some (-1L), Fake fresh0, Fake fresh0, Four))) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (IA.const 1L + ((IA.const 1L) * temp "x")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (Off (Some 1L, Fake fresh0, One))) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (IA.const 1L + ((IA.const 5L) * temp "x")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh0, Fake fresh0, Four))) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (IA.const 1L + (temp "x" * (IA.const 1L))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (Off (Some 1L, Fake fresh0, One))) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (IA.const 1L + (temp "x" * (IA.const 5L))) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh0, Fake fresh0, Four))) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* lea case 3 *)
  (* without destination *)
  FreshReg.reset ();
  let expr1 = (IA.const 1L + (temp "x" + temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh0, Fake fresh1, One))) reg1;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "x" + temp "y") + IA.const 1L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh0, Fake fresh1, One))) reg1;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "x" + temp "y") - IA.const 1L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-1L), Fake fresh0, Fake fresh1, One))) reg1;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = (temp "x" + (IA.const 1L + temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh0, Fake fresh1, One))) reg1;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((IA.const 1L + temp "x") + temp "y") in
  let expected = [
    movq (Reg (Fake "y")) reg0;
    movq (Reg (Fake "x")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh0, Fake fresh1, One))) reg1;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = (temp "x" + (temp "y" + IA.const 1L)) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh0, Fake fresh1, One))) reg1;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = (temp "x" + (temp "y" - IA.const 1L)) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-1L), Fake fresh0, Fake fresh1, One))) reg1;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "x" + IA.const 1L) + temp "y") in
  let expected = [
    movq (Reg (Fake "y")) reg0;
    movq (Reg (Fake "x")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh0, Fake fresh1, One))) reg1;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "x" - IA.const 1L) + temp "y") in
  let expected = [
    movq (Reg (Fake "y")) reg0;
    movq (Reg (Fake "x")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-1L), Fake fresh0, Fake fresh1, One))) reg1;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  (* with destination *)
  FreshReg.reset ();
  let stmt1 = move (temp "z") (IA.const 1L + (temp "x" + temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh0, Fake fresh1, One))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") ((temp "x" + temp "y") + IA.const 1L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh0, Fake fresh1, One))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") ((temp "x" + temp "y") - IA.const 1L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-1L), Fake fresh0, Fake fresh1, One))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (temp "x" + (IA.const 1L + temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh0, Fake fresh1, One))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") ((IA.const 1L + temp "x") + temp "y") in
  let expected = [
    movq (Reg (Fake "y")) reg0;
    movq (Reg (Fake "x")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh0, Fake fresh1, One))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (temp "x" + (temp "y" + IA.const 1L)) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh0, Fake fresh1, One))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (temp "x" + (temp "y" - IA.const 1L)) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-1L), Fake fresh0, Fake fresh1, One))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") ((temp "x" + IA.const 1L) + temp "y") in
  let expected = [
    movq (Reg (Fake "y")) reg0;
    movq (Reg (Fake "x")) reg1;
    leaq (Asm.Mem (BaseOff (Some 1L, Fake fresh0, Fake fresh1, One))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") ((temp "x" - IA.const 1L) + temp "y") in
  let expected = [
    movq (Reg (Fake "y")) reg0;
    movq (Reg (Fake "x")) reg1;
    leaq (Asm.Mem (BaseOff (Some (-1L), Fake fresh0, Fake fresh1, One))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* lea case 4 *)
  (* without destination *)
  FreshReg.reset ();
  let expr1 = (temp "x" + IA.const 5L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (Base (Some 5L, Fake fresh0))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = (temp "x" - IA.const 5L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (Base (Some (-5L), Fake fresh0))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = (IA.const 5L + temp "x") in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (Base (Some 5L, Fake fresh0))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  (* with destination *)
  FreshReg.reset ();
  let stmt1 = move (temp "y") (temp "x" + IA.const 5L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (Base (Some 5L, Fake fresh0))) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (temp "x" - IA.const 5L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (Base (Some (-5L), Fake fresh0))) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (IA.const 5L + temp "x") in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (Base (Some 5L, Fake fresh0))) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* lea case 5 *)
  (* without destination *)
  FreshReg.reset ();
  let expr1 = ((IA.const 2L * temp "x") + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (None, Fake fresh1, Fake fresh0, Two))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "x" * IA.const 2L) + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (None, Fake fresh1, Fake fresh0, Two))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 =  ((temp "y") + (temp "x" * IA.const 2L)) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (None, Fake fresh1, Fake fresh0, Two))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 =  ((temp "y") + (IA.const 2L * temp "x")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (None, Fake fresh1, Fake fresh0, Two))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  (* with destination *)
  FreshReg.reset ();
  let stmt1 = move (temp "z") ((IA.const 2L * temp "x") + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (None, Fake fresh1, Fake fresh0, Two))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") ((temp "x" * IA.const 2L) + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (None, Fake fresh1, Fake fresh0, Two))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") ((temp "y") + (temp "x" * IA.const 2L)) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (None, Fake fresh1, Fake fresh0, Two))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") ((temp "y") + (IA.const 2L * temp "x")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (None, Fake fresh1, Fake fresh0, Two))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* lea case 6 *)
  (* without destination *)
  FreshReg.reset ();
  let expr1 = ((temp "x") * IA.const 2L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (Off (None, Fake fresh0, Two))) reg0
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = (IA.const 2L * (temp "x")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (Off (None, Fake fresh0, Two))) reg0
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  (* with destination *)
  FreshReg.reset ();
  let stmt1 = move (temp "z") ((temp "x") * IA.const 2L) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (Off (None, Fake fresh0, Two))) (Reg (Fake "z"))
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (IA.const 2L * (temp "x")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    leaq (Asm.Mem (Off (None, Fake fresh0, Two))) (Reg (Fake "z"))
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* lea case 7 *)
  (* without destination *)
  FreshReg.reset ();
  let expr1 = ((temp "x") + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (None, Fake fresh0, Fake fresh1, One))) reg0;
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") ((temp "x") + (temp "y")) in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    movq (Reg (Fake "y")) reg1;
    leaq (Asm.Mem (BaseOff (None, Fake fresh0, Fake fresh1, One))) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* comparisons with zero *)
  (* without destination *) 
  FreshReg.reset ();
  let expr1 = ((temp "z") == IA.const 0L) in
  let expected = [
    test (Reg (Fake "z")) (Reg (Fake "z"));
    asetz (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "z"));
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "z") != IA.const 0L) in
  let expected = [
    test (Reg (Fake "z")) (Reg (Fake "z"));
    asetnz (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "z"));
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "z") < IA.const 0L) in
  let expected = [
    test (Reg (Fake "z")) (Reg (Fake "z"));
    asets (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "z"));
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "z") > IA.const 0L) in
  let expected = [
    test (Reg (Fake "z")) (Reg (Fake "z"));
    asetg (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "z"));
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "z") <= IA.const 0L) in
  let expected = [
    test (Reg (Fake "z")) (Reg (Fake "z"));
    asetle (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "z"));
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "z") >= IA.const 0L) in
  let expected = [
    test (Reg (Fake "z")) (Reg (Fake "z"));
    asetns (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "z"));
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = (IA.const 0L == (temp "z")) in
  let expected = [
    test (Reg (Fake "z")) (Reg (Fake "z"));
    asetz (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "z"));
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = (IA.const 0L != (temp "z")) in
  let expected = [
    test (Reg (Fake "z")) (Reg (Fake "z"));
    asetnz (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "z"));
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = (IA.const 0L < (temp "z")) in
  let expected = [
    test (Reg (Fake "z")) (Reg (Fake "z"));
    asetg (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "z"));
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = (IA.const 0L > (temp "z")) in
  let expected = [
    test (Reg (Fake "z")) (Reg (Fake "z"));
    asets (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "z"));
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = (IA.const 0L <= (temp "z")) in
  let expected = [
    test (Reg (Fake "z")) (Reg (Fake "z"));
    asetns (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "z"));
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = (IA.const 0L >= (temp "z")) in
  let expected = [
    test (Reg (Fake "z")) (Reg (Fake "z"));
    asetle (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "z"));
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  (* with destination *)
  FreshReg.reset ();
  let stmt1 = move (temp "x") ((temp "z") == IA.const 0L) in
  let expected = [
    test (Reg (Fake "z")) (Reg (Fake "z"));
    asetz (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") ((temp "z") != IA.const 0L) in
  let expected = [
    test (Reg (Fake "z")) (Reg (Fake "z"));
    asetnz (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") ((temp "z") < IA.const 0L) in
  let expected = [
    test (Reg (Fake "z")) (Reg (Fake "z"));
    asets (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") ((temp "z") > IA.const 0L) in
  let expected = [
    test (Reg (Fake "z")) (Reg (Fake "z"));
    asetg (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") ((temp "z") <= IA.const 0L) in
  let expected = [
    test (Reg (Fake "z")) (Reg (Fake "z"));
    asetle (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") ((temp "z") >= IA.const 0L) in
  let expected = [
    test (Reg (Fake "z")) (Reg (Fake "z"));
    asetns (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (IA.const 0L == (temp "z")) in
  let expected = [
    test (Reg (Fake "z")) (Reg (Fake "z"));
    asetz (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (IA.const 0L != (temp "z")) in
  let expected = [
    test (Reg (Fake "z")) (Reg (Fake "z"));
    asetnz (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (IA.const 0L < (temp "z")) in
  let expected = [
    test (Reg (Fake "z")) (Reg (Fake "z"));
    asetg (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (IA.const 0L > (temp "z")) in
  let expected = [
    test (Reg (Fake "z")) (Reg (Fake "z"));
    asets (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (IA.const 0L <= (temp "z")) in
  let expected = [
    test (Reg (Fake "z")) (Reg (Fake "z"));
    asetns (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (IA.const 0L >= (temp "z")) in
  let expected = [
    test (Reg (Fake "z")) (Reg (Fake "z"));
    asetle (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* binop with immediate cases *)
  (* and, or, xor *)
  (* without destination *) 
  FreshReg.reset ();
  let expr1 = ((temp "x") & IA.const 2L) in
  let expected = [
    andq (Asm.Const 2L) (Reg (Fake "x"));    
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "x") || IA.const 2L) in
  let expected = [
    orq (Asm.Const 2L) (Reg (Fake "x"));    
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = ((temp "x") ^ IA.const 2L) in
  let expected = [
    xorq (Asm.Const 2L) (Reg (Fake "x"));    
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = (IA.const 2L & (temp "x")) in
  let expected = [
    andq (Asm.Const 2L) (Reg (Fake "x"));    
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset (); let expr1 = (IA.const 2L || (temp "x")) in
  let expected = [
    orq (Asm.Const 2L) (Reg (Fake "x"));    
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = (IA.const 2L ^ (temp "x")) in
  let expected = [
    xorq (Asm.Const 2L) (Reg (Fake "x"));    
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  (* with destination but does not match *) 
  FreshReg.reset ();
  let stmt1 = move (temp "y") ((temp "x") & IA.const 2L) in
  let expected = [
    andq (Asm.Const 2L) (Reg (Fake "x"));    
    movq (Reg (Fake "x")) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") ((temp "x") || IA.const 2L) in
  let expected = [
    orq (Asm.Const 2L) (Reg (Fake "x"));    
    movq (Reg (Fake "x")) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") ((temp "x") ^ IA.const 2L) in
  let expected = [
    xorq (Asm.Const 2L) (Reg (Fake "x"));    
    movq (Reg (Fake "x")) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (IA.const 2L & (temp "x")) in
  let expected = [
    andq (Asm.Const 2L) (Reg (Fake "x"));    
    movq (Reg (Fake "x")) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (IA.const 2L || (temp "x")) in
  let expected = [
    orq (Asm.Const 2L) (Reg (Fake "x"));    
    movq (Reg (Fake "x")) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (IA.const 2L ^ (temp "x")) in
  let expected = [
    xorq (Asm.Const 2L) (Reg (Fake "x"));    
    movq (Reg (Fake "x")) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* with destination and does match *) 
  FreshReg.reset ();
  let stmt1 = move (temp "x") ((temp "x") & IA.const 2L) in
  let expected = [
    andq (Asm.Const 2L) (Reg (Fake "x"));    
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") ((temp "x") || IA.const 2L) in
  let expected = [
    orq (Asm.Const 2L) (Reg (Fake "x"));    
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") ((temp "x") ^ IA.const 2L) in
  let expected = [
    xorq (Asm.Const 2L) (Reg (Fake "x"));    
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (IA.const 2L & (temp "x")) in
  let expected = [
    andq (Asm.Const 2L) (Reg (Fake "x"));    
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (IA.const 2L || (temp "x")) in
  let expected = [
    orq (Asm.Const 2L) (Reg (Fake "x"));    
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (IA.const 2L ^ (temp "x")) in
  let expected = [
    xorq (Asm.Const 2L) (Reg (Fake "x"));    
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* shifts *)
  (* without destination *)
  FreshReg.reset ();
  let expr1 = (temp "x" << IA.const 2L) in  
  let expected = [
    shlq (Asm.Const 2L) (Reg (Fake "x")); 
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = (temp "x" >> IA.const 2L) in  
  let expected = [
    shrq (Asm.Const 2L) (Reg (Fake "x")); 
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = (temp "x" >>> IA.const 2L) in  
  let expected = [
    sarq (Asm.Const 2L) (Reg (Fake "x")); 
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  (* with destination but does not match variable *)
  FreshReg.reset ();
  let stmt1 = move (temp "y") (temp "x" << IA.const 2L) in  
  let expected = [
    shlq (Asm.Const 2L) (Reg (Fake "x")); 
    movq (Reg (Fake "x")) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (temp "x" >> IA.const 2L) in  
  let expected = [
    shrq (Asm.Const 2L) (Reg (Fake "x")); 
    movq (Reg (Fake "x")) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (temp "x" >>> IA.const 2L) in  
  let expected = [
    sarq (Asm.Const 2L) (Reg (Fake "x")); 
    movq (Reg (Fake "x")) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* with destination and matches variable *)
  FreshReg.reset ();
  let stmt1 = move (temp "x") (temp "x" << IA.const 2L) in  
  let expected = [
    shlq (Asm.Const 2L) (Reg (Fake "x")); 
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (temp "x" >> IA.const 2L) in  
  let expected = [
    shrq (Asm.Const 2L) (Reg (Fake "x")); 
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (temp "x" >>> IA.const 2L) in  
  let expected = [
    sarq (Asm.Const 2L) (Reg (Fake "x")); 
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* eq, neq, lt, gt, leq, geq cases *)
  (* without destination *)
  FreshReg.reset ();
  let expr1 = (temp "x" == IA.const 2L) in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asete (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = (temp "x" != IA.const 2L) in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asetne (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = (temp "x" < IA.const 2L) in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asetl (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = (temp "x" > IA.const 2L) in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asetg (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = (temp "x" <= IA.const 2L) in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asetle (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = (temp "x" >= IA.const 2L) in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asetge (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);
  
  (* order flipped *)
  FreshReg.reset ();
  let expr1 = (IA.const 2L == temp "x") in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asete (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = (IA.const 2L != temp "x") in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asetne (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = (IA.const 2L < temp "x") in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asetg (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = (IA.const 2L > temp "x") in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asetl (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = (IA.const 2L <= temp "x") in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asetge (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = (IA.const 2L >= temp "x") in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asetle (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  (* with destination but does not match variable *)
  FreshReg.reset ();
  let stmt1 = move (temp "y") (temp "x" == IA.const 2L) in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asete (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (temp "x" != IA.const 2L) in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asetne (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (temp "x" < IA.const 2L) in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asetl (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (temp "x" > IA.const 2L) in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asetg (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (temp "x" <= IA.const 2L) in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asetle (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (temp "x" >= IA.const 2L) in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asetge (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);
  
  (* order flipped *)
  FreshReg.reset ();
  let stmt1 = move (temp "y") (IA.const 2L == temp "x") in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asete (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (IA.const 2L != temp "x") in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asetne (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (IA.const 2L < temp "x") in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asetg (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (IA.const 2L > temp "x") in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asetl (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (IA.const 2L <= temp "x") in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asetge (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (IA.const 2L >= temp "x") in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asetle (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* with destination and does match variable *)
  FreshReg.reset ();
  let stmt1 = move (temp "x") (temp "x" == IA.const 2L) in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asete (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (temp "x" != IA.const 2L) in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asetne (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (temp "x" < IA.const 2L) in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asetl (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (temp "x" > IA.const 2L) in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asetg (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (temp "x" <= IA.const 2L) in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asetle (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (temp "x" >= IA.const 2L) in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asetge (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);
  
  (* order flipped *)
  FreshReg.reset ();
  let stmt1 = move (temp "x") (IA.const 2L == temp "x") in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asete (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (IA.const 2L != temp "x") in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asetne (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (IA.const 2L < temp "x") in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asetg (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (IA.const 2L > temp "x") in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asetl (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (IA.const 2L <= temp "x") in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asetge (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (IA.const 2L >= temp "x") in
  let expected = [
    cmpq (Asm.Const 2L) (Reg (Fake "x"));
    asetle (Reg (Real Cl)); 
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* basic binops *)
  (* without destination *)
  FreshReg.reset ();
  let expr1 = temp "x" - temp "y" in
  let expected = [
    subq (Reg (Fake "y")) (Reg (Fake "x"));      
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  let expr1 = temp "x" & temp "y" in
  let expected = [
    andq (Reg (Fake "y")) (Reg (Fake "x"));      
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  let expr1 = temp "x" || temp "y" in
  let expected = [
    orq (Reg (Fake "y")) (Reg (Fake "x"));      
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  let expr1 = temp "x" ^ temp "y" in
  let expected = [
    xorq (Reg (Fake "y")) (Reg (Fake "x"));      
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  let expr1 = temp "x" << temp "y" in
  let expected = [
    movq (Reg (Fake "y")) (Reg (Real Rcx));
    shlq (Reg (Real Cl)) (Reg (Fake "x"));      
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  let expr1 = temp "x" >> temp "y" in
  let expected = [
    movq (Reg (Fake "y")) (Reg (Real Rcx));
    shrq (Reg (Real Cl)) (Reg (Fake "x"));      
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  let expr1 = temp "x" >>> temp "y" in
  let expected = [
    movq (Reg (Fake "y")) (Reg (Real Rcx));
    sarq (Reg (Real Cl)) (Reg (Fake "x"));      
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = temp "x" == temp "y" in
  let expected = [
    cmpq (Reg (Fake "y")) (Reg (Fake "x"));
    asete (Reg (Real Cl));
    movq (Reg (Real Rcx)) (Reg (Fake "y"));
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = temp "x" != temp "y" in
  let expected = [
    cmpq (Reg (Fake "y")) (Reg (Fake "x"));
    asetne (Reg (Real Cl));
    movq (Reg (Real Rcx)) (Reg (Fake "y"));
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = temp "x" < temp "y" in
  let expected = [
    cmpq (Reg (Fake "y")) (Reg (Fake "x"));
    asetl (Reg (Real Cl));
    movq (Reg (Real Rcx)) (Reg (Fake "y"));
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = temp "x" > temp "y" in
  let expected = [
    cmpq (Reg (Fake "y")) (Reg (Fake "x"));
    asetg (Reg (Real Cl));
    movq (Reg (Real Rcx)) (Reg (Fake "y"));
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = temp "x" <= temp "y" in
  let expected = [
    cmpq (Reg (Fake "y")) (Reg (Fake "x"));
    asetle (Reg (Real Cl));
    movq (Reg (Real Rcx)) (Reg (Fake "y"));
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = temp "x" >= temp "y" in
  let expected = [
    cmpq (Reg (Fake "y")) (Reg (Fake "x"));
    asetge (Reg (Real Cl));
    movq (Reg (Real Rcx)) (Reg (Fake "y"));
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = temp "x" * temp "y" in
  let expected = [
    movq (Reg (Fake "y")) (Reg (Real Rax));
    imulq (Reg (Fake "x"));
    movq (Reg (Real Rax)) (Reg (Fake "y"))
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = temp "x" *>> temp "y" in
  let expected = [
    movq (Reg (Fake "y")) (Reg (Real Rax));
    imulq (Reg (Fake "x"));
    movq (Reg (Real Rdx)) (Reg (Fake "y"))
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = temp "x" / temp "y" in
  let expected = [
    movq (Reg (Fake "x")) (Reg (Real Rax));
    idivq (Reg (Fake "y"));
    movq (Reg (Real Rax)) (Reg (Fake "y"))
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  FreshReg.reset ();
  let expr1 = temp "x" % temp "y" in
  let expected = [
    movq (Reg (Fake "x")) (Reg (Real Rax));
    idivq (Reg (Fake "y"));
    movq (Reg (Real Rdx)) (Reg (Fake "y"))
  ]
  in
  expected === snd(chomp_expr dummy_ctx dummy_fcontexts expr1);

  (* with destination but does not match variables *)
  FreshReg.reset ();
  let stmt1 = move (temp "z") (temp "x" - temp "y") in
  let expected = [
    subq (Reg (Fake "y")) (Reg (Fake "x"));      
    movq (Reg (Fake "x")) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  let stmt1 = move (temp "z") (temp "x" & temp "y") in
  let expected = [
    andq (Reg (Fake "y")) (Reg (Fake "x"));      
    movq (Reg (Fake "x")) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  let stmt1 = move (temp "z") (temp "x" || temp "y") in
  let expected = [
    orq (Reg (Fake "y")) (Reg (Fake "x"));      
    movq (Reg (Fake "x")) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  let stmt1 = move (temp "z") (temp "x" ^ temp "y") in
  let expected = [
    xorq (Reg (Fake "y")) (Reg (Fake "x"));      
    movq (Reg (Fake "x")) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  let stmt1 = move (temp "z") (temp "x" << temp "y") in
  let expected = [
    movq (Reg (Fake "y")) (Reg (Real Rcx));
    shlq (Reg (Real Cl)) (Reg (Fake "x"));      
    movq (Reg (Fake "x")) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  let stmt1 = move (temp "z") (temp "x" >> temp "y") in
  let expected = [
    movq (Reg (Fake "y")) (Reg (Real Rcx));
    shrq (Reg (Real Cl)) (Reg (Fake "x"));      
    movq (Reg (Fake "x")) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  let stmt1 = move (temp "z") (temp "x" >>> temp "y") in
  let expected = [
    movq (Reg (Fake "y")) (Reg (Real Rcx));
    sarq (Reg (Real Cl)) (Reg (Fake "x"));      
    movq (Reg (Fake "x")) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (temp "x" == temp "y") in
  let expected = [
    cmpq (Reg (Fake "y")) (Reg (Fake "x"));
    asete (Reg (Real Cl));
    movq (Reg (Real Rcx)) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (temp "x" != temp "y") in
  let expected = [
    cmpq (Reg (Fake "y")) (Reg (Fake "x"));
    asetne (Reg (Real Cl));
    movq (Reg (Real Rcx)) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (temp "x" < temp "y") in
  let expected = [
    cmpq (Reg (Fake "y")) (Reg (Fake "x"));
    asetl (Reg (Real Cl));
    movq (Reg (Real Rcx)) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (temp "x" > temp "y") in
  let expected = [
    cmpq (Reg (Fake "y")) (Reg (Fake "x"));
    asetg (Reg (Real Cl));
    movq (Reg (Real Rcx)) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (temp "x" <= temp "y") in
  let expected = [
    cmpq (Reg (Fake "y")) (Reg (Fake "x"));
    asetle (Reg (Real Cl));
    movq (Reg (Real Rcx)) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (temp "x" >= temp "y") in
  let expected = [
    cmpq (Reg (Fake "y")) (Reg (Fake "x"));
    asetge (Reg (Real Cl));
    movq (Reg (Real Rcx)) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (temp "x" * temp "y") in
  let expected = [
    movq (Reg (Fake "y")) (Reg (Real Rax));
    imulq (Reg (Fake "x"));
    movq (Reg (Real Rax)) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (temp "x" *>> temp "y") in
  let expected = [
    movq (Reg (Fake "y")) (Reg (Real Rax));
    imulq (Reg (Fake "x"));
    movq (Reg (Real Rdx)) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (temp "x" / temp "y") in
  let expected = [
    movq (Reg (Fake "x")) (Reg (Real Rax));
    idivq (Reg (Fake "y"));
    movq (Reg (Real Rax)) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "z") (temp "x" % temp "y") in
  let expected = [
    movq (Reg (Fake "x")) (Reg (Real Rax));
    idivq (Reg (Fake "y"));
    movq (Reg (Real Rdx)) (Reg (Fake "z"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* with destination and does match variables *)
  FreshReg.reset ();
  let stmt1 = move (temp "x") (temp "x" - temp "y") in
  let expected = [
    subq (Reg (Fake "y")) (Reg (Fake "x"));      
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  let stmt1 = move (temp "x") (temp "x" & temp "y") in
  let expected = [
    andq (Reg (Fake "y")) (Reg (Fake "x"));      
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  let stmt1 = move (temp "x") (temp "x" || temp "y") in
  let expected = [
    orq (Reg (Fake "y")) (Reg (Fake "x"));      
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  let stmt1 = move (temp "x") (temp "x" ^ temp "y") in
  let expected = [
    xorq (Reg (Fake "y")) (Reg (Fake "x"));      
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  let stmt1 = move (temp "x") (temp "x" << temp "y") in
  let expected = [
    movq (Reg (Fake "y")) (Reg (Real Rcx));
    shlq (Reg (Real Cl)) (Reg (Fake "x"));      
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  let stmt1 = move (temp "x") (temp "x" >> temp "y") in
  let expected = [
    movq (Reg (Fake "y")) (Reg (Real Rcx));
    shrq (Reg (Real Cl)) (Reg (Fake "x"));      
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  let stmt1 = move (temp "x") (temp "x" >>> temp "y") in
  let expected = [
    movq (Reg (Fake "y")) (Reg (Real Rcx));
    sarq (Reg (Real Cl)) (Reg (Fake "x"));      
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (temp "x" == temp "y") in
  let expected = [
    cmpq (Reg (Fake "y")) (Reg (Fake "x"));
    asete (Reg (Real Cl));
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (temp "x" != temp "y") in
  let expected = [
    cmpq (Reg (Fake "y")) (Reg (Fake "x"));
    asetne (Reg (Real Cl));
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (temp "x" < temp "y") in
  let expected = [
    cmpq (Reg (Fake "y")) (Reg (Fake "x"));
    asetl (Reg (Real Cl));
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (temp "x" > temp "y") in
  let expected = [
    cmpq (Reg (Fake "y")) (Reg (Fake "x"));
    asetg (Reg (Real Cl));
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (temp "x" <= temp "y") in
  let expected = [
    cmpq (Reg (Fake "y")) (Reg (Fake "x"));
    asetle (Reg (Real Cl));
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (temp "x" >= temp "y") in
  let expected = [
    cmpq (Reg (Fake "y")) (Reg (Fake "x"));
    asetge (Reg (Real Cl));
    movq (Reg (Real Rcx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (temp "x" * temp "y") in
  let expected = [
    movq (Reg (Fake "y")) (Reg (Real Rax));
    imulq (Reg (Fake "x"));
    movq (Reg (Real Rax)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (temp "x" *>> temp "y") in
  let expected = [
    movq (Reg (Fake "y")) (Reg (Real Rax));
    imulq (Reg (Fake "x"));
    movq (Reg (Real Rdx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (temp "x" / temp "y") in
  let expected = [
    movq (Reg (Fake "x")) (Reg (Real Rax));
    idivq (Reg (Fake "y"));
    movq (Reg (Real Rax)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "x") (temp "x" % temp "y") in
  let expected = [
    movq (Reg (Fake "x")) (Reg (Real Rax));
    idivq (Reg (Fake "y"));
    movq (Reg (Real Rdx)) (Reg (Fake "x"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* with destination and does match variables *)
  (* flipped case *)
  FreshReg.reset ();
  let stmt1 = move (temp "y") (temp "x" - temp "y") in
  let expected = [
    subq (Reg (Fake "y")) (Reg (Fake "x"));      
    movq (Reg (Fake "x")) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  let stmt1 = move (temp "y") (temp "x" & temp "y") in
  let expected = [
    andq (Reg (Fake "x")) (Reg (Fake "y"));      
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  let stmt1 = move (temp "y") (temp "x" || temp "y") in
  let expected = [
    orq (Reg (Fake "x")) (Reg (Fake "y"));      
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  let stmt1 = move (temp "y") (temp "x" ^ temp "y") in
  let expected = [
    xorq (Reg (Fake "x")) (Reg (Fake "y"));      
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  let stmt1 = move (temp "y") (temp "x" << temp "y") in
  let expected = [
    movq (Reg (Fake "y")) (Reg (Real Rcx));
    shlq (Reg (Real Cl)) (Reg (Fake "x"));      
    movq (Reg (Fake "x")) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  let stmt1 = move (temp "y") (temp "x" >> temp "y") in
  let expected = [
    movq (Reg (Fake "y")) (Reg (Real Rcx));
    shrq (Reg (Real Cl)) (Reg (Fake "x"));      
    movq (Reg (Fake "x")) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  let stmt1 = move (temp "y") (temp "x" >>> temp "y") in
  let expected = [
    movq (Reg (Fake "y")) (Reg (Real Rcx));
    sarq (Reg (Real Cl)) (Reg (Fake "x"));      
    movq (Reg (Fake "x")) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (temp "x" == temp "y") in
  let expected = [
    cmpq (Reg (Fake "y")) (Reg (Fake "x"));
    asete (Reg (Real Cl));
    movq (Reg (Real Rcx)) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (temp "x" != temp "y") in
  let expected = [
    cmpq (Reg (Fake "y")) (Reg (Fake "x"));
    asetne (Reg (Real Cl));
    movq (Reg (Real Rcx)) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (temp "x" < temp "y") in
  let expected = [
    cmpq (Reg (Fake "y")) (Reg (Fake "x"));
    asetl (Reg (Real Cl));
    movq (Reg (Real Rcx)) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (temp "x" > temp "y") in
  let expected = [
    cmpq (Reg (Fake "y")) (Reg (Fake "x"));
    asetg (Reg (Real Cl));
    movq (Reg (Real Rcx)) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (temp "x" <= temp "y") in
  let expected = [
    cmpq (Reg (Fake "y")) (Reg (Fake "x"));
    asetle (Reg (Real Cl));
    movq (Reg (Real Rcx)) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (temp "x" >= temp "y") in
  let expected = [
    cmpq (Reg (Fake "y")) (Reg (Fake "x"));
    asetge (Reg (Real Cl));
    movq (Reg (Real Rcx)) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (temp "x" * temp "y") in
  let expected = [
    movq (Reg (Fake "y")) (Reg (Real Rax));
    imulq (Reg (Fake "x"));
    movq (Reg (Real Rax)) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (temp "x" *>> temp "y") in
  let expected = [
    movq (Reg (Fake "y")) (Reg (Real Rax));
    imulq (Reg (Fake "x"));
    movq (Reg (Real Rdx)) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (temp "x" / temp "y") in
  let expected = [
    movq (Reg (Fake "x")) (Reg (Real Rax));
    idivq (Reg (Fake "y"));
    movq (Reg (Real Rax)) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let stmt1 = move (temp "y") (temp "x" % temp "y") in
  let expected = [
    movq (Reg (Fake "x")) (Reg (Real Rax));
    idivq (Reg (Fake "y"));
    movq (Reg (Real Rdx)) (Reg (Fake "y"));
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* chomp_stmt tests, excluding move w/ binop on rhs *)
  (* cjump *)
  (* mod2 *)
  FreshReg.reset ();
  let e1 = (temp "x") % (IA.const 2L) == (IA.const 0L) in
  let stmt1 = cjumpone e1 "tru" in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    bt (Asm.Const 0L) reg0;
    jnc (Asm.Label "tru");
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let e1 = (IA.const 0L) == (temp "x") % (IA.const 2L) in
  let stmt1 = cjumpone e1 "tru" in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    bt (Asm.Const 0L) reg0;
    jnc (Asm.Label "tru");
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let e1 = (IA.const 1L) == (temp "x") % (IA.const 2L) in
  let stmt1 = cjumpone e1 "tru" in let expected = [
    movq (Reg (Fake "x")) reg0;
    bt (Asm.Const 0L) reg0;
    jc (Asm.Label "tru");
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let e1 = (temp "x") % (IA.const 2L) == (IA.const 1L) in
  let stmt1 = cjumpone e1 "tru" in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    bt (Asm.Const 0L) reg0;
    jc (Asm.Label "tru");
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* comparing with 0 *)
  FreshReg.reset ();
  let e1 = (temp "x") == (IA.const 0L) in
  let stmt1 = cjumpone e1 "tru" in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    test reg0 reg0;
    jz (Asm.Label "tru");
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let e1 = (IA.const 0L) == (temp "x") in
  let stmt1 = cjumpone e1 "tru" in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    test reg0 reg0;
    jz (Asm.Label "tru");
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let e1 = (temp "x") != (IA.const 0L) in
  let stmt1 = cjumpone e1 "tru" in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    test reg0 reg0;
    jnz (Asm.Label "tru");
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let e1 = (IA.const 0L) != (temp "x") in
  let stmt1 = cjumpone e1 "tru" in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    test reg0 reg0;
    jnz (Asm.Label "tru");
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let e1 = (temp "x") < (IA.const 0L) in
  let stmt1 = cjumpone e1 "tru" in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    test reg0 reg0;
    js (Asm.Label "tru");
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let e1 = (IA.const 0L) > (temp "x") in
  let stmt1 = cjumpone e1 "tru" in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    test reg0 reg0;
    js (Asm.Label "tru");
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let e1 = (temp "x") > (IA.const 0L) in
  let stmt1 = cjumpone e1 "tru" in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    test reg0 reg0;
    jg (Asm.Label "tru");
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let e1 = (IA.const 0L) < (temp "x") in
  let stmt1 = cjumpone e1 "tru" in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    test reg0 reg0;
    jg (Asm.Label "tru");
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let e1 = (temp "x") <= (IA.const 0L) in
  let stmt1 = cjumpone e1 "tru" in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    test reg0 reg0;
    jle (Asm.Label "tru");
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let e1 = (IA.const 0L) >= (temp "x") in
  let stmt1 = cjumpone e1 "tru" in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    test reg0 reg0;
    jle (Asm.Label "tru");
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let e1 = (temp "x") >= (IA.const 0L) in
  let stmt1 = cjumpone e1 "tru" in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    test reg0 reg0;
    jns (Asm.Label "tru");
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  FreshReg.reset ();
  let e1 = (IA.const 0L) <= (temp "x") in
  let stmt1 = cjumpone e1 "tru" in
  let expected = [
    movq (Reg (Fake "x")) reg0;
    test reg0 reg0;
    jns (Asm.Label "tru");
  ]
  in
  expected === (chomp_stmt dummy_ctx dummy_fcontexts stmt1);

  (* comparing with constant *)

  (* jump *)
  (* exp *)
  (* label *)
  (* move temp e, where e is not binop *)
  (* move mem e *)
  (* seq *)
  (* return *)
  ()

let test_register_allocation _ =
  let open Asm in
  let open Asm.Abbreviations in
  let open AsmsEq in
  let open Tiling in

  let input = [] in
  let expected = [] in
  expected === register_allocate input;

  let input = [movq arax arbx] in
  let expected = [movq rax rbx] in
  expected === register_allocate input;

  let input = [movq x arbx] in
  let expected = [
    movq (-8L $ mrbp) r13;
    movq r13 rbx;
    movq r13 (-8L $ mrbp);
  ] in
  expected === register_allocate input;

  let input = [movq x y] in
  let expected = [
    movq (-8L $ mrbp) r13;
    movq (-16L $ mrbp) r14;
    movq r13 r14;
    movq r13 (-8L $ mrbp);
    movq r14 (-16L $ mrbp);
  ] in
  expected === register_allocate input;

  let input = [
    movq x y;
    movq z x;
  ] in
  let expected = [
    movq (-8L $ mrbp) r13;
    movq (-16L $ mrbp) r14;
    movq r13 r14;
    movq r13 (-8L $ mrbp);
    movq r14 (-16L $ mrbp);
    movq (-24L $ mrbp) r13;
    movq (-8L $ mrbp) r14;
    movq r13 r14;
    movq r13 (-24L $ mrbp);
    movq r14 (-8L $ mrbp);
  ] in
  expected === register_allocate input;

  let input = [
    push arbp;
    movq arsp arbp;
    movq x y;
    andq z x;
    push a;
    leave;
    ret;
  ] in
  let expected = [
    push rbp;
    movq rsp rbp;
    movq (-8L $ mrbp) r13;
    movq (-16L $ mrbp) r14;
    movq r13 r14;
    movq r13 (-8L $ mrbp);
    movq r14 (-16L $ mrbp);
    movq (-24L $ mrbp) r13;
    movq (-8L $ mrbp) r14;
    andq r13 r14;
    movq r13 (-24L $ mrbp);
    movq r14 (-8L $ mrbp);
    movq (-32L $ mrbp) r13;
    push r13;
    movq r13 (-32L $ mrbp);
    leave;
    ret;
  ] in
  expected === register_allocate input;

  let input = [Lab "foo"] in
  let expected = [Lab "foo"] in
  expected === register_allocate input;

  let input = [
    Directive ("align", ["4"]);
    Directive ("globl", ["foo"]);
  ] in
  let expected = [
    Directive ("align", ["4"]);
    Directive ("globl", ["foo"]);
  ] in
  expected === register_allocate input;

  ()

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* ! DON'T FORGET TO ADD YOUR TESTS HERE                                     ! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let main () =
    "suite" >::: [
      "test_munch_expr"          >:: test_munch_expr;
      "test_munch_stmt"          >:: test_munch_stmt;
      "test_munch_func_decl"     >:: test_munch_func_decl;
      "test_chomp"               >:: test_chomp;
      "test_register_allocation" >:: test_register_allocation;
    ] |> run_test_tt_main

let _ = main ()
