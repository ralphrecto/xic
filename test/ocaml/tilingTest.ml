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
  let (===) ((r1, a1): (Asm.abstract_reg * Asm.abstract_asm list))
            ((r2, a2): (Asm.abstract_reg * Asm.abstract_asm list)) : unit =
    let printer (r, a) =
      sprintf "\n%s\n%s\n" (Asm.string_of_abstract_reg r)
                           (Asm.string_of_abstract_asms a)
    in
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

  let munch_expr = munch_expr dummy_ctx dummy_fcontexts in
  let fakereg i = Fake (FreshReg.gen i) in
  let fakeop i = fake (FreshReg.gen i) in

  let test expected input =
    FreshReg.reset ();
    expected === munch_expr input;
  in

  (* basic temps *)
  let input = temp "foo" in
  let expected = ((fakereg 0), [
    movq (fake "foo") (fakeop 0)
  ]) in
  test expected input;

  let input = temp "bar" in
  let expected = ((fakereg 0), [
    movq (fake "bar") (fakeop 0)
  ]) in
  test expected input;

  (* consts *)
  let input = three in
  let expected = ((fakereg 0), [
    movq (const 3) (fakeop 0)
  ]) in
  test expected input;

  (* binops *)
  let simple_binop_test irop asmop =
    let input = irop one two in
    let expected = ((fakereg 0), [
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

  let shift_binop_test irop asmop =
    let input = irop one two in
    let expected = ((fakereg 0), [
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
  let reg0 = Reg (Fake (FreshReg.fresh ())) in
  let _reg1 = Reg (Fake (FreshReg.fresh ())) in
  let _reg2 = Reg (Fake (FreshReg.fresh ())) in
  let _reg3 = Reg (Fake (FreshReg.fresh ())) in
  let _reg4 = Reg (Fake (FreshReg.fresh ())) in
  let _reg5 = Reg (Fake (FreshReg.fresh ())) in
  let _reg6 = Reg (Fake (FreshReg.fresh ())) in
  let _reg7 = Reg (Fake (FreshReg.fresh ())) in
  let _reg8 = Reg (Fake (FreshReg.fresh ())) in
  let _reg9 = Reg (Fake (FreshReg.fresh ())) in
  let _reg10 = Reg (Fake (FreshReg.fresh ())) in

  (* mod2 == 0 with no set destination *)
  FreshReg.reset ();
  let expr1 = ((temp "x") % (IA.const 2L)) == (IA.const 0L) in
  let fresh_reg = reg0 in
  let expected = [
    movq (Reg (Fake "x")) fresh_reg;
    bt (Asm.Const 0L) fresh_reg;
    setnc fresh_reg
  ]
  in
  let result = snd (chomp_expr dummy_ctx dummy_fcontexts expr1) in
  expected === result;

  (* mod2 == 0 with set destination *)
  FreshReg.reset ();
  let stmt1 = move (temp "y") (((temp "x") % (IA.const 2L)) == (IA.const 0L)) in
  let fresh_reg = reg0 in
  let expected = [
    movq (Reg (Fake "x")) fresh_reg;
    bt (Asm.Const 0L) fresh_reg;
    setnc (Reg (Fake "y"))
  ]
  in
  let result = chomp_stmt dummy_ctx dummy_fcontexts stmt1 in
  expected === result;

  (* mod2 == 1 with no set destination *)
  FreshReg.reset ();
  let expr1 = ((temp "x") % (IA.const 2L)) == (IA.const 1L) in
  let fresh_reg = reg0 in
  let expected = [
    movq (Reg (Fake "x")) fresh_reg;
    bt (Asm.Const 0L) fresh_reg;
    setc fresh_reg
  ]
  in
  let result = snd (chomp_expr dummy_ctx dummy_fcontexts expr1) in
  expected === result;

  (* mod2 == 1 with set destination *)
  FreshReg.reset ();
  let stmt1 = move (temp "y") (((temp "x") % (IA.const 2L)) == (IA.const 1L)) in
  let fresh_reg = reg0 in
  let expected = [
    movq (Reg (Fake "x")) fresh_reg;
    bt (Asm.Const 0L) fresh_reg;
    setc (Reg (Fake "y"))
  ]
  in
  let result = chomp_stmt dummy_ctx dummy_fcontexts stmt1 in
  expected === result;

  (* neg case with no set destination *)

  (* neg case with set destination and same as var negating *)

  (* neg case with set destination but different from var negating *)

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
      "test_chomp"               >:: test_chomp;
      "test_register_allocation" >:: test_register_allocation;
    ] |> run_test_tt_main

let _ = main ()
