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
  let ret_temp_test max_args i asmoperand =
    let fc = Func_context.({dummy_ctx with max_args}) in
    let input = temp (Ir_generation.FreshRetReg.gen i) in
    let expected = ((gen 0), [
      movq asmoperand (fakeop 0)
    ]) in
    test ~ctx:fc expected input;
  in

  ret_temp_test 0 0 arax;
  ret_temp_test 0 1 ardx;
  ret_temp_test 0 2 (0L $ mem_rsp);
  ret_temp_test 0 3 (8L $ mem_rsp);
  ret_temp_test 0 4 (16L $ mem_rsp);

  ret_temp_test 1 0 arax;
  ret_temp_test 1 1 ardx;
  ret_temp_test 1 2 (8L $ mem_rsp);
  ret_temp_test 1 3 (16L $ mem_rsp);
  ret_temp_test 1 4 (24L $ mem_rsp);

  ret_temp_test 2 0 arax;
  ret_temp_test 2 1 ardx;
  ret_temp_test 2 2 (16L $ mem_rsp);
  ret_temp_test 2 3 (24L $ mem_rsp);
  ret_temp_test 2 4 (32L $ mem_rsp);

  ret_temp_test 3 0 arax;
  ret_temp_test 3 1 ardx;
  ret_temp_test 3 2 (24L $ mem_rsp);
  ret_temp_test 3 3 (32L $ mem_rsp);
  ret_temp_test 3 4 (40L $ mem_rsp);

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
  (* TODO: *)
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
  (* let fakereg i = Fake (gen i) in *)
  let fakeop i = fake (gen i) in
  (* let mem_rsp = Asm.Mem (Base (None, Real Rsp)) in *)
  (* let mem_rbp = Asm.Mem (Base (None, Real Rbp)) in *)

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

  (* label *)

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
      "test_chomp"               >:: test_chomp;
      "test_register_allocation" >:: test_register_allocation;
    ] |> run_test_tt_main

let _ = main ()
