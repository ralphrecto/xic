open Core.Std
open OUnit
open TestUtil
open Util

module AsmsEq = struct
  let (===) (a: Asm.asm list) (b: Asm.asm list) : unit =
    assert_equal ~printer:(fun a -> "\n" ^ Asm.string_of_asms a ^ "\n") a b
end

module Dummy = struct
  open Func_context
  let dummy_ctx = {num_args = 0; num_rets = 0; max_args = 0; max_rets = 0;}
  let dummy_fcontexts = String.Map.empty
end

(*
 * there isn't a separate chomp_expr test
 * because the cases other than binop are identical to munch
 *)
let test_chomp_binop () =
  let open Ir.Abbreviations in
  let open Ir in
  let open Asm in
  let open AsmsEq in
  let open Tiling in
  let module IA = Ir.Abbreviations in

  (* mod2 == 0 with no set destination *)
  FreshReg.reset ();
  let _mod2_cmp_0 = eq_ (mod_ (temp "x") (IA.const 2L)) (IA.const 0L) in
  let fresh_reg = Reg (Fake (FreshReg.fresh ())) in
  let _expected = [
    mov (Reg (Fake "x")) fresh_reg;
    bt (Asm.Const 0L) fresh_reg;
    setnc fresh_reg
  ]
  in
  (* mod2 == 0 with set destination *)

 ()

let test_register_allocation () =
  let open Asm in
  let open Asm.Abbreviations in
  let open AsmsEq in
  let open Tiling in

  let input = [] in
  let expected = [] in
  expected === register_allocate input;

  let input = [mov arax arbx] in
  let expected = [mov rax rbx] in
  expected === register_allocate input;

  let input = [mov x arbx] in
  let expected = [
    mov (-8L $ mrbp) r13;
    mov r13 rbx;
    mov r13 (-8L $ mrbp);
  ] in
  expected === register_allocate input;

  let input = [mov x y] in
  let expected = [
    mov (-8L $ mrbp) r13;
    mov (-16L $ mrbp) r14;
    mov r13 r14;
    mov r13 (-8L $ mrbp);
    mov r14 (-16L $ mrbp);
  ] in
  expected === register_allocate input;

  let input = [
    mov x y;
    mov z x;
  ] in
  let expected = [
    mov (-8L $ mrbp) r13;
    mov (-16L $ mrbp) r14;
    mov r13 r14;
    mov r13 (-8L $ mrbp);
    mov r14 (-16L $ mrbp);
    mov (-24L $ mrbp) r13;
    mov (-8L $ mrbp) r14;
    mov r13 r14;
    mov r13 (-24L $ mrbp);
    mov r14 (-8L $ mrbp);
  ] in
  expected === register_allocate input;

  let input = [
    push arbp;
    mov arsp arbp;
    mov x y;
    andq z x;
    push a;
    leave;
    ret;
  ] in
  let expected = [
    push rbp;
    mov rsp rbp;
    mov (-8L $ mrbp) r13;
    mov (-16L $ mrbp) r14;
    mov r13 r14;
    mov r13 (-8L $ mrbp);
    mov r14 (-16L $ mrbp);
    mov (-24L $ mrbp) r13;
    mov (-8L $ mrbp) r14;
    andq r13 r14;
    mov r13 (-24L $ mrbp);
    mov r14 (-8L $ mrbp);
    mov (-32L $ mrbp) r13;
    push r13;
    mov r13 (-32L $ mrbp);
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
      "test_chomp"               >:: test_chomp_binop;
      "test_register_allocation" >:: test_register_allocation;
    ] |> run_test_tt_main

let _ = main ()
