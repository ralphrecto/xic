open Core.Std
open OUnit2
open TestUtil
open Util
open Pre
open Ir
open ExprSet

let get_subexpr_test _ =

  let make_binop e1 e2 = BinOp (e1, ADD, e2) in

  let binop1  = BinOp (Const 1L, ADD, Const 2L) in
  let binop2  = BinOp (Const 1L, SUB, Const 2L) in
  let binop3  = BinOp (Const 1L, MUL, Const 2L) in
  let binop4  = BinOp (Const 1L, HMUL, Const 2L) in
  let binop5  = BinOp (Const 1L, LEQ, Const 2L) in
  let binop6  = BinOp (Const 1L, GEQ, Const 2L) in
  let binop7  = BinOp (Const 1L, AND, Const 2L) in
  let binop8  = BinOp (Const 1L, OR, Const 2L) in
  let binop9  = BinOp (Const 1L, EQ, Const 2L) in
  let binop10 = make_binop binop1 binop2 in
  let binop11 = make_binop binop10 binop3 in
  let binop12 = make_binop binop11 binop4 in
  let binop13 = make_binop binop12 binop5 in
  let binop14 = make_binop binop13 binop6 in
  let binop15 = make_binop binop14 binop7 in
  let binop16 = make_binop binop15 binop8 in
  let binop17 = make_binop binop16 binop9 in
  let binop18 = make_binop binop17 binop10 in
  let binop19 = make_binop binop18 binop11 in

  let subs1  = [binop1] in
  let subs2  = [binop10; binop2; binop1] in
  let subs3  = [binop11; binop3; binop10; binop2; binop1] in
  let subs4  = [binop12; binop4; binop11; binop3; binop10;
                binop2; binop1] in
  let subs5  = [binop13; binop5; binop12; binop4; binop11;
                binop3; binop10; binop2; binop1] in
  let subs6  = [binop14; binop6; binop13; binop5; binop12;
                binop4; binop11; binop3; binop10; binop2;
                binop1] in
  let subs7  = [binop15; binop7; binop14; binop6; binop13;
                binop5; binop12; binop4; binop11; binop3;
                binop10; binop2; binop1] in
  let subs8  = [binop16; binop8; binop15; binop7; binop14;
                binop6; binop13; binop5; binop12; binop4;
                binop11; binop3; binop10; binop2; binop1] in
  let subs9  = [binop17; binop9; binop16; binop8; binop15;
                binop7; binop14; binop6; binop13; binop5;
                binop12; binop4; binop11; binop3; binop10;
                binop2; binop1] in
  let subs10 = [binop18; binop10; binop17; binop9; binop16;
                binop8; binop15; binop7; binop14; binop6;
                binop13; binop5; binop12; binop4; binop11;
                binop3; binop10; binop2; binop1] in
  let subs11 = [binop19; binop11; binop18; binop10; binop17;
                binop9; binop16; binop8; binop15; binop7;
                binop14; binop6; binop13; binop5; binop12;
                binop4; binop11; binop3; binop10; binop2;
                binop1] in

  let subs_set1  = of_list subs1 in
  let subs_set2  = of_list subs2 in
  let subs_set3  = of_list subs3 in
  let subs_set4  = of_list subs4 in
  let subs_set5  = of_list subs5 in
  let subs_set6  = of_list subs6 in
  let subs_set7  = of_list subs7 in
  let subs_set8  = of_list subs8 in
  let subs_set9  = of_list subs9 in
  let subs_set10 = of_list subs10 in
  let subs_set11 = of_list subs11 in

  let _ = assert_true (equal (get_subexpr binop1) subs_set1) in
  let _ = assert_true (equal (get_subexpr binop10) subs_set2) in
  let _ = assert_true (equal (get_subexpr binop11) subs_set3) in
  let _ = assert_true (equal (get_subexpr binop12) subs_set4) in
  let _ = assert_true (equal (get_subexpr binop13) subs_set5) in
  let _ = assert_true (equal (get_subexpr binop14) subs_set6) in
  let _ = assert_true (equal (get_subexpr binop15) subs_set7) in
  let _ = assert_true (equal (get_subexpr binop16) subs_set8) in
  let _ = assert_true (equal (get_subexpr binop17) subs_set9) in
  let _ = assert_true (equal (get_subexpr binop18) subs_set10) in
  let _ = assert_true (equal (get_subexpr binop19) subs_set11) in

  let call1  = Call (Name "dummy", [binop1]) in
  let call10 = Call (Name "dummy", [binop10]) in
  let call11 = Call (Name "dummy", [binop11]) in
  let call12 = Call (Name "dummy", [binop12]) in
  let call13 = Call (Name "dummy", [binop13]) in
  let call14 = Call (Name "dummy", [binop14]) in
  let call15 = Call (Name "dummy", [binop15]) in
  let call16 = Call (Name "dummy", [binop16]) in
  let call17 = Call (Name "dummy", [binop17]) in
  let call18 = Call (Name "dummy", [binop18]) in
  let call19 = Call (Name "dummy", [binop19]) in

  let _ = assert_true (equal (get_subexpr call1) subs_set1) in
  let _ = assert_true (equal (get_subexpr call10) subs_set2) in
  let _ = assert_true (equal (get_subexpr call11) subs_set3) in
  let _ = assert_true (equal (get_subexpr call12) subs_set4) in
  let _ = assert_true (equal (get_subexpr call13) subs_set5) in
  let _ = assert_true (equal (get_subexpr call14) subs_set6) in
  let _ = assert_true (equal (get_subexpr call15) subs_set7) in
  let _ = assert_true (equal (get_subexpr call16) subs_set8) in
  let _ = assert_true (equal (get_subexpr call17) subs_set9) in
  let _ = assert_true (equal (get_subexpr call18) subs_set10) in
  let _ = assert_true (equal (get_subexpr call19) subs_set11) in

  let mem1  = Mem (binop1, NORMAL) in
  let mem10 = Mem (binop10, NORMAL) in
  let mem11 = Mem (binop11, NORMAL) in
  let mem12 = Mem (binop12, NORMAL) in
  let mem13 = Mem (binop13, NORMAL) in
  let mem14 = Mem (binop14, NORMAL) in
  let mem15 = Mem (binop15, NORMAL) in
  let mem16 = Mem (binop16, NORMAL) in
  let mem17 = Mem (binop17, NORMAL) in
  let mem18 = Mem (binop18, NORMAL) in
  let mem19 = Mem (binop19, NORMAL) in
  let mem20 = Mem (call1, NORMAL) in
  let mem21 = Mem (call10, NORMAL) in
  let mem22 = Mem (call11, NORMAL) in
  let mem23 = Mem (call12, NORMAL) in
  let mem24 = Mem (call13, NORMAL) in
  let mem25 = Mem (call14, NORMAL) in
  let mem26 = Mem (call15, NORMAL) in
  let mem27 = Mem (call16, NORMAL) in
  let mem28 = Mem (call17, NORMAL) in
  let mem29 = Mem (call18, NORMAL) in
  let mem30 = Mem (call19, NORMAL) in

  let mem_subs1  = mem1 :: subs1 in
  let mem_subs2  = mem10 :: subs2 in
  let mem_subs3  = mem11 :: subs3 in
  let mem_subs4  = mem12 :: subs4 in
  let mem_subs5  = mem13 :: subs5 in
  let mem_subs6  = mem14 :: subs6 in
  let mem_subs7  = mem15 :: subs7 in
  let mem_subs8  = mem16 :: subs8 in
  let mem_subs9  = mem17 :: subs9 in
  let mem_subs10 = mem18 :: subs10 in
  let mem_subs11 = mem19 :: subs11 in
  let mem_subs12 = mem20 :: subs1 in
  let mem_subs13 = mem21 :: subs2 in
  let mem_subs14 = mem22 :: subs3 in
  let mem_subs15 = mem23 :: subs4 in
  let mem_subs16 = mem24 :: subs5 in
  let mem_subs17 = mem25 :: subs6 in
  let mem_subs18 = mem26 :: subs7 in
  let mem_subs19 = mem27 :: subs8 in
  let mem_subs20 = mem28 :: subs9 in
  let mem_subs21 = mem29 :: subs10 in
  let mem_subs22 = mem30 :: subs11 in

  let subs_set1  = of_list mem_subs1 in
  let subs_set10 = of_list mem_subs2 in
  let subs_set11 = of_list mem_subs3 in
  let subs_set12 = of_list mem_subs4 in
  let subs_set13 = of_list mem_subs5 in
  let subs_set14 = of_list mem_subs6 in
  let subs_set15 = of_list mem_subs7 in
  let subs_set16 = of_list mem_subs8 in
  let subs_set17 = of_list mem_subs9 in
  let subs_set18 = of_list mem_subs10 in
  let subs_set19 = of_list mem_subs11 in
  let subs_set20 = of_list mem_subs12 in
  let subs_set21 = of_list mem_subs13 in
  let subs_set22 = of_list mem_subs14 in
  let subs_set23 = of_list mem_subs15 in
  let subs_set24 = of_list mem_subs16 in
  let subs_set25 = of_list mem_subs17 in
  let subs_set26 = of_list mem_subs18 in
  let subs_set27 = of_list mem_subs19 in
  let subs_set28 = of_list mem_subs20 in
  let subs_set29 = of_list mem_subs21 in
  let subs_set30 = of_list mem_subs22 in

  let _ = assert_true (equal (get_subexpr mem1) subs_set1) in
  let _ = assert_true (equal (get_subexpr mem10) subs_set10) in
  let _ = assert_true (equal (get_subexpr mem11) subs_set11) in
  let _ = assert_true (equal (get_subexpr mem12) subs_set12) in
  let _ = assert_true (equal (get_subexpr mem13) subs_set13) in
  let _ = assert_true (equal (get_subexpr mem14) subs_set14) in
  let _ = assert_true (equal (get_subexpr mem15) subs_set15) in
  let _ = assert_true (equal (get_subexpr mem16) subs_set16) in
  let _ = assert_true (equal (get_subexpr mem17) subs_set17) in
  let _ = assert_true (equal (get_subexpr mem18) subs_set18) in
  let _ = assert_true (equal (get_subexpr mem19) subs_set19) in
  let _ = assert_true (equal (get_subexpr mem20) subs_set20) in
  let _ = assert_true (equal (get_subexpr mem21) subs_set21) in
  let _ = assert_true (equal (get_subexpr mem22) subs_set22) in
  let _ = assert_true (equal (get_subexpr mem23) subs_set23) in
  let _ = assert_true (equal (get_subexpr mem24) subs_set24) in
  let _ = assert_true (equal (get_subexpr mem25) subs_set25) in
  let _ = assert_true (equal (get_subexpr mem26) subs_set26) in
  let _ = assert_true (equal (get_subexpr mem27) subs_set27) in
  let _ = assert_true (equal (get_subexpr mem28) subs_set28) in
  let _ = assert_true (equal (get_subexpr mem29) subs_set29) in
  let _ = assert_true (equal (get_subexpr mem30) subs_set30) in

  ()

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* ! DON'T FORGET TO ADD YOUR TESTS HERE                                     ! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let main () =
    "suite" >::: [
      "get_subexpr_test" >:: get_subexpr_test;
    ] |> run_test_tt_main

let _ = main ()

