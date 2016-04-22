open Core.Std
open OUnit2
open TestUtil
open Util
open Pre
open Ir
open ExprSet

let get_subexpr_test _ =

  let make_binop e1 e2 = BinOp (e1, EQ, e2) in

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

  let mem_subs_set1  = of_list mem_subs1 in
  let mem_subs_set10 = of_list mem_subs2 in
  let mem_subs_set11 = of_list mem_subs3 in
  let mem_subs_set12 = of_list mem_subs4 in
  let mem_subs_set13 = of_list mem_subs5 in
  let mem_subs_set14 = of_list mem_subs6 in
  let mem_subs_set15 = of_list mem_subs7 in
  let mem_subs_set16 = of_list mem_subs8 in
  let mem_subs_set17 = of_list mem_subs9 in
  let mem_subs_set18 = of_list mem_subs10 in
  let mem_subs_set19 = of_list mem_subs11 in
  let mem_subs_set20 = of_list mem_subs12 in
  let mem_subs_set21 = of_list mem_subs13 in
  let mem_subs_set22 = of_list mem_subs14 in
  let mem_subs_set23 = of_list mem_subs15 in
  let mem_subs_set24 = of_list mem_subs16 in
  let mem_subs_set25 = of_list mem_subs17 in
  let mem_subs_set26 = of_list mem_subs18 in
  let mem_subs_set27 = of_list mem_subs19 in
  let mem_subs_set28 = of_list mem_subs20 in
  let mem_subs_set29 = of_list mem_subs21 in
  let mem_subs_set30 = of_list mem_subs22 in

  let _ = assert_true (equal (get_subexpr mem1) mem_subs_set1) in
  let _ = assert_true (equal (get_subexpr mem10) mem_subs_set10) in
  let _ = assert_true (equal (get_subexpr mem11) mem_subs_set11) in
  let _ = assert_true (equal (get_subexpr mem12) mem_subs_set12) in
  let _ = assert_true (equal (get_subexpr mem13) mem_subs_set13) in
  let _ = assert_true (equal (get_subexpr mem14) mem_subs_set14) in
  let _ = assert_true (equal (get_subexpr mem15) mem_subs_set15) in
  let _ = assert_true (equal (get_subexpr mem16) mem_subs_set16) in
  let _ = assert_true (equal (get_subexpr mem17) mem_subs_set17) in
  let _ = assert_true (equal (get_subexpr mem18) mem_subs_set18) in
  let _ = assert_true (equal (get_subexpr mem19) mem_subs_set19) in
  let _ = assert_true (equal (get_subexpr mem20) mem_subs_set20) in
  let _ = assert_true (equal (get_subexpr mem21) mem_subs_set21) in
  let _ = assert_true (equal (get_subexpr mem22) mem_subs_set22) in
  let _ = assert_true (equal (get_subexpr mem23) mem_subs_set23) in
  let _ = assert_true (equal (get_subexpr mem24) mem_subs_set24) in
  let _ = assert_true (equal (get_subexpr mem25) mem_subs_set25) in
  let _ = assert_true (equal (get_subexpr mem26) mem_subs_set26) in
  let _ = assert_true (equal (get_subexpr mem27) mem_subs_set27) in
  let _ = assert_true (equal (get_subexpr mem28) mem_subs_set28) in
  let _ = assert_true (equal (get_subexpr mem29) mem_subs_set29) in
  let _ = assert_true (equal (get_subexpr mem30) mem_subs_set30) in

  let mem_binop1  = make_binop mem1 mem20 in
  let mem_binop2  = make_binop mem10 mem21 in
  let mem_binop3  = make_binop mem11 mem22 in
  let mem_binop4  = make_binop mem12 mem23 in
  let mem_binop5  = make_binop mem13 mem24 in
  let mem_binop6  = make_binop mem14 mem25 in
  let mem_binop7  = make_binop mem15 mem26 in
  let mem_binop8  = make_binop mem16 mem27 in
  let mem_binop9  = make_binop mem17 mem28 in
  let mem_binop10 = make_binop mem18 mem29 in
  let mem_binop11 = make_binop mem19 mem30 in

  let mem_binop_subs1  = mem_binop1  :: (mem_subs1 @ mem_subs12) in
  let mem_binop_subs2  = mem_binop2  :: (mem_subs2 @ mem_subs13) in
  let mem_binop_subs3  = mem_binop3  :: (mem_subs3 @ mem_subs14) in
  let mem_binop_subs4  = mem_binop4  :: (mem_subs4 @ mem_subs15) in
  let mem_binop_subs5  = mem_binop5  :: (mem_subs5 @ mem_subs16) in
  let mem_binop_subs6  = mem_binop6  :: (mem_subs6 @ mem_subs17) in
  let mem_binop_subs7  = mem_binop7  :: (mem_subs7 @ mem_subs18) in
  let mem_binop_subs8  = mem_binop8  :: (mem_subs8 @ mem_subs19) in
  let mem_binop_subs9  = mem_binop9  :: (mem_subs9 @ mem_subs20) in
  let mem_binop_subs10 = mem_binop10 :: (mem_subs10 @ mem_subs21) in
  let mem_binop_subs11 = mem_binop11 :: (mem_subs11 @ mem_subs22) in

  let mem_binop_subs_set1  = of_list mem_binop_subs1 in
  let mem_binop_subs_set10 = of_list mem_binop_subs2 in
  let mem_binop_subs_set11 = of_list mem_binop_subs3 in
  let mem_binop_subs_set12 = of_list mem_binop_subs4 in
  let mem_binop_subs_set13 = of_list mem_binop_subs5 in
  let mem_binop_subs_set14 = of_list mem_binop_subs6 in
  let mem_binop_subs_set15 = of_list mem_binop_subs7 in
  let mem_binop_subs_set16 = of_list mem_binop_subs8 in
  let mem_binop_subs_set17 = of_list mem_binop_subs9 in
  let mem_binop_subs_set18 = of_list mem_binop_subs10 in
  let mem_binop_subs_set19 = of_list mem_binop_subs11 in

  let _ = assert_true (equal (get_subexpr mem_binop1) mem_binop_subs_set1) in
  let _ = assert_true (equal (get_subexpr mem_binop2) mem_binop_subs_set10) in
  let _ = assert_true (equal (get_subexpr mem_binop3) mem_binop_subs_set11) in
  let _ = assert_true (equal (get_subexpr mem_binop4) mem_binop_subs_set12) in
  let _ = assert_true (equal (get_subexpr mem_binop5) mem_binop_subs_set13) in
  let _ = assert_true (equal (get_subexpr mem_binop6) mem_binop_subs_set14) in
  let _ = assert_true (equal (get_subexpr mem_binop7) mem_binop_subs_set15) in
  let _ = assert_true (equal (get_subexpr mem_binop8) mem_binop_subs_set16) in
  let _ = assert_true (equal (get_subexpr mem_binop9) mem_binop_subs_set17) in
  let _ = assert_true (equal (get_subexpr mem_binop10) mem_binop_subs_set18) in
  let _ = assert_true (equal (get_subexpr mem_binop11) mem_binop_subs_set19) in

  let temp1 = Temp "x" in

  let _ = assert_true (equal (get_subexpr temp1) empty) in

  let cjump1 = CJumpOne (binop1, "dummy") in
  let cjump2 = CJumpOne (binop10, "dummy") in
  let cjump3 = CJumpOne (binop11, "dummy") in
  let cjump4 = CJumpOne (binop12, "dummy") in
  let cjump5 = CJumpOne (binop13, "dummy") in
  let cjump6 = CJumpOne (binop14, "dummy") in
  let cjump7 = CJumpOne (binop15, "dummy") in
  let cjump8 = CJumpOne (binop16, "dummy") in
  let cjump9 = CJumpOne (binop17, "dummy") in
  let cjump10 = CJumpOne (binop18, "dummy") in
  let cjump11 = CJumpOne (binop19, "dummy") in
  let cjump12 = CJumpOne (call1, "dummy") in
  let cjump13 = CJumpOne (call10, "dummy") in
  let cjump14 = CJumpOne (call11, "dummy") in
  let cjump15 = CJumpOne (call12, "dummy") in
  let cjump16 = CJumpOne (call13, "dummy") in
  let cjump17 = CJumpOne (call14, "dummy") in
  let cjump18 = CJumpOne (call15, "dummy") in
  let cjump19 = CJumpOne (call16, "dummy") in
  let cjump20 = CJumpOne (call17, "dummy") in
  let cjump21 = CJumpOne (call18, "dummy") in
  let cjump22 = CJumpOne (call19, "dummy") in
  let cjump23 = CJumpOne (mem1, "dummy") in
  let cjump24 = CJumpOne (mem10, "dummy") in
  let cjump25 = CJumpOne (mem11, "dummy") in
  let cjump26 = CJumpOne (mem12, "dummy") in
  let cjump27 = CJumpOne (mem13, "dummy") in
  let cjump28 = CJumpOne (mem14, "dummy") in
  let cjump29 = CJumpOne (mem15, "dummy") in
  let cjump30 = CJumpOne (mem16, "dummy") in
  let cjump31 = CJumpOne (mem17, "dummy") in
  let cjump32 = CJumpOne (mem18, "dummy") in
  let cjump33 = CJumpOne (mem19, "dummy") in
  let cjump34 = CJumpOne (mem20, "dummy") in
  let cjump35 = CJumpOne (mem21, "dummy") in
  let cjump36 = CJumpOne (mem22, "dummy") in
  let cjump37 = CJumpOne (mem23, "dummy") in
  let cjump38 = CJumpOne (mem24, "dummy") in
  let cjump39 = CJumpOne (mem25, "dummy") in
  let cjump40 = CJumpOne (mem26, "dummy") in
  let cjump41 = CJumpOne (mem27, "dummy") in
  let cjump42 = CJumpOne (mem28, "dummy") in
  let cjump43 = CJumpOne (mem29, "dummy") in
  let cjump44 = CJumpOne (mem30, "dummy") in
  let cjump45 = CJumpOne (mem_binop1, "dummy") in
  let cjump46 = CJumpOne (mem_binop2, "dummy") in
  let cjump47 = CJumpOne (mem_binop3, "dummy") in
  let cjump48 = CJumpOne (mem_binop4, "dummy") in
  let cjump49 = CJumpOne (mem_binop5, "dummy") in
  let cjump50 = CJumpOne (mem_binop6, "dummy") in
  let cjump51 = CJumpOne (mem_binop7, "dummy") in
  let cjump52 = CJumpOne (mem_binop8, "dummy") in
  let cjump53 = CJumpOne (mem_binop9, "dummy") in
  let cjump54 = CJumpOne (mem_binop10, "dummy") in
  let cjump55 = CJumpOne (mem_binop11, "dummy") in

  let _ = assert_true (equal (get_subexpr_stmt cjump1) subs_set1) in
  let _ = assert_true (equal (get_subexpr_stmt cjump2) subs_set2) in
  let _ = assert_true (equal (get_subexpr_stmt cjump3) subs_set3) in
  let _ = assert_true (equal (get_subexpr_stmt cjump4) subs_set4) in
  let _ = assert_true (equal (get_subexpr_stmt cjump5) subs_set5) in
  let _ = assert_true (equal (get_subexpr_stmt cjump6) subs_set6) in
  let _ = assert_true (equal (get_subexpr_stmt cjump7) subs_set7) in
  let _ = assert_true (equal (get_subexpr_stmt cjump8) subs_set8) in
  let _ = assert_true (equal (get_subexpr_stmt cjump9) subs_set9) in
  let _ = assert_true (equal (get_subexpr_stmt cjump10) subs_set10) in
  let _ = assert_true (equal (get_subexpr_stmt cjump11) subs_set11) in
  let _ = assert_true (equal (get_subexpr_stmt cjump12) subs_set1) in
  let _ = assert_true (equal (get_subexpr_stmt cjump13) subs_set2) in
  let _ = assert_true (equal (get_subexpr_stmt cjump14) subs_set3) in
  let _ = assert_true (equal (get_subexpr_stmt cjump15) subs_set4) in
  let _ = assert_true (equal (get_subexpr_stmt cjump16) subs_set5) in
  let _ = assert_true (equal (get_subexpr_stmt cjump17) subs_set6) in
  let _ = assert_true (equal (get_subexpr_stmt cjump18) subs_set7) in
  let _ = assert_true (equal (get_subexpr_stmt cjump19) subs_set8) in
  let _ = assert_true (equal (get_subexpr_stmt cjump20) subs_set9) in
  let _ = assert_true (equal (get_subexpr_stmt cjump21) subs_set10) in
  let _ = assert_true (equal (get_subexpr_stmt cjump22) subs_set11) in
  let _ = assert_true (equal (get_subexpr_stmt cjump23) mem_subs_set1) in
  let _ = assert_true (equal (get_subexpr_stmt cjump24) mem_subs_set10) in
  let _ = assert_true (equal (get_subexpr_stmt cjump25) mem_subs_set11) in
  let _ = assert_true (equal (get_subexpr_stmt cjump26) mem_subs_set12) in
  let _ = assert_true (equal (get_subexpr_stmt cjump27) mem_subs_set13) in
  let _ = assert_true (equal (get_subexpr_stmt cjump28) mem_subs_set14) in
  let _ = assert_true (equal (get_subexpr_stmt cjump29) mem_subs_set15) in
  let _ = assert_true (equal (get_subexpr_stmt cjump30) mem_subs_set16) in
  let _ = assert_true (equal (get_subexpr_stmt cjump31) mem_subs_set17) in
  let _ = assert_true (equal (get_subexpr_stmt cjump32) mem_subs_set18) in
  let _ = assert_true (equal (get_subexpr_stmt cjump33) mem_subs_set19) in
  let _ = assert_true (equal (get_subexpr_stmt cjump34) mem_subs_set20) in
  let _ = assert_true (equal (get_subexpr_stmt cjump35) mem_subs_set21) in
  let _ = assert_true (equal (get_subexpr_stmt cjump36) mem_subs_set22) in
  let _ = assert_true (equal (get_subexpr_stmt cjump37) mem_subs_set23) in
  let _ = assert_true (equal (get_subexpr_stmt cjump38) mem_subs_set24) in
  let _ = assert_true (equal (get_subexpr_stmt cjump39) mem_subs_set25) in
  let _ = assert_true (equal (get_subexpr_stmt cjump40) mem_subs_set26) in
  let _ = assert_true (equal (get_subexpr_stmt cjump41) mem_subs_set27) in
  let _ = assert_true (equal (get_subexpr_stmt cjump42) mem_subs_set28) in
  let _ = assert_true (equal (get_subexpr_stmt cjump43) mem_subs_set29) in
  let _ = assert_true (equal (get_subexpr_stmt cjump44) mem_subs_set30) in
  let _ = assert_true (equal (get_subexpr_stmt cjump45) mem_binop_subs_set1) in
  let _ = assert_true (equal (get_subexpr_stmt cjump46) mem_binop_subs_set10) in
  let _ = assert_true (equal (get_subexpr_stmt cjump47) mem_binop_subs_set11) in
  let _ = assert_true (equal (get_subexpr_stmt cjump48) mem_binop_subs_set12) in
  let _ = assert_true (equal (get_subexpr_stmt cjump49) mem_binop_subs_set13) in
  let _ = assert_true (equal (get_subexpr_stmt cjump50) mem_binop_subs_set14) in
  let _ = assert_true (equal (get_subexpr_stmt cjump51) mem_binop_subs_set15) in
  let _ = assert_true (equal (get_subexpr_stmt cjump52) mem_binop_subs_set16) in
  let _ = assert_true (equal (get_subexpr_stmt cjump53) mem_binop_subs_set17) in
  let _ = assert_true (equal (get_subexpr_stmt cjump54) mem_binop_subs_set18) in
  let _ = assert_true (equal (get_subexpr_stmt cjump55) mem_binop_subs_set19) in

  let move1 = Move (Temp "dummy", binop1) in
  let move2 = Move (Temp "dummy", binop10) in
  let move3 = Move (Temp "dummy", binop11) in
  let move4 = Move (Temp "dummy", binop12) in
  let move5 = Move (Temp "dummy", binop13) in
  let move6 = Move (Temp "dummy", binop14) in
  let move7 = Move (Temp "dummy", binop15) in
  let move8 = Move (Temp "dummy", binop16) in
  let move9 = Move (Temp "dummy", binop17) in
  let move10 = Move (Temp "dummy", binop18) in
  let move11 = Move (Temp "dummy", binop19) in
  let move12 = Move (Temp "dummy", call1) in
  let move13 = Move (Temp "dummy", call10) in
  let move14 = Move (Temp "dummy", call11) in
  let move15 = Move (Temp "dummy", call12) in
  let move16 = Move (Temp "dummy", call13) in
  let move17 = Move (Temp "dummy", call14) in
  let move18 = Move (Temp "dummy", call15) in
  let move19 = Move (Temp "dummy", call16) in
  let move20 = Move (Temp "dummy", call17) in
  let move21 = Move (Temp "dummy", call18) in
  let move22 = Move (Temp "dummy", call19) in
  let move23 = Move (Temp "dummy", mem1) in
  let move24 = Move (Temp "dummy", mem10) in
  let move25 = Move (Temp "dummy", mem11) in
  let move26 = Move (Temp "dummy", mem12) in
  let move27 = Move (Temp "dummy", mem13) in
  let move28 = Move (Temp "dummy", mem14) in
  let move29 = Move (Temp "dummy", mem15) in
  let move30 = Move (Temp "dummy", mem16) in
  let move31 = Move (Temp "dummy", mem17) in
  let move32 = Move (Temp "dummy", mem18) in
  let move33 = Move (Temp "dummy", mem19) in
  let move34 = Move (Temp "dummy", mem20) in
  let move35 = Move (Temp "dummy", mem21) in
  let move36 = Move (Temp "dummy", mem22) in
  let move37 = Move (Temp "dummy", mem23) in
  let move38 = Move (Temp "dummy", mem24) in
  let move39 = Move (Temp "dummy", mem25) in
  let move40 = Move (Temp "dummy", mem26) in
  let move41 = Move (Temp "dummy", mem27) in
  let move42 = Move (Temp "dummy", mem28) in
  let move43 = Move (Temp "dummy", mem29) in
  let move44 = Move (Temp "dummy", mem30) in
  let move45 = Move (Temp "dummy", mem_binop1) in
  let move46 = Move (Temp "dummy", mem_binop2) in
  let move47 = Move (Temp "dummy", mem_binop3) in
  let move48 = Move (Temp "dummy", mem_binop4) in
  let move49 = Move (Temp "dummy", mem_binop5) in
  let move50 = Move (Temp "dummy", mem_binop6) in
  let move51 = Move (Temp "dummy", mem_binop7) in
  let move52 = Move (Temp "dummy", mem_binop8) in
  let move53 = Move (Temp "dummy", mem_binop9) in
  let move54 = Move (Temp "dummy", mem_binop10) in
  let move55 = Move (Temp "dummy", mem_binop11) in

  let _ = assert_true (equal (get_subexpr_stmt move1) subs_set1) in
  let _ = assert_true (equal (get_subexpr_stmt move2) subs_set2) in
  let _ = assert_true (equal (get_subexpr_stmt move3) subs_set3) in
  let _ = assert_true (equal (get_subexpr_stmt move4) subs_set4) in
  let _ = assert_true (equal (get_subexpr_stmt move5) subs_set5) in
  let _ = assert_true (equal (get_subexpr_stmt move6) subs_set6) in
  let _ = assert_true (equal (get_subexpr_stmt move7) subs_set7) in
  let _ = assert_true (equal (get_subexpr_stmt move8) subs_set8) in
  let _ = assert_true (equal (get_subexpr_stmt move9) subs_set9) in
  let _ = assert_true (equal (get_subexpr_stmt move10) subs_set10) in
  let _ = assert_true (equal (get_subexpr_stmt move11) subs_set11) in
  let _ = assert_true (equal (get_subexpr_stmt move12) subs_set1) in
  let _ = assert_true (equal (get_subexpr_stmt move13) subs_set2) in
  let _ = assert_true (equal (get_subexpr_stmt move14) subs_set3) in
  let _ = assert_true (equal (get_subexpr_stmt move15) subs_set4) in
  let _ = assert_true (equal (get_subexpr_stmt move16) subs_set5) in
  let _ = assert_true (equal (get_subexpr_stmt move17) subs_set6) in
  let _ = assert_true (equal (get_subexpr_stmt move18) subs_set7) in
  let _ = assert_true (equal (get_subexpr_stmt move19) subs_set8) in
  let _ = assert_true (equal (get_subexpr_stmt move20) subs_set9) in
  let _ = assert_true (equal (get_subexpr_stmt move21) subs_set10) in
  let _ = assert_true (equal (get_subexpr_stmt move22) subs_set11) in
  let _ = assert_true (equal (get_subexpr_stmt move23) mem_subs_set1) in
  let _ = assert_true (equal (get_subexpr_stmt move24) mem_subs_set10) in
  let _ = assert_true (equal (get_subexpr_stmt move25) mem_subs_set11) in
  let _ = assert_true (equal (get_subexpr_stmt move26) mem_subs_set12) in
  let _ = assert_true (equal (get_subexpr_stmt move27) mem_subs_set13) in
  let _ = assert_true (equal (get_subexpr_stmt move28) mem_subs_set14) in
  let _ = assert_true (equal (get_subexpr_stmt move29) mem_subs_set15) in
  let _ = assert_true (equal (get_subexpr_stmt move30) mem_subs_set16) in
  let _ = assert_true (equal (get_subexpr_stmt move31) mem_subs_set17) in
  let _ = assert_true (equal (get_subexpr_stmt move32) mem_subs_set18) in
  let _ = assert_true (equal (get_subexpr_stmt move33) mem_subs_set19) in
  let _ = assert_true (equal (get_subexpr_stmt move34) mem_subs_set20) in
  let _ = assert_true (equal (get_subexpr_stmt move35) mem_subs_set21) in
  let _ = assert_true (equal (get_subexpr_stmt move36) mem_subs_set22) in
  let _ = assert_true (equal (get_subexpr_stmt move37) mem_subs_set23) in
  let _ = assert_true (equal (get_subexpr_stmt move38) mem_subs_set24) in
  let _ = assert_true (equal (get_subexpr_stmt move39) mem_subs_set25) in
  let _ = assert_true (equal (get_subexpr_stmt move40) mem_subs_set26) in
  let _ = assert_true (equal (get_subexpr_stmt move41) mem_subs_set27) in
  let _ = assert_true (equal (get_subexpr_stmt move42) mem_subs_set28) in
  let _ = assert_true (equal (get_subexpr_stmt move43) mem_subs_set29) in
  let _ = assert_true (equal (get_subexpr_stmt move44) mem_subs_set30) in
  let _ = assert_true (equal (get_subexpr_stmt move45) mem_binop_subs_set1) in
  let _ = assert_true (equal (get_subexpr_stmt move46) mem_binop_subs_set10) in
  let _ = assert_true (equal (get_subexpr_stmt move47) mem_binop_subs_set11) in
  let _ = assert_true (equal (get_subexpr_stmt move48) mem_binop_subs_set12) in
  let _ = assert_true (equal (get_subexpr_stmt move49) mem_binop_subs_set13) in
  let _ = assert_true (equal (get_subexpr_stmt move50) mem_binop_subs_set14) in
  let _ = assert_true (equal (get_subexpr_stmt move51) mem_binop_subs_set15) in
  let _ = assert_true (equal (get_subexpr_stmt move52) mem_binop_subs_set16) in
  let _ = assert_true (equal (get_subexpr_stmt move53) mem_binop_subs_set17) in
  let _ = assert_true (equal (get_subexpr_stmt move54) mem_binop_subs_set18) in
  let _ = assert_true (equal (get_subexpr_stmt move55) mem_binop_subs_set19) in

  let jump1 = Jump (Name "dummy") in
  let label1 = Label "dummy" in
  let return1 = Return in

  let _ = assert_true (equal (get_subexpr_stmt jump1) empty) in
  let _ = assert_true (equal (get_subexpr_stmt label1) empty) in
  let _ = assert_true (equal (get_subexpr_stmt return1) empty) in

  ()

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* ! DON'T FORGET TO ADD YOUR TESTS HERE                                     ! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let main () =
    "suite" >::: [
      "get_subexpr_test" >:: get_subexpr_test;
    ] |> run_test_tt_main

let _ = main ()

