open Core.Std
open OUnit2
open TestUtil
open Util
open Pre
open Ir
open ExprSet

module IrCfgEq = struct
  let (===) (a: Cfg.IrCfg.t) (b: Cfg.IrCfg.t) : unit =
    assert_equal ~cmp:Cfg.IrCfg.equal ~printer:Cfg.IrCfg.to_dot a b

  let (=/=) (a: Cfg.IrCfg.t) (b: Cfg.IrCfg.t) : unit =
    if Cfg.IrCfg.equal a b then
        let a = Cfg.IrCfg.to_dot a in
        let b = Cfg.IrCfg.to_dot b in
        assert_failure (sprintf "These are equal, but shouldn't be:\n%s\n%s" a b)
end

module EdgeToExprEq = struct
  type mapping = (Cfg.IrCfg.E.t * Pre.ExprSet.t) list

  let printer (f: mapping) : string =
    List.map f ~f:(fun (edge, exprs) ->
      sprintf "  (%s) -> %s"
        (Cfg.IrCfg.string_of_edge edge)
        (ExprSet.to_small_string exprs)
    )
    |> String.concat ~sep:",\n"
    |> fun s -> "[\n" ^ s ^ "\n]"

  let sort (f: mapping) : mapping =
    List.sort ~cmp:(fun (edge, _) (edge', _) -> Cfg.IrCfg.E.compare edge edge') f

  let cmp (a: mapping) (b: mapping) : bool =
    List.length a = List.length b &&
    List.for_all (List.zip_exn a b) ~f:(fun ((edge1, expr1), (edge2, expr2)) ->
      Cfg.IrCfg.E.compare edge1 edge2 = 0 &&
      Pre.ExprSet.equal expr1 expr2
    )

  let (===) (a: mapping) (b: mapping) : unit =
    assert_equal ~cmp ~printer (sort a) (sort b)

  let (=/=) (a: mapping) (b: mapping) : unit =
    if cmp a b then
        let a = printer a in
        let b = printer b in
        assert_failure (sprintf "These are equal, but shouldn't be:\n%s\n%s" a b)
end

module StmtsEq = struct
  open Ir
  let (===) (a: stmt list) (b: stmt list) : unit =
    assert_equal ~printer:string_of_stmts a b
end

let make_graph vertexes edges =
  let open Cfg.IrCfg in
  let g = create () in
  List.iter vertexes ~f:(fun i -> add_vertex g (V.create i));
  List.iter edges ~f:(fun (i, l, j) -> add_edge_e g (E.create i l j));
  g

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

  let move56 = Move (mem1, binop1) in
  let move57 = Move (mem1, binop10) in
  let move58 = Move (mem1, binop11) in
  let move59 = Move (mem1, binop12) in
  let move60 = Move (mem1, binop13) in
  let move61 = Move (mem1, binop14) in
  let move62 = Move (mem1, binop15) in
  let move63 = Move (mem1, binop16) in
  let move64 = Move (mem1, binop17) in
  let move65 = Move (mem1, binop18) in
  let move66 = Move (mem1, binop19) in
  let move67 = Move (mem1, call1) in
  let move68 = Move (mem1, call10) in
  let move69 = Move (mem1, call11) in
  let move70 = Move (mem1, call12) in
  let move71 = Move (mem1, call13) in
  let move72 = Move (mem1, call14) in
  let move73 = Move (mem1, call15) in
  let move74 = Move (mem1, call16) in
  let move75 = Move (mem1, call17) in
  let move76 = Move (mem1, call18) in
  let move77 = Move (mem1, call19) in
  let move78 = Move (mem1, mem1) in
  let move79 = Move (mem1, mem10) in
  let move80 = Move (mem1, mem11) in
  let move81 = Move (mem1, mem12) in
  let move82 = Move (mem1, mem13) in
  let move83 = Move (mem1, mem14) in
  let move84 = Move (mem1, mem15) in
  let move85 = Move (mem1, mem16) in
  let move86 = Move (mem1, mem17) in
  let move87 = Move (mem1, mem18) in
  let move88 = Move (mem1, mem19) in
  let move89 = Move (mem1, mem20) in
  let move90 = Move (mem1, mem21) in
  let move91 = Move (mem1, mem22) in
  let move92 = Move (mem1, mem23) in
  let move93 = Move (mem1, mem24) in
  let move94 = Move (mem1, mem25) in
  let move95 = Move (mem1, mem26) in
  let move96 = Move (mem1, mem27) in
  let move97 = Move (mem1, mem28) in
  let move98 = Move (mem1, mem29) in
  let move99 = Move (mem1, mem30) in
  let move100 = Move (mem1, mem_binop1) in
  let move101 = Move (mem1, mem_binop2) in
  let move102 = Move (mem1, mem_binop3) in
  let move103 = Move (mem1, mem_binop4) in
  let move104 = Move (mem1, mem_binop5) in
  let move105 = Move (mem1, mem_binop6) in
  let move106 = Move (mem1, mem_binop7) in
  let move107 = Move (mem1, mem_binop8) in
  let move108 = Move (mem1, mem_binop9) in
  let move109 = Move (mem1, mem_binop10) in
  let move110 = Move (mem1, mem_binop11) in

  let move_mem1 = mem1 :: subs1 in
  let move_mem2 = mem1 :: subs2 in
  let move_mem3 = mem1 :: subs3 in
  let move_mem4 = mem1 :: subs4 in
  let move_mem5 = mem1 :: subs5 in
  let move_mem6 = mem1 :: subs6 in
  let move_mem7 = mem1 :: subs7 in
  let move_mem8 = mem1 :: subs8 in
  let move_mem9 = mem1 :: subs9 in
  let move_mem10 = mem1 :: subs10 in
  let move_mem11 = mem1 :: subs11 in
  let move_mem12 = mem1 :: subs1 in
  let move_mem13 = mem1 :: subs2 in
  let move_mem14 = mem1 :: subs3 in
  let move_mem15 = mem1 :: subs4 in
  let move_mem16 = mem1 :: subs5 in
  let move_mem17 = mem1 :: subs6 in
  let move_mem18 = mem1 :: subs7 in
  let move_mem19 = mem1 :: subs8 in
  let move_mem20 = mem1 :: subs9 in
  let move_mem21 = mem1 :: subs10 in
  let move_mem22 = mem1 :: subs11 in
  let move_mem23 = mem1 :: mem_subs1 in
  let move_mem24 = mem1 :: mem_subs2  in
  let move_mem25 = mem1 :: mem_subs3  in
  let move_mem26 = mem1 :: mem_subs4  in
  let move_mem27 = mem1 :: mem_subs5  in
  let move_mem28 = mem1 :: mem_subs6  in
  let move_mem29 = mem1 :: mem_subs7  in
  let move_mem30 = mem1 :: mem_subs8  in
  let move_mem31 = mem1 :: mem_subs9  in
  let move_mem32 = mem1 :: mem_subs10 in
  let move_mem33 = mem1 :: mem_subs11 in
  let move_mem34 = mem1 :: mem_subs12 in
  let move_mem35 = mem1 :: mem_subs13 in
  let move_mem36 = mem1 :: mem_subs14 in
  let move_mem37 = mem1 :: mem_subs15 in
  let move_mem38 = mem1 :: mem_subs16 in
  let move_mem39 = mem1 :: mem_subs17 in
  let move_mem40 = mem1 :: mem_subs18 in
  let move_mem41 = mem1 :: mem_subs19 in
  let move_mem42 = mem1 :: mem_subs20 in
  let move_mem43 = mem1 :: mem_subs21 in
  let move_mem44 = mem1 :: mem_subs22 in
  let move_mem45 = mem1 :: mem_binop_subs1 in
  let move_mem46 = mem1 :: mem_binop_subs2 in
  let move_mem47 = mem1 :: mem_binop_subs3 in
  let move_mem48 = mem1 :: mem_binop_subs4 in
  let move_mem49 = mem1 :: mem_binop_subs5 in
  let move_mem50 = mem1 :: mem_binop_subs6 in
  let move_mem51 = mem1 :: mem_binop_subs7 in
  let move_mem52 = mem1 :: mem_binop_subs8 in
  let move_mem53 = mem1 :: mem_binop_subs9 in
  let move_mem54 = mem1 :: mem_binop_subs10 in
  let move_mem55 = mem1 :: mem_binop_subs11 in

  let move_mem_subs1  = of_list move_mem1  in
  let move_mem_subs2  = of_list move_mem2  in
  let move_mem_subs3  = of_list move_mem3  in
  let move_mem_subs4  = of_list move_mem4  in
  let move_mem_subs5  = of_list move_mem5  in
  let move_mem_subs6  = of_list move_mem6  in
  let move_mem_subs7  = of_list move_mem7  in
  let move_mem_subs8  = of_list move_mem8  in
  let move_mem_subs9  = of_list move_mem9  in
  let move_mem_subs10 = of_list move_mem10 in
  let move_mem_subs11 = of_list move_mem11 in
  let move_mem_subs12 = of_list move_mem12 in
  let move_mem_subs13 = of_list move_mem13 in
  let move_mem_subs14 = of_list move_mem14 in
  let move_mem_subs15 = of_list move_mem15 in
  let move_mem_subs16 = of_list move_mem16 in
  let move_mem_subs17 = of_list move_mem17 in
  let move_mem_subs18 = of_list move_mem18 in
  let move_mem_subs19 = of_list move_mem19 in
  let move_mem_subs20 = of_list move_mem20 in
  let move_mem_subs21 = of_list move_mem21 in
  let move_mem_subs22 = of_list move_mem22 in
  let move_mem_subs23 = of_list move_mem23 in
  let move_mem_subs24 = of_list move_mem24 in
  let move_mem_subs25 = of_list move_mem25 in
  let move_mem_subs26 = of_list move_mem26 in
  let move_mem_subs27 = of_list move_mem27 in
  let move_mem_subs28 = of_list move_mem28 in
  let move_mem_subs29 = of_list move_mem29 in
  let move_mem_subs30 = of_list move_mem30 in
  let move_mem_subs31 = of_list move_mem31 in
  let move_mem_subs32 = of_list move_mem32 in
  let move_mem_subs33 = of_list move_mem33 in
  let move_mem_subs34 = of_list move_mem34 in
  let move_mem_subs35 = of_list move_mem35 in
  let move_mem_subs36 = of_list move_mem36 in
  let move_mem_subs37 = of_list move_mem37 in
  let move_mem_subs38 = of_list move_mem38 in
  let move_mem_subs39 = of_list move_mem39 in
  let move_mem_subs40 = of_list move_mem40 in
  let move_mem_subs41 = of_list move_mem41 in
  let move_mem_subs42 = of_list move_mem42 in
  let move_mem_subs43 = of_list move_mem43 in
  let move_mem_subs44 = of_list move_mem44 in
  let move_mem_subs45 = of_list move_mem45 in
  let move_mem_subs46 = of_list move_mem46 in
  let move_mem_subs47 = of_list move_mem47 in
  let move_mem_subs48 = of_list move_mem48 in
  let move_mem_subs49 = of_list move_mem49 in
  let move_mem_subs50 = of_list move_mem50 in
  let move_mem_subs51 = of_list move_mem51 in
  let move_mem_subs52 = of_list move_mem52 in
  let move_mem_subs53 = of_list move_mem53 in
  let move_mem_subs54 = of_list move_mem54 in
  let move_mem_subs55 = of_list move_mem55 in

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

  let _ = assert_true (equal (get_subexpr_stmt move56) move_mem_subs1) in
  let _ = assert_true (equal (get_subexpr_stmt move57) move_mem_subs2) in
  let _ = assert_true (equal (get_subexpr_stmt move58) move_mem_subs3) in
  let _ = assert_true (equal (get_subexpr_stmt move59) move_mem_subs4) in
  let _ = assert_true (equal (get_subexpr_stmt move60) move_mem_subs5) in
  let _ = assert_true (equal (get_subexpr_stmt move61) move_mem_subs6) in
  let _ = assert_true (equal (get_subexpr_stmt move62) move_mem_subs7) in
  let _ = assert_true (equal (get_subexpr_stmt move63) move_mem_subs8) in
  let _ = assert_true (equal (get_subexpr_stmt move64) move_mem_subs9) in
  let _ = assert_true (equal (get_subexpr_stmt move65) move_mem_subs10) in
  let _ = assert_true (equal (get_subexpr_stmt move66) move_mem_subs11) in
  let _ = assert_true (equal (get_subexpr_stmt move67) move_mem_subs12) in
  let _ = assert_true (equal (get_subexpr_stmt move68) move_mem_subs13) in
  let _ = assert_true (equal (get_subexpr_stmt move69) move_mem_subs14) in
  let _ = assert_true (equal (get_subexpr_stmt move70) move_mem_subs15) in
  let _ = assert_true (equal (get_subexpr_stmt move71) move_mem_subs16) in
  let _ = assert_true (equal (get_subexpr_stmt move72) move_mem_subs17) in
  let _ = assert_true (equal (get_subexpr_stmt move73) move_mem_subs18) in
  let _ = assert_true (equal (get_subexpr_stmt move74) move_mem_subs19) in
  let _ = assert_true (equal (get_subexpr_stmt move75) move_mem_subs20) in
  let _ = assert_true (equal (get_subexpr_stmt move76) move_mem_subs21) in
  let _ = assert_true (equal (get_subexpr_stmt move77) move_mem_subs22) in
  let _ = assert_true (equal (get_subexpr_stmt move78) move_mem_subs23) in
  let _ = assert_true (equal (get_subexpr_stmt move79) move_mem_subs24) in
  let _ = assert_true (equal (get_subexpr_stmt move80) move_mem_subs25) in
  let _ = assert_true (equal (get_subexpr_stmt move81) move_mem_subs26) in
  let _ = assert_true (equal (get_subexpr_stmt move82) move_mem_subs27) in
  let _ = assert_true (equal (get_subexpr_stmt move83) move_mem_subs28) in
  let _ = assert_true (equal (get_subexpr_stmt move84) move_mem_subs29) in
  let _ = assert_true (equal (get_subexpr_stmt move85) move_mem_subs30) in
  let _ = assert_true (equal (get_subexpr_stmt move86) move_mem_subs31) in
  let _ = assert_true (equal (get_subexpr_stmt move87) move_mem_subs32) in
  let _ = assert_true (equal (get_subexpr_stmt move88) move_mem_subs33) in
  let _ = assert_true (equal (get_subexpr_stmt move89) move_mem_subs34) in
  let _ = assert_true (equal (get_subexpr_stmt move90) move_mem_subs35) in
  let _ = assert_true (equal (get_subexpr_stmt move91) move_mem_subs36) in
  let _ = assert_true (equal (get_subexpr_stmt move92) move_mem_subs37) in
  let _ = assert_true (equal (get_subexpr_stmt move93) move_mem_subs38) in
  let _ = assert_true (equal (get_subexpr_stmt move94) move_mem_subs39) in
  let _ = assert_true (equal (get_subexpr_stmt move95) move_mem_subs40) in
  let _ = assert_true (equal (get_subexpr_stmt move96) move_mem_subs41) in
  let _ = assert_true (equal (get_subexpr_stmt move97) move_mem_subs42) in
  let _ = assert_true (equal (get_subexpr_stmt move98) move_mem_subs43) in
  let _ = assert_true (equal (get_subexpr_stmt move99) move_mem_subs44) in
  let _ = assert_true (equal (get_subexpr_stmt move100) move_mem_subs45) in
  let _ = assert_true (equal (get_subexpr_stmt move101) move_mem_subs46) in
  let _ = assert_true (equal (get_subexpr_stmt move102) move_mem_subs47) in
  let _ = assert_true (equal (get_subexpr_stmt move103) move_mem_subs48) in
  let _ = assert_true (equal (get_subexpr_stmt move104) move_mem_subs49) in
  let _ = assert_true (equal (get_subexpr_stmt move105) move_mem_subs50) in
  let _ = assert_true (equal (get_subexpr_stmt move106) move_mem_subs51) in
  let _ = assert_true (equal (get_subexpr_stmt move107) move_mem_subs52) in
  let _ = assert_true (equal (get_subexpr_stmt move108) move_mem_subs53) in
  let _ = assert_true (equal (get_subexpr_stmt move109) move_mem_subs54) in
  let _ = assert_true (equal (get_subexpr_stmt move110) move_mem_subs55) in

  let jump1 = Jump (Name "dummy") in
  let label1 = Label "dummy" in
  let return1 = Return in

  let _ = assert_true (equal (get_subexpr_stmt jump1) empty) in
  let _ = assert_true (equal (get_subexpr_stmt label1) empty) in
  let _ = assert_true (equal (get_subexpr_stmt return1) empty) in

  ()

let preprocess_test _ =
  let open Ir.Abbreviations in
  let open Cfg.IrData in
  let open Cfg.IrDataStartExit in
  let open IrCfgEq in

  let test input_vs input_es expected_vs expected_es =
    let input = make_graph input_vs input_es in
    let expected = make_graph expected_vs expected_es in
    Pre.preprocess input;
    expected === input
  in

  let norm = Cfg.EdgeData.Normal in

  let ir0 = move (temp "ir0") (zero) in
  let ir1 = move (temp "ir1") (one) in
  let ir2 = move (temp "ir2") (two) in
  let ir3 = move (temp "ir3") (three) in
  let ir4 = move (temp "ir4") (four) in
  let ir5 = move (temp "ir5") (five) in
  let ir6 = move (temp "ir6") (six) in

  let n0 = Cfg.IrCfg.V.create (Node {num=0; ir=ir0}) in
  let n1 = Cfg.IrCfg.V.create (Node {num=1; ir=ir1}) in
  let n2 = Cfg.IrCfg.V.create (Node {num=2; ir=ir2}) in
  let n3 = Cfg.IrCfg.V.create (Node {num=3; ir=ir3}) in
  let n4 = Cfg.IrCfg.V.create (Node {num=4; ir=ir4}) in
  let n5 = Cfg.IrCfg.V.create (Node {num=5; ir=ir5}) in
  let n6 = Cfg.IrCfg.V.create (Node {num=6; ir=ir6}) in

  let d3  = Cfg.IrCfg.V.create (Node {num=3;  ir=Pre.dummy_ir}) in
  let d4  = Cfg.IrCfg.V.create (Node {num=4;  ir=Pre.dummy_ir}) in
  let d5  = Cfg.IrCfg.V.create (Node {num=5;  ir=Pre.dummy_ir}) in
  let d6  = Cfg.IrCfg.V.create (Node {num=6;  ir=Pre.dummy_ir}) in
  let d7  = Cfg.IrCfg.V.create (Node {num=7;  ir=Pre.dummy_ir}) in
  let d8  = Cfg.IrCfg.V.create (Node {num=8;  ir=Pre.dummy_ir}) in
  let d9  = Cfg.IrCfg.V.create (Node {num=9;  ir=Pre.dummy_ir}) in
  let d10 = Cfg.IrCfg.V.create (Node {num=10; ir=Pre.dummy_ir}) in

  let ivs = [n0; n1] in
  let ies = [(n0, norm, n1)] in
  let evs = ivs in
  let ees = ies in
  test ivs ies evs ees;

  let ivs = [n0; n1; n2] in
  let ies = [(n0, norm, n2); (n1, norm, n2)] in
  let evs = ivs @ [d3; d4] in
  let ees = [
    (n0, norm, d3); (d3, norm, n2);
    (n1, norm, d4); (d4, norm, n2);
  ] in
  test ivs ies evs ees;

  let ivs = [n0; n1; n2] in
  let ies = [(n0, norm, n1); (n0, norm, n2)] in
  let evs = ivs in
  let ees = ies in
  test ivs ies evs ees;

  let ivs = [n0; n1; n2; n3] in
  let ies = [(n0, norm, n3); (n1, norm, n3); (n2, norm, n3)] in
  let evs = ivs @ [d4; d5; d6] in
  let ees = [
    (n0, norm, d4); (d4, norm, n3);
    (n1, norm, d5); (d5, norm, n3);
    (n2, norm, d6); (d6, norm, n3);
  ] in
  test ivs ies evs ees;

  let ivs = [n0; n1; n2; n3; n4; n5; n6] in
  let ies = [
    (n0, norm, n1);
    (n0, norm, n2);
    (n1, norm, n3);
    (n2, norm, n3);
    (n3, norm, n4);
    (n3, norm, n5);
    (n4, norm, n6);
    (n5, norm, n6);
  ] in
  let evs = ivs @ [d7; d8; d9; d10] in
  let ees = [
    (n0, norm, n1);
    (n0, norm, n2);
    (n1, norm, d7); (d7, norm, n3);
    (n2, norm, d8); (d8, norm, n3);
    (n3, norm, n4);
    (n3, norm, n5);
    (n4, norm, d9); (d9, norm, n6);
    (n5, norm, d10); (d10, norm, n6);
  ] in
  test ivs ies evs ees;

  ()

module BookExample = struct
  open Ir.Abbreviations
  open Ir.Infix
  module C = Cfg.IrCfg
  module D = Cfg.IrData
  module SE = Cfg.IrDataStartExit
  module E = Pre.ExprSet

  let tru = Cfg.EdgeData.True
  let fls = Cfg.EdgeData.False
  let start = C.V.create SE.Start
  let exit = C.V.create SE.Exit
  let norm = Cfg.EdgeData.Normal
  let node num ir = C.V.create (SE.Node D.{num; ir})

  (* vertexes *)
  let n1 = node 1 (label "node1")
  let n2 = node 2 (move (temp "c") two)
  let n3 = node 3 (label "node3")
  let n4 = node 4 (label "node4")
  let n5 = node 5 (move (temp "a") (temp "b" + temp "c"))
  let n6 = node 6 (label "node6")
  let n7 = node 7 (move (temp "d") (temp "b" + temp "c"))
  let n8 = node 8 (label "node8")
  let n9 = node 9 (move (temp "e") (temp "b" + temp "c"))
  let n10 = node 10 (label "node10")
  let n11 = node 11 (label "node11")
  let vs = [start; n1; n2; n3; n4; n5; n6; n7; n8; n9; n10; n11; exit]

  (* edges *)
  let es_1   = (start, norm, n1)
  let e1_2   = (n1,    fls,  n2)
  let e2_3   = (n2,    norm, n3)
  let e3_4   = (n3,    norm, n4)
  let e4_7   = (n4,    norm, n7)
  let e1_5   = (n1,    tru,  n5)
  let e5_6   = (n5,    norm, n6)
  let e6_7   = (n6,    norm, n7)
  let e7_8   = (n7,    norm, n8)
  let e8_9   = (n8,    fls,  n9)
  let e9_10  = (n9,    norm, n10)
  let e10_11 = (n10,   tru,  n11)
  let e11_e  = (n11,   norm, exit)
  let e8_11  = (n8,    tru,  n11)
  let e10_9  = (n10,   fls,  n9)
  let es = [
    es_1; e1_2; e2_3; e3_4; e4_7; e1_5; e5_6; e6_7; e7_8; e8_9; e9_10; e10_11;
    e11_e; e8_11; e10_9;
  ]

  (* graph *)
  let g = make_graph vs es

  let univ = E.of_list [temp "b" + temp "c"]

  let uses = fun v ->
    match v with
    | SE.Start
    | SE.Exit -> E.empty
    | SE.Node _ ->
        let eq v v' = C.V.compare v v' = 0 in
        if Pervasives.(eq v n5 || eq v n7 || eq v n9)
          then E.of_list [temp "b" + temp "c"]
          else E.empty

  let kills = fun v ->
    if List.mem ~equal:(fun v v' -> C.V.compare v v' = 0) [n2; n5; n7; n9] v
      then E.singleton (temp "b" + temp "c")
      else E.empty

  let busy = fun v ->
    if List.mem ~equal:(fun v v' -> C.V.compare v v' = 0) [n5; n6; n3; n4; n7; n9] v
      then E.singleton (temp "b" + temp "c")
      else E.empty

  let earliest = fun v ->
    if List.mem ~equal:(fun v v' -> C.V.compare v v' = 0) [n3; n5] v
      then E.singleton (temp "b" + temp "c")
      else E.empty

  let latest = fun v ->
    if List.mem ~equal:(fun v v' -> C.V.compare v v' = 0) [n4; n5] v
      then E.singleton (temp "b" + temp "c")
      else E.empty
end

module LoopExample = struct
  open Ir.Abbreviations
  open Ir.Infix
  module C = Cfg.IrCfg
  module D = Cfg.IrData
  module SE = Cfg.IrDataStartExit
  module E = Pre.ExprSet

  let tru = Cfg.EdgeData.True
  let fls = Cfg.EdgeData.False
  let start = C.V.create SE.Start
  let exit = C.V.create SE.Exit
  let norm = Cfg.EdgeData.Normal
  let node num ir = C.V.create (SE.Node D.{num; ir})

  let a = temp "a"
  let n0 = node 0 (move a one)
  let n1 = node 1 (label "head")
  let n2 = node 2 (cjumpone (a < nine) "done")
  let n3 = node 3 (move a (a + a))
  let n4 = node 4 (jump (name "head"))
  let n5 = node 5 (label "done")
  let vs = [start; n0; n1; n2; n3; n4; n5; exit]

  let es_0 = (start, norm, n0)
  let e0_1 = (n0,    norm, n1)
  let e1_2 = (n1,    norm, n2)
  let e2_3 = (n2,    tru,  n3)
  let e3_4 = (n3,    norm, n4)
  let e4_1 = (n4,    norm, n1)
  let e2_5 = (n2,    fls,  n5)
  let e5_e = (n5,    norm, exit)
  let es = [es_0; e0_1; e1_2; e2_3; e3_4; e4_1; e2_5; e5_e]

  let g = make_graph vs es
  let univ = E.of_list [a < nine; a + a]

  let uses = function
    | v when C.V.equal v n2 -> E.singleton (a < nine)
    | v when C.V.equal v n3 -> E.singleton (a + a)
    | _ -> E.empty

  let kills = function
    | v when C.V.equal v n0 -> E.of_list [a < nine; a + a]
    | v when C.V.equal v n3 -> E.of_list [a < nine; a + a]
    | _ -> E.empty

  let busy = fun v ->
    if List.mem ~equal:C.V.equal [n0; n1; n2; n4] v
      then E.singleton (a < nine)
    else if List.mem ~equal:C.V.equal [n3] v
      then E.singleton (a + a)
    else
      E.empty

  let earliest = fun v ->
         if C.V.equal n0 v then E.singleton (a < nine)
    else if C.V.equal n3 v then E.singleton (a + a)
    else                        E.empty

  let latest = fun v ->
         if C.V.equal v n1 then E.singleton (a < nine)
    else if C.V.equal v n3 then E.singleton (a + a)
    else                        E.empty
end

let busy_test _ =
  let open Ir.Abbreviations in
  let open Ir.Infix in
  let module C = Cfg.IrCfg in
  let module D = Cfg.IrData in
  let module SE = Cfg.IrDataStartExit in
  let module E = Pre.ExprSet in

  (* testing helper *)
  let test expected edges g univ uses kills =
    let open EdgeToExprEq in
    let make_edge (src, l, dst) = C.E.create src l dst in
    let edges = List.map edges ~f:make_edge in
    let expected = List.map expected ~f:(fun (edge, expr) -> (make_edge edge, expr)) in
    let actual = Pre.BusyExpr.worklist BusyExprCFG.{g; univ; uses; kills} g in
    expected === List.map edges ~f:(fun edge -> (edge, actual edge))
  in

  (* helpers *)
  let start = C.V.create SE.Start in
  let exit = C.V.create SE.Exit in
  let norm = Cfg.EdgeData.Normal in

  (* (start) -> (x = 1 + 2) -> (exit) *)
  let n0 = C.V.create (SE.Node D.{num=0; ir=(move (temp "x") (one + two))}) in
  let vs = [start; n0; exit] in

  let e0 = (start, norm, n0) in
  let e1 = (n0, norm, exit) in
  let es = [(start, norm, n0); (n0, norm, exit)] in

  let g = make_graph vs es in
  let univ = E.of_list [one + two] in
  let uses = function
    | SE.Start | SE.Exit -> E.empty
    | SE.Node _ -> E.of_list [one + two]
  in
  let kills = fun _ -> E.empty in

  let expected = [
    (e0, E.of_list [one + two]);
    (e1, E.empty);
  ] in
  test expected es g univ uses kills;

  (* small example *)
  let open LoopExample in
  let expected = [
    (es_0, E.empty);
    (e0_1, E.singleton (a < nine));
    (e1_2, E.singleton (a < nine));
    (e2_3, E.singleton (a + a));
    (e3_4, E.singleton (a < nine));
    (e4_1, E.singleton (a < nine));
    (e2_5, E.empty);
    (e5_e, E.empty);
  ] in
  test expected es g univ uses kills;

  (* book example *)
  let open BookExample in
  let bc = E.singleton (temp "b" + temp "c") in
  let expected = [
    (es_1,   E.empty);
    (e1_2,   E.empty);
    (e2_3,   bc);
    (e3_4,   bc);
    (e4_7,   bc);
    (e1_5,   bc);
    (e5_6,   bc);
    (e6_7,   bc);
    (e7_8,   E.empty);
    (e8_9,   bc);
    (e9_10,  E.empty);
    (e10_11, E.empty);
    (e11_e,  E.empty);
    (e8_11,  E.empty);
    (e10_9,  bc);
  ] in
  test expected es g univ uses kills;

  ()

let avail_test _ =
  let open Ir.Abbreviations in
  let open Ir.Infix in
  let module C = Cfg.IrCfg in
  let module D = Cfg.IrData in
  let module SE = Cfg.IrDataStartExit in
  let module E = Pre.ExprSet in

  (* testing helper *)
  let test expected edges g univ busy kills =
    let open EdgeToExprEq in
    let make_edge (src, l, dst) = C.E.create src l dst in
    let edges = List.map edges ~f:make_edge in
    let expected = List.map expected ~f:(fun (edge, expr) -> (make_edge edge, expr)) in
    let actual = Pre.AvailExpr.worklist AvailExprCFG.{g; univ; busy; kills} g in
    expected === List.map edges ~f:(fun edge -> (edge, actual edge))
  in

  (* small example *)
  let open LoopExample in
  let expected = [
    (es_0, E.empty);
    (e0_1, E.singleton (a < nine));
    (e1_2, E.singleton (a < nine));
    (e2_3, E.singleton (a < nine));
    (e3_4, E.of_list [a < nine; a + a]);
    (e4_1, E.of_list [a < nine; a + a]);
    (e2_5, E.singleton (a < nine));
    (e5_e, E.singleton (a < nine));
  ] in
  test expected es g univ busy kills;

  (* book example *)
  let open BookExample in
  let bc = E.singleton (temp "b" + temp "c") in
  let expected = [
    (es_1,   E.empty);
    (e1_2,   E.empty);
    (e2_3,   E.empty);
    (e3_4,   bc);
    (e4_7,   bc);
    (e1_5,   E.empty);
    (e5_6,   bc);
    (e6_7,   bc);
    (e7_8,   bc);
    (e8_9,   bc);
    (e9_10,  bc);
    (e10_11, bc);
    (e11_e,  bc);
    (e8_11,  bc);
    (e10_9,  bc);
  ] in
  test expected es g univ busy kills;

  ()

let post_test _ =
  let open Ir.Abbreviations in
  let open Ir.Infix in
  let module C = Cfg.IrCfg in
  let module D = Cfg.IrData in
  let module SE = Cfg.IrDataStartExit in
  let module E = Pre.ExprSet in

  (* testing helper *)
  let test expected edges g univ uses earliest =
    let open EdgeToExprEq in
    let make_edge (src, l, dst) = C.E.create src l dst in
    let edges = List.map edges ~f:make_edge in
    let expected = List.map expected ~f:(fun (edge, expr) -> (make_edge edge, expr)) in
    let actual = Pre.PostponeExpr.worklist PostponeExprCFG.{g; univ; uses; earliest} g in
    expected === List.map edges ~f:(fun edge -> (edge, actual edge))
  in

  (* small example *)
  let open LoopExample in
  let expected = [
    (es_0, E.empty);
    (e0_1, E.singleton (a < nine));
    (e1_2, E.empty);
    (e2_3, E.empty);
    (e3_4, E.empty);
    (e4_1, E.empty);
    (e2_5, E.empty);
    (e5_e, E.empty);
  ] in
  test expected es g univ uses earliest;

  (* book example *)
  let open BookExample in
  let bc = E.singleton (temp "b" + temp "c") in
  let expected = [
    (es_1,   E.empty);
    (e1_2,   E.empty);
    (e2_3,   E.empty);
    (e3_4,   bc);
    (e4_7,   bc);
    (e1_5,   E.empty);
    (e5_6,   E.empty);
    (e6_7,   E.empty);
    (e7_8,   E.empty);
    (e8_9,   E.empty);
    (e9_10,  E.empty);
    (e10_11, E.empty);
    (e11_e,  E.empty);
    (e8_11,  E.empty);
    (e10_9,  E.empty);
  ] in
  test expected es g univ uses earliest;

  ()

let used_test _ =
  let open Ir.Abbreviations in
  let open Ir.Infix in
  let module C = Cfg.IrCfg in
  let module D = Cfg.IrData in
  let module SE = Cfg.IrDataStartExit in
  let module E = Pre.ExprSet in

  (* testing helper *)
  let test expected edges g uses latest =
    let open EdgeToExprEq in
    let make_edge (src, l, dst) = C.E.create src l dst in
    let edges = List.map edges ~f:make_edge in
    let expected = List.map expected ~f:(fun (edge, expr) -> (make_edge edge, expr)) in
    let actual = Pre.UsedExpr.worklist UsedExprCFG.{uses; latest} g in
    expected === List.map edges ~f:(fun edge -> (edge, actual edge))
  in

  (* book example *)
  let open BookExample in
  let bc = E.singleton (temp "b" + temp "c") in
  let expected = [
    (es_1,   E.empty);
    (e1_2,   E.empty);
    (e2_3,   E.empty);
    (e3_4,   E.empty);
    (e4_7,   bc);
    (e1_5,   E.empty);
    (e5_6,   bc);
    (e6_7,   bc);
    (e7_8,   bc);
    (e8_9,   bc);
    (e9_10,  bc);
    (e10_11, E.empty);
    (e11_e,  E.empty);
    (e8_11,  E.empty);
    (e10_9,  bc);
  ] in
  test expected es g uses latest;

  ()

let enchilada_test _ =
  let open StmtsEq in
  let open Ir.Abbreviations in
  let open Ir.Infix in

  let a = temp "a" in
  let b = temp "b" in
  let c = temp "c" in
  let d = temp "d" in
  let e = temp "e" in

  (* book example *)
  let irs = [
    cjumpone one "n5";  (* B1 *)
    move c two;         (* B2 *)
    jump (name "n7");   (* B3 *)
    label "n5";         (* B5 *)
    move a (b + c);     (* B5 *)
    label "n7";         (* B7 *)
    move d (b + c);     (* B7 *)
    cjumpone one "n11"; (* B8 *)
    label "n9";         (* B9 *)
    move e (b + c);     (* B9 *)
    cjumpone one "n9";  (* B10 *)
    label "n11";        (* B11 *)
  ] in

  Ir_generation.FreshTemp.reset();
  Fresh.FreshLabel.reset();
  let t = Ir.Temp (Ir_generation.FreshTemp.gen 0) in
  let expected = [
    label (Fresh.FreshLabel.gen 0);  (* start *)
    cjumpone one "n5";  (* B1 *)
    move c two;         (* B2 *)
    move t (b + c);     (* B3 *)
    jump (name "n7");   (* B3 *)
    label "n5";         (* B5 *)
    move t (b + c);     (* B5 *)
    move a t;           (* B5 *)
    label "n7";         (* B7 *)
    move d t;           (* B7 *)
    cjumpone one "n11"; (* B8 *)
    label "n9";         (* B9 *)
    move e t;           (* B9 *)
    cjumpone one "n9";  (* B10 *)
    label "n11";        (* B11 *)
  ] in
  expected === Pre.pre irs;
  ()

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* ! DON'T FORGET TO ADD YOUR TESTS HERE                                     ! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let main () =
    "suite" >::: [
      "get_subexpr_test" >:: get_subexpr_test;
      "preprocess_test"  >:: preprocess_test;
      "busy_test"        >:: busy_test;
      "avail_test"       >:: avail_test;
      "post_test"        >:: post_test;
      "used_test"        >:: used_test;
      "enchilada_test"   >:: enchilada_test;
    ] |> run_test_tt_main

let _ = main ()

