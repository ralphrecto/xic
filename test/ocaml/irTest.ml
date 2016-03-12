open Core.Std
open Ir
open Ir_generation
open OUnit
open TestUtil

module Fresh = struct
  let t n =
    Temp (sprintf "temp%d" n)

  let l n =
    Label (sprintf "label%d" n)
end

(* By default, OUnit's assert_equal function doesn't print out anything useful
 * when its assertion fails. For example, 1 === 2 won't print either 1 or 2!
 * This module specializes === to a bunch of data types that do print
 * things out when tests fail! For example, if you want to check that two
 * expressions are equal, just do ExprEq.(a === b)` or `let open ExprEq in a
 * === b`. *)
module ExprEq = struct
  let (===) (a: expr) (b: expr) : unit =
    assert_equal ~printer:string_of_expr a b
end

module StmtEq = struct
  let (===) (a: stmt) (b: stmt) : unit =
    assert_equal ~printer:string_of_stmt a b
end

module StmtsEq = struct
  let (===) (a: stmt list) (b: stmt list) : unit =
    assert_equal ~printer:string_of_stmts a b
end

module PairEq = struct
  let (===) (a: stmt list * expr) (b: stmt list * expr) : unit =
    let printer (ss, e) = sprintf "<%s; %s>" (string_of_stmts ss) (string_of_expr e) in
    assert_equal ~printer a b
end

let test_lower_expr () =
  let open PairEq in
  let open Fresh in

  let one = Const 1L in
  let two = Const 2L in
  let stmts = [Return; Return; CJump (one, "t", "f")] in
  let seq = Seq stmts in
  let eseq e = ESeq (seq, e) in

  (* Const *)
  ([], Const 42L) === lower_expr (Const 42L);

  (* Name *)
  ([], Name "foo") === lower_expr (Name "foo");

  (* Temp *)
  ([], Temp "foo") === lower_expr (Temp "foo");

  (* Mem *)
  (stmts, Mem (one, NORMAL)) === lower_expr (Mem (eseq one, NORMAL));

  (* ESeq *)
  (stmts @ stmts, one) === lower_expr (ESeq (seq, eseq one));

  (* BinOp *)
  (stmts@[Move (t 0, one)]@stmts, BinOp(t 0, ADD, one))
  ===
  lower_expr (BinOp (eseq one, ADD, eseq one));

  (* Call *)
  (stmts@[Move (t 1, Name "f")]@
   stmts@[Move (t 2, one)]@
   stmts@[Move (t 3, two)]@
   [Move (t 4, Call(t 1, [t 2; t 3], 0))],
   t 4)
  ===
  lower_expr (Call (eseq (Name "f"), [eseq one; eseq two], 0));

  ()

let test_lower_stmt () =
  let open StmtsEq in
  let open Fresh in

  let one = Const 1L in
  let two = Const 2L in
  let stmts = [Return; Return; CJump (one, "t", "f")] in
  let seq = Seq stmts in
  let eseq e = ESeq (seq, e) in

  (* Label *)
  [Label "l"] === lower_stmt (Label "l");

  (* Return *)
  [Return] === lower_stmt Return;

  (* CJump *)
  stmts@[CJump (one, "t", "f")] === lower_stmt (CJump (eseq one, "t", "f"));

  (* Jump *)
  stmts@[Jump one] === lower_stmt (Jump (eseq one));

  (* Exp *)
  stmts === lower_stmt (Exp (eseq one));

	(* Move *)
  stmts@[Move (t 5, two)]@stmts@[Move (one, t 5)]
  ===
  lower_stmt (Move (eseq one, eseq two));

	(* Seq ss *)
  stmts@stmts@stmts
  ===
  lower_stmt (Seq [Seq stmts; Seq stmts; Seq stmts]);

  ()

let test_reorder () =
	(* labels *)
	let l1 = Ir.Label "l1" in
	let l2 = Ir.Label "l2" in
	let l3 = Ir.Label "l3" in
	let l4 = Ir.Label "l4" in
	let l5 = Ir.Label "l5" in

	(* statements *)
	let s1 = CJump (Const 1L, "l2", "l3") in
	let s2 = CJump (Const 1L, "l2", "l4") in
	let s3 = Jump (Name "l2") in
	let s4 = Jump (Name "l5") in
	let s5 = Return in

	let s_list = [l1; s1; l2; s2; l3; s3; l4; s4; l5; s5] in

	let reordered = block_reorder s_list in

	(* blocks *)
	let b1 = Block ("l1", [CJumpOne (Const 1L, "l2")]) in
	let b2 = Block ("l3", []) in
	let b3 = Block ("l2", [CJumpOne (Const 1L, "l2")]) in
	let b4 = Block ("l4", []) in
	let b5 = Block ("l5", [Return]) in

	let expected = [b1; b2; b3; b4; b5] in

	assert_equal reordered expected

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* ! DON'T FORGET TO ADD YOUR TESTS HERE                                     ! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let main () =
		"suite" >::: [
			"test_lower_expr" >:: test_lower_expr;
			"test_lower_stmt" >:: test_lower_stmt;
			"test_reorder" >:: test_reorder;
    ] |> run_test_tt_main

let _ = main ()
