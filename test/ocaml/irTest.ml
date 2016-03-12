open Core.Std
open Ir
open Ir_generation
open OUnit
open TestUtil

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
			"test_reorder" >:: test_reorder;
    ] |> run_test_tt_main

let _ = main ()
