open Core.Std
open Typecheck
open OUnit
open Expr
open Stmt
open Sigma
open Ast.S

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* ! ADD YOUR TESTS ADD YOUR TESTS ADD YOUR TESTS ADD YOUR TESTS ADD YOUR TEST *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let (>>=) = Result.(>>=)
let (>>|) = Result.(>>|)

let assert_true (b: bool) : unit =
  assert_equal b true

(* Dummy pos *)
let p = (-1, -1)
let empty_c = Context.empty

let (|-) c e = (c, e)

(* If <: is subtype, then =: is equal type. *)
let (e=:) ((c, e): context * Pos.expr) (t: Expr.t) : unit =
  let b = is_ok (expr_typecheck c e >>| fun e' -> assert_equal (fst e') t) in
  assert_true b

let test_expr () =
  empty_c |- (p, Int 42L) e=: IntT;
  ()


let (f=:) ((c, e): context * Pos.callable) (func_t: Expr.t * Expr.t) : unit =
	let b = is_ok (fst_func_pass c e >>= fun gamma ->
								 snd_func_pass gamma e >>= fun (t,_) -> Ok (assert_equal t func_t)) in
	assert_true b
	

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* ! ADD YOUR TESTS ADD YOUR TESTS ADD YOUR TESTS ADD YOUR TESTS ADD YOUR TEST *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let main () =
    "suite" >::: [
        "test_expr" >:: test_expr;
    ] |> run_test_tt_main

let _ = main ()

