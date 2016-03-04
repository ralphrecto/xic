open Core.Std
open Typecheck
open OUnit
open Expr
open Stmt
open Sigma
open Ast.S

let (>>=) = Result.(>>=)
let (>>|) = Result.(>>|)

let assert_true (b: bool) : unit =
  assert_equal b true

(* Dummy pos *)
let p = (-1, -1)
let empty = Context.empty

let (|-) c e = (c, e)

(* If <: is subtype, then =: is equal type. *)
let (=:) ((c, e): context * Pos.expr) (t: Expr.t) : unit =
  let b = is_ok (expr_typecheck c e >>| fun e' -> assert_equal (fst e') t) in
  assert_true b

let test_expr () =
    let open Pos in
    empty |- int 42L =: IntT;
    ()

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* ! DON'T FORGET TO ADD YOUR TESTS HERE                                     ! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let main () =
    "suite" >::: [
        "test_expr" >:: test_expr;
    ] |> run_test_tt_main

let _ = main ()

