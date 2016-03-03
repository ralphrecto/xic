open Core.Std
open Typecheck
open OUnit
open Expr
open Stmt
open Sigma

(* Dummy pos *)
let p = (-1, -1) 
(* Expected, actual *)
let empty_c = Context.empty


let test_expr () = 
    (*assert_equal (IntT) (expr_typecheck empty_c (p, Int 42));*)
    assert_equal () ();
    ()

let main () =
    "suite" >::: [
        "test_expr" >:: test_expr;
    ] |> run_test_tt_main

(*Ppx_inline_test_lib.Runtime.summarize ()*)
(*let () = Pa_ounit_lib.Runtime.summarize ()*)
let _ = main ()

