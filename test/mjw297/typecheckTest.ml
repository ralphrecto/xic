open Core.Std
open Typecheck
open OUnit

(* Dummy pos *)
(* Expected, actual *)
let%test_unit "test_expr" = 
    failwith "foo"
    (*assert_equal (IntT 42) (expr_typecheck empty_c (p, Int 42));
    ()*)

Ppx_inline_test_lib.Runtime.summarize ()

