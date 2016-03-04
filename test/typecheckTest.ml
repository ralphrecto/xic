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
let empty = Context.empty

let (|-) c e = (c, e)

module TestExpr = struct
  (* If <: is subtype, then =: is equal type. *)
  let (=:) ((c, e): context * Pos.expr) (t: Expr.t) : unit =
    let b = is_ok (expr_typecheck c e >>| fun e' -> assert_equal (fst e') t) in
    assert_true b
end

let test_expr () =
    let open Pos in
    let open TestExpr in
    let one = int 1L in
    let two = int 1L in
    let tru = bool true in
    let fls = bool false in

    empty |- one =: IntT;
    empty |- tru =: BoolT;
    empty |- fls =: BoolT;
    empty |- string "a" =: ArrayT IntT;
    empty |- char 'c' =: IntT;
    empty |- (one + two) =: IntT;
    empty |- (one - two) =: IntT;
    empty |- (one * two) =: IntT;
    empty |- (one *>> two) =: IntT;
    empty |- (one / two) =: IntT;
    empty |- (one % two) =: IntT;
    empty |- (~~ one) =: IntT;
    empty |- (one == two) =: BoolT;
    empty |- (one != two) =: BoolT;
    empty |- (one < two) =: BoolT;
    empty |- (one <= two) =: BoolT;
    empty |- (one > two) =: BoolT;
    empty |- (one >= two) =: BoolT;
    empty |- (!tru) =: BoolT;
    empty |- (tru == fls) =: BoolT;
    empty |- (tru != fls) =: BoolT;
    empty |- (tru & fls) =: BoolT;
    empty |- (tru || fls) =: BoolT;
    ()

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* ! DON'T FORGET TO ADD YOUR TESTS HERE                                     ! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let main () =
    "suite" >::: [
        "test_expr" >:: test_expr;
    ] |> run_test_tt_main

let _ = main ()

