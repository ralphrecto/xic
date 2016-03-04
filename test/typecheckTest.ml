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
    let err = begin
      expr_typecheck c e >>| fun e' ->
      let t' = fst e' in
      if t' = t then
        ()
      else begin
        printf ">>> %s : %s != %s\n" (Ast.string_of_expr e')
                                     (to_string t')
                                     (to_string t);
        assert_equal t' t
      end
    end in
    match err with
    | Ok _ -> ()
    | Error (_, s) -> begin
        printf ">>> %s =: %s errored with %s\n" (Ast.string_of_expr e) (to_string t) s;
        assert_true false
    end

  let (=/=) (c: context) (e: Pos.expr) : unit =
    begin
      expr_typecheck c e >>| fun e' ->
      printf ">>> %s : %s; expected error\n" (Ast.string_of_expr e')
                                             (to_string (fst e'));
      assert_true false
    end
    |> is_error
    |> assert_true
end

let one   = Pos.(int 1L)
let two   = Pos.(int 1L)
let three = Pos.(int 1L)
let tru   = Pos.(bool true)
let fls   = Pos.(bool false)

let test_expr () =
    let open Pos in
    let open TestExpr in

    empty |- (one) =: IntT;
    empty |- (tru) =: BoolT;
    empty |- (fls) =: BoolT;
    empty |- (string "a") =: ArrayT IntT;
    empty |- (char 'c') =: IntT;

    empty |- (one + two) =: IntT;
    empty |- (one - two) =: IntT;
    empty |- (one * two) =: IntT;
    empty |- (one *>> two) =: IntT;
    empty |- (one / two) =: IntT;
    empty |- (one % two) =: IntT;
    empty |- (~~ one) =: IntT;

    empty =/= (tru + two);
    empty =/= (tru - two);
    empty =/= (tru * two);
    empty =/= (tru *>> two);
    empty =/= (tru / two);
    empty =/= (tru % two);
    empty =/= (one + tru);
    empty =/= (one - tru);
    empty =/= (one * tru);
    empty =/= (one *>> tru);
    empty =/= (one / tru);
    empty =/= (one % tru);
    empty =/= (tru + tru);
    empty =/= (tru - tru);
    empty =/= (tru * tru);
    empty =/= (tru *>> tru);
    empty =/= (tru / tru);
    empty =/= (tru % tru);
    empty =/= (~~ tru);

    empty |- (one == two) =: BoolT;
    empty |- (one != two) =: BoolT;
    empty |- (one < two) =: BoolT;
    empty |- (one <= two) =: BoolT;
    empty |- (one > two) =: BoolT;
    empty |- (one >= two) =: BoolT;
    empty |- (!tru) =: BoolT;

    empty =/= (tru < two);
    empty =/= (tru <= two);
    empty =/= (tru > two);
    empty =/= (tru >= two);
    empty =/= (one < tru);
    empty =/= (one <= tru);
    empty =/= (one > tru);
    empty =/= (one >= tru);
    empty =/= (tru < tru);
    empty =/= (tru <= tru);
    empty =/= (tru > tru);
    empty =/= (tru >= tru);
    empty =/= (!one);

    empty |- (tru == fls) =: BoolT;
    empty |- (tru != fls) =: BoolT;
    empty |- (tru & fls) =: BoolT;
    empty |- (tru || fls) =: BoolT;

    empty =/= (one & fls);
    empty =/= (one || fls);
    empty =/= (tru & one);
    empty =/= (tru || one);
    empty =/= (one & one);
    empty =/= (one || one);

    empty |- (arr[]    == arr[]) =: BoolT;
    empty |- (arr[one] == arr[]) =: BoolT;
    empty |- (arr[]    == arr[one]) =: BoolT;
    empty |- (arr[one] == arr[one]) =: BoolT;
    empty |- (arr[]    != arr[]) =: BoolT;
    empty |- (arr[one] != arr[]) =: BoolT;
    empty |- (arr[]    != arr[one]) =: BoolT;
    empty |- (arr[one] != arr[one]) =: BoolT;

    empty =/= (arr[one] == arr[tru]);
    empty =/= (arr[tru] == arr[one]);
    empty =/= (arr[one] == arr[arr[one]]);
    empty =/= (arr[arr[one]] == arr[one]);
    empty =/= (arr[tru] == arr[arr[one]]);
    empty =/= (arr[arr[tru]] == arr[one]);
    empty =/= (arr[one] != arr[tru]);
    empty =/= (arr[tru] != arr[one]);
    empty =/= (arr[one] != arr[arr[one]]);
    empty =/= (arr[arr[one]] != arr[one]);
    empty =/= (arr[tru] != arr[arr[one]]);
    empty =/= (arr[arr[tru]] != arr[one]);

    empty |- (length (arr[])) =: IntT;
    empty |- (length (arr[one])) =: IntT;
    empty =/= (length one);
    empty =/= (length tru);

    empty |- (arr []) =: EmptyArray;
    empty |- (arr [one]) =: ArrayT IntT;
    empty |- (arr [one; two]) =: ArrayT IntT;
    empty |- (arr [one; two; three]) =: ArrayT IntT;
    empty |- (arr [one; two; three; one]) =: ArrayT IntT;
    empty |- (arr [tru]) =: ArrayT BoolT;
    empty |- (arr [tru; fls]) =: ArrayT BoolT;
    empty |- (arr [tru; fls; tru]) =: ArrayT BoolT;
    empty |- (arr [tru; fls; tru; fls]) =: ArrayT BoolT;
    empty |- (arr [arr[tru]]) =: ArrayT (ArrayT BoolT);
    empty |- (arr [arr[tru]; arr[tru]]) =: ArrayT (ArrayT BoolT);
    empty |- (arr [arr[]; arr[tru]]) =: ArrayT (ArrayT BoolT);
    empty |- (arr [arr[]; arr[tru]; arr[tru]]) =: ArrayT (ArrayT BoolT);
    empty |- (arr [arr[tru]; arr[]]) =: ArrayT (ArrayT BoolT);
    empty |- (arr [arr[tru]; arr[]; arr[tru]]) =: ArrayT (ArrayT BoolT);
    empty |- (arr [arr[tru]; arr[tru]; arr[]]) =: ArrayT (ArrayT BoolT);
    empty |- (arr [arr[tru]]) =: ArrayT (ArrayT BoolT);
    empty |- (arr [arr[tru]; arr[tru]]) =: ArrayT (ArrayT BoolT);
    empty |- (arr [arr[]; arr[tru]]) =: ArrayT (ArrayT BoolT);
    empty |- (arr [arr[]; arr[tru]; arr[tru]]) =: ArrayT (ArrayT BoolT);
    empty |- (arr [arr[tru]; arr[]]) =: ArrayT (ArrayT BoolT);
    empty |- (arr [arr[tru]; arr[]; arr[tru]]) =: ArrayT (ArrayT BoolT);
    empty |- (arr [arr[tru]; arr[tru]; arr[]]) =: ArrayT (ArrayT BoolT);
    empty |- (arr [arr [arr[tru]]]) =: ArrayT (ArrayT (ArrayT BoolT));
    empty |- (arr [arr [arr[tru]; arr[tru]]]) =: ArrayT (ArrayT (ArrayT BoolT));
    empty |- (arr [arr [arr[]; arr[tru]]]) =: ArrayT (ArrayT (ArrayT BoolT));
    empty |- (arr [arr [arr[]; arr[tru]; arr[tru]]]) =: ArrayT (ArrayT (ArrayT BoolT));
    empty |- (arr [arr [arr[tru]; arr[]]]) =: ArrayT (ArrayT (ArrayT BoolT));
    empty |- (arr [arr [arr[tru]; arr[]; arr[tru]]]) =: ArrayT (ArrayT (ArrayT BoolT));
    empty |- (arr [arr [arr[tru]; arr[tru]; arr[]]]) =: ArrayT (ArrayT (ArrayT BoolT));

    empty =/= (arr[one;tru]);
    empty =/= (arr[tru;one]);
    empty =/= (arr[one;tru;one]);
    empty =/= (arr[tru;one;tru]);
    empty =/= (arr[one;one;tru]);
    empty =/= (arr[tru;tru;one]);
    empty =/= (arr[one;arr[]]);
    empty =/= (arr[arr[];one]);
    empty =/= (arr[one;arr[];one]);
    empty =/= (arr[arr[];one;arr[]]);
    empty =/= (arr[one;one;arr[]]);
    empty =/= (arr[one;arr[];arr[]]);
    empty =/= (arr[arr[one];arr[tru]]);
    empty =/= (arr[arr[tru];arr[one]]);
    empty =/= (arr[arr[one];arr[tru];arr[one]]);
    empty =/= (arr[arr[tru];arr[one];arr[tru]]);
    empty =/= (arr[arr[one];arr[one];arr[tru]]);
    empty =/= (arr[arr[tru];arr[tru];arr[one]]);
    empty =/= (arr[arr[one];arr[arr[]]]);
    empty =/= (arr[arr[arr[]];arr[one]]);
    empty =/= (arr[arr[one];arr[arr[]];arr[one]]);
    empty =/= (arr[arr[arr[]];arr[one];arr[arr[]]]);
    empty =/= (arr[arr[one];arr[one];arr[arr[]]]);
    empty =/= (arr[arr[one];arr[arr[]];arr[arr[]]]);

    empty |- (index (arr[one]) one) =: IntT;
    empty |- (index (arr[tru]) one) =: BoolT;
    empty |- (index (arr[arr[one]]) one) =: ArrayT IntT;
    empty |- (index (arr[arr[one]]) one) =: ArrayT IntT;
    empty |- (index (arr[arr[arr[tru]]]) one) =: ArrayT (ArrayT BoolT);
    empty |- (index (arr[arr[arr[tru]]]) one) =: ArrayT (ArrayT BoolT);

    empty =/= (index (arr[one]) tru);
    empty =/= (index (arr[tru]) tru);
    empty =/= (index (arr[arr[one]]) tru);
    empty =/= (index (arr[arr[one]]) tru);
    empty =/= (index (arr[arr[arr[tru]]]) tru);
    empty =/= (index (arr[arr[arr[tru]]]) tru);
    empty =/= (index (arr[one]) (arr[]));
    empty =/= (index (arr[tru]) (arr[]));
    empty =/= (index (arr[arr[one]]) (arr[]));
    empty =/= (index (arr[arr[one]]) (arr[]));
    empty =/= (index (arr[arr[arr[tru]]]) (arr[]));
    empty =/= (index (arr[arr[arr[tru]]]) (arr[]));
    empty =/= (index tru one);

    empty |- (arr[] + arr[]) =: EmptyArray;
    empty |- (arr[] + arr[one]) =: ArrayT IntT;
    empty |- (arr[one] + arr[]) =: ArrayT IntT;
    empty |- (arr[one] + arr[one]) =: ArrayT IntT;
    empty |- (arr[one] + arr[one]) =: ArrayT IntT;
    empty |- (arr[one;one] + arr[one]) =: ArrayT IntT;
    empty |- (arr[one] + arr[one;one]) =: ArrayT IntT;
    empty |- (arr[one;one] + arr[one;one]) =: ArrayT IntT;
    empty |- (arr[arr[]] + arr[]) =: ArrayT EmptyArray;
    empty |- (arr[arr[arr[]]] + arr[]) =: ArrayT (ArrayT EmptyArray);
    empty |- (arr[arr[arr[arr[]]]] + arr[]) =: ArrayT (ArrayT (ArrayT EmptyArray));
    empty |- (arr[arr[]] + arr[arr[]]) =: ArrayT EmptyArray;
    empty |- (arr[arr[arr[]]] + arr[arr[]]) =: ArrayT (ArrayT EmptyArray);
    empty |- (arr[arr[arr[arr[]]]] + arr[arr[]]) =: ArrayT (ArrayT (ArrayT EmptyArray));
    empty |- (arr[arr[arr[]]] + arr[arr[arr[]]]) =: ArrayT (ArrayT EmptyArray);
    empty |- (arr[arr[arr[arr[]]]] + arr[arr[arr[]]]) =: ArrayT (ArrayT (ArrayT EmptyArray));
    empty |- (arr[arr[]] + arr[arr[tru]]) =: ArrayT (ArrayT BoolT);
    empty |- (arr[arr[]] + arr[arr[arr[tru]]]) =: ArrayT (ArrayT (ArrayT BoolT));
    empty |- (arr[arr[arr[]]] + arr[arr[arr[tru]]]) =: ArrayT (ArrayT (ArrayT BoolT));

    ()

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* ! DON'T FORGET TO ADD YOUR TESTS HERE                                     ! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let main () =
    "suite" >::: [
        "test_expr" >:: test_expr;
    ] |> run_test_tt_main

let _ = main ()

