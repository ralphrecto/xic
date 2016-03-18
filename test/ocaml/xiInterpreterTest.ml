open Core.Std
open OUnit
open TestUtil
open Xi_interpreter

module ValueEq = struct
  let (===) (a: value) (b: value) : unit =
    assert_equal ~printer:string_of_value a b
end

let test_eval_expr () =
  let open ValueEq in
  let open Typecheck.Abbreviations in
  let c = String.Map.empty in

  Int 1L === eval_expr c (one);
  Int 2L === eval_expr c (two);

  Bool true === eval_expr c (tru);
  Bool false === eval_expr c (fls);

  String "foo" === eval_expr c (string "foo");
  String "bar" === eval_expr c (string "bar");

  Char 'a' === eval_expr c (char 'a');
  Char 'b' === eval_expr c (char 'b');

  Array [] === eval_expr c (arr[]);
  Array [Int 1L] === eval_expr c (arr[one]);
  Array [Int 1L;Int 2L] === eval_expr c (arr[one;two]);
  Array [Int 2L;Int 1L] === eval_expr c (arr[two;one]);

  Int 1L     === eval_expr c (three -   two);
  Int 6L     === eval_expr c (three *   two);
  Int 0L     === eval_expr c (three *>> two);
  Int 1L     === eval_expr c (three /   two);
  Int 1L     === eval_expr c (three %   two);
  Int 5L     === eval_expr c (three +   two);
  Bool false === eval_expr c (three <   two);
  Bool false === eval_expr c (three <=  two);
  Bool true  === eval_expr c (three >=  two);
  Bool true  === eval_expr c (three >   two);
  Bool false === eval_expr c (three ==  two);
  Bool true  === eval_expr c (three !=  two);
  Bool false === eval_expr c (tru   &   fls);
  Bool true  === eval_expr c (tru   ||  fls);
  Bool false === eval_expr c (tru   ==  fls);
  Bool true  === eval_expr c (tru   !=  fls);

  Bool true  === eval_expr c (!fls);
  Bool false === eval_expr c (!tru);
  Int (-1L)  === eval_expr c (~~one);
  Int (-2L)  === eval_expr c (~~two);

  Int 0L === eval_expr c (index (arr[zero;one;two;three;four;five]) zero);
  Int 1L === eval_expr c (index (arr[zero;one;two;three;four;five]) one);
  Int 2L === eval_expr c (index (arr[zero;one;two;three;four;five]) two);
  Int 3L === eval_expr c (index (arr[zero;one;two;three;four;five]) three);
  Int 4L === eval_expr c (index (arr[zero;one;two;three;four;five]) four);
  Int 5L === eval_expr c (index (arr[zero;one;two;three;four;five]) five);
  Int 1L === eval_expr c (index (index (arr[arr[one]]) zero) zero);
  Int 1L === eval_expr c (index (index (index (arr[arr[arr[one]]]) zero) zero) zero);

  Int 0L === eval_expr c (length (arr[]));
  Int 1L === eval_expr c (length (arr[one]));
  Int 2L === eval_expr c (length (arr[one;one]));
  Int 3L === eval_expr c (length (arr[one;one;two]));

  ()

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* ! DON'T FORGET TO ADD YOUR TESTS HERE                                     ! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let main () =
    "suite" >::: [
      "test_eval_expr" >:: test_eval_expr;
    ] |> run_test_tt_main

let _ = main ()
