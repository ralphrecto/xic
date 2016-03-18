open Core.Std
open OUnit
open TestUtil
open Xi_interpreter

module ValueEq = struct
  let (===) (a: value) (b: value) : unit =
    assert_equal ~printer:string_of_value a b
end

(* Context helpers *)
let empty = String.Map.empty

let gam (xs: (string * store option) list) : context =
  String.Map.of_alist_exn xs

let vals (vs: (string * value) list) : context =
  gam (List.map vs ~f:(fun (v, t) -> (v, Some (Value t))))

let funcs (fs: (string * (string option) list * Typecheck.stmt) list) : context =
  gam (List.map fs ~f:(fun (f, args, s) -> (f, Some (Function (args, s)))))

let test_eval_expr () =
  let open ValueEq in
  let open Typecheck.Abbreviations in

  Int 1L === eval_expr empty (one);
  Int 2L === eval_expr empty (two);

  Int 1L === eval_expr empty (tru);
  Int 0L === eval_expr empty (fls);

  Array [Int 102L;Int 111L;Int 111L] === eval_expr empty (string "foo");

  Int 97L === eval_expr empty (char 'a');
  Int 98L === eval_expr empty (char 'b');

  Array [] === eval_expr empty (arr[]);
  Array [Int 1L] === eval_expr empty (arr[one]);
  Array [Int 1L;Int 2L] === eval_expr empty (arr[one;two]);
  Array [Int 2L;Int 1L] === eval_expr empty (arr[two;one]);

  Int 1L === eval_expr empty (three -   two);
  Int 6L === eval_expr empty (three *   two);
  Int 0L === eval_expr empty (three *>> two);
  Int 1L === eval_expr empty (three /   two);
  Int 1L === eval_expr empty (three %   two);
  Int 5L === eval_expr empty (three +   two);
  Int 0L === eval_expr empty (three <   two);
  Int 0L === eval_expr empty (three <=  two);
  Int 1L === eval_expr empty (three >=  two);
  Int 1L === eval_expr empty (three >   two);
  Int 0L === eval_expr empty (three ==  two);
  Int 1L === eval_expr empty (three !=  two);
  Int 0L === eval_expr empty (tru   &   fls);
  Int 1L === eval_expr empty (tru   ||  fls);
  Int 0L === eval_expr empty (tru   ==  fls);
  Int 1L === eval_expr empty (tru   !=  fls);

  Int 1L === eval_expr empty (!fls);
  Int 0L === eval_expr empty (!tru);
  Int (-1L) === eval_expr empty (~~one);
  Int (-2L) === eval_expr empty (~~two);

  Array [] === eval_expr empty (arr[] + arr[]);
  Array [Int 1L] === eval_expr empty (arr[one] + arr[]);
  Array [Int 1L] === eval_expr empty (arr[] + arr[one]);
  Array [Int 1L;Int 2L] === eval_expr empty (arr[one] + arr[two]);
  Array [Int 1L;Int 2L;Int 3L] === eval_expr empty (arr[one;two] + arr[three]);
  Array [Int 1L;Int 2L;Int 3L] === eval_expr empty (arr[one] + arr[two;three]);

  Int 0L === eval_expr empty (index (arr[zero;one;two;three;four;five]) zero);
  Int 1L === eval_expr empty (index (arr[zero;one;two;three;four;five]) one);
  Int 2L === eval_expr empty (index (arr[zero;one;two;three;four;five]) two);
  Int 3L === eval_expr empty (index (arr[zero;one;two;three;four;five]) three);
  Int 4L === eval_expr empty (index (arr[zero;one;two;three;four;five]) four);
  Int 5L === eval_expr empty (index (arr[zero;one;two;three;four;five]) five);
  Int 1L === eval_expr empty (index (index (arr[arr[one]]) zero) zero);
  Int 1L === eval_expr empty (index (index (index (arr[arr[arr[one]]]) zero) zero) zero);

  Int 0L === eval_expr empty (length (arr[]));
  Int 1L === eval_expr empty (length (arr[one]));
  Int 2L === eval_expr empty (length (arr[one;one]));
  Int 3L === eval_expr empty (length (arr[one;one;two]));

  let c = vals ["x",Int 1L; "y",Array [Int 1L; Int 2L]] in
  Int 1L === eval_expr c (id "x");
  Array [Int 1L; Int 2L] === eval_expr c (id "y");

  let c = funcs [
    "f1", [], return [one];
    "f2", [Some "x"], return [one];
    "f3", [None], return [one];
    "f4", [Some "x"], return [id "x"];
    "f5", [Some "x"; Some "y"], return [id "x" + id "y"];
    "f6", [Some "x"; Some "y"], return [id "x" + id "y"; id "x"; zero];
  ] in
  Int 1L === eval_expr c (funccall "f1" []);
  Int 1L === eval_expr c (funccall "f2" [one]);
  Int 1L === eval_expr c (funccall "f3" [one]);
  Int 2L === eval_expr c (funccall "f4" [two]);
  Int 5L === eval_expr c (funccall "f5" [two;three]);
  Tuple [Int 5L;Int 2L;Int 0L] === eval_expr c (funccall "f6" [two;three]);

  ()

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* ! DON'T FORGET TO ADD YOUR TESTS HERE                                     ! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let main () =
    "suite" >::: [
      "test_eval_expr" >:: test_eval_expr;
    ] |> run_test_tt_main

let _ = main ()
