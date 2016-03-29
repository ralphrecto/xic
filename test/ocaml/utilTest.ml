open Core.Std
open OUnit
open TestUtil
open Util

let test_init () =
  [] === init [];
  [] === init [1];
  [1] === init [1;2];
  [1;2;3;4;5] === init [1;2;3;4;5;6];
  ()

let test_join () =
  "" === join [];
  "a" === join ["a"];
  "a\nb" === join ["a";"b"];
  "a\nb\nc\nd\ne" === join ["a";"b";"c";"d";"e"];
  ()

let test_commas () =
  "" === commas [];
  "a" === commas ["a"];
  "a,b" === commas ["a";"b"];
  "a,b,c,d,e" === commas ["a";"b";"c";"d";"e"];
  ()

let test_pairs () =
  [] === pairs [];
  [] === pairs [1];
  [(1,2)] === pairs [1;2];
  [(1,2);(2,3)] === pairs [1;2;3];
  [(1,2);(2,3);(3,4)] === pairs [1;2;3;4];
  ()

let test_all_eq () =
  let (===) xs ys = assert_true  (all_eq xs ys) in
  let (=/=) xs ys = assert_false (all_eq xs ys) in

  [] === [];
  [1] === [1];
  [1;2] === [1;2];
  [2;1] === [1;2];
  [1;2] === [2;1];
  [2;1] === [2;1];
  [1;2;3] === [1;2;3];
  [2;1;3] === [3;2;1];

  [] =/= [1];
  [1] =/= [];
  [1] =/= [2];
  [1] =/= [1;2];
  [1;2] =/= [1];
  [1;2] =/= [2;3];
  ()

let test_get_and_incr () =
  let x = ref 0 in
  0 === get_and_incr x;
  1 === get_and_incr x;
  2 === get_and_incr x;
  3 === get_and_incr x;
  x := 0;
  0 === get_and_incr x;
  1 === get_and_incr x;
  2 === get_and_incr x;
  3 === get_and_incr x;
  ()

let test_string_of_int () =
  Some 1 === int_of_string "1";
  Some 2 === int_of_string "2";
  Some (-1) === int_of_string "-1";
  None === int_of_string "foo";
  ()

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* ! DON'T FORGET TO ADD YOUR TESTS HERE                                     ! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let main () =
    "suite" >::: [
      "test_init"          >:: test_init;
      "test_join"          >:: test_join;
      "test_commas"        >:: test_commas;
      "test_pairs"         >:: test_pairs;
      "test_all_eq"        >:: test_all_eq;
      "test_get_and_incr"  >:: test_get_and_incr;
      "test_string_of_int" >:: test_string_of_int;
    ] |> run_test_tt_main

let _ = main ()
