open Core.Std
open OUnit
open TestUtil

let test_pairs () =
  let pairs = Util.pairs in
  [] === pairs [];
  [] === pairs [1];
  [(1,2)] === pairs [1;2];
  [(1,2);(2,3)] === pairs [1;2;3];
  [(1,2);(2,3);(3,4)] === pairs [1;2;3;4];
  ()

let test_all_eq () =
  let (===) xs ys = assert_true  (Util.all_eq xs ys) in
  let (=/=) xs ys = assert_false (Util.all_eq xs ys) in

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

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* ! DON'T FORGET TO ADD YOUR TESTS HERE                                     ! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let main () =
    "suite" >::: [
      "test_pairs"  >:: test_pairs;
      "test_all_eq" >:: test_all_eq;
    ] |> run_test_tt_main

let _ = main ()
