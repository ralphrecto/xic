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

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* ! DON'T FORGET TO ADD YOUR TESTS HERE                                     ! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let main () =
    "suite" >::: [
      "test_pairs" >:: test_pairs;
    ] |> run_test_tt_main

let _ = main ()
