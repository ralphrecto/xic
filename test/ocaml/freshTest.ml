open Core.Std
open OUnit
open TestUtil

module FooName = struct
  let name = "foo"
end

module FreshFoo = Fresh.Make(FooName)

let test_fresh () =
  "foo0" === FreshFoo.fresh ();
  "foo1" === FreshFoo.fresh ();
  "foo2" === FreshFoo.fresh ();
  "foo3" === FreshFoo.fresh ();
  "foo4" === FreshFoo.fresh ();
  "foo5" === FreshFoo.fresh ();
  ()

let test_gen () =
  "foo0" === FreshFoo.gen 0;
  "foo42" === FreshFoo.gen 42;
  "foo5430" === FreshFoo.gen 5430;
  ()

let test_reset () =
  FreshFoo.reset ();
  "foo0" === FreshFoo.fresh ();
  "foo1" === FreshFoo.fresh ();
  "foo2" === FreshFoo.fresh ();
  "foo3" === FreshFoo.fresh ();
  "foo4" === FreshFoo.fresh ();
  "foo5" === FreshFoo.fresh ();
  FreshFoo.reset ();
  "foo0" === FreshFoo.fresh ();
  "foo1" === FreshFoo.fresh ();
  "foo2" === FreshFoo.fresh ();
  "foo3" === FreshFoo.fresh ();
  "foo4" === FreshFoo.fresh ();
  "foo5" === FreshFoo.fresh ();
  ()

let test_get () =
  Some 1 === FreshFoo.get "foo1";
  Some 2 === FreshFoo.get "foo2";
  None   === FreshFoo.get "foo";
  None   === FreshFoo.get "bar1";
  ()

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* ! DON'T FORGET TO ADD YOUR TESTS HERE                                     ! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let main () =
    "suite" >::: [
      "test_fresh" >:: test_fresh;
      "test_gen"   >:: test_gen;
      "test_reset" >:: test_reset;
      "test_get"   >:: test_get;
    ] |> run_test_tt_main

let _ = main ()
