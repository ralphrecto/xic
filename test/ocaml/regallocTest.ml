open Core.Std
open OUnit2
open TestUtil
open Util
open Regalloc
open Asm

module AsmCfgEq = struct
  let (===) (a: Cfg.AsmCfg.t) (b: Cfg.AsmCfg.t) : unit =
    assert_equal ~cmp:Cfg.AsmCfg.equal ~printer:Cfg.AsmCfg.to_dot a b

  let (=/=) (a: Cfg.AsmCfg.t) (b: Cfg.AsmCfg.t) : unit =
    if Cfg.AsmCfg.equal a b then
        let a = Cfg.AsmCfg.to_dot a in
        let b = Cfg.AsmCfg.to_dot b in
        assert_failure (sprintf "These are equal, but shouldn't be:\n%s\n%s" a b)
end

let live_variables_test _ =

  ()



(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* ! DON'T FORGET TO ADD YOUR TESTS HERE                                     ! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let main () =
    "suite" >::: [
      "live_variables_test" >:: live_variables_test;
    ] |> run_test_tt_main

let _ = main ()

