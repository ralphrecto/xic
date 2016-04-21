open Core.Std
open Cfg
open Dataflow
open Asm
open Tiling
open Fresh

module LiveVariableLattice : LowerSemilattice with
  type data = Int.Set.t = struct

  type data = Int.Set.t
  let ( ** ) = Int.Set.union
  let ( === ) = Int.Set.equal
end

module AsmWithLiveVar : CFGWithLatticeT = struct
  module Lattice = LiveVariableLattice
  module CFG = AsmCfg
  open Lattice
  open CFG

  type graph = CFG.t
  type node = CFG.V.t
  type edge = CFG.E.t
  type data = Lattice.data

  (* returns a sets of vars used and defd, respectively *)
  let _usedvars : abstract_asm -> Int.Set.t * Int.Set.t =
    let set_of_arg (arg: abstract_reg operand) : Int.Set.t =
      let fakes = fakes_of_operand arg in
      let f acc fake =
        (* TODO: need to fix not all the temporary registers will be of form __asmreg- *)
        match FreshReg.get fake with
        | None -> acc
        | Some x -> Int.Set.add acc x in
      List.fold_left ~f ~init:Int.Set.empty fakes in
    let binops_use_plus_def =  [
      "addq";
      "subq";
      "andq";
      "orq";
      "xorq";
      "shlq";
      "shrq";
      "sarq";
      "leaq";
      "movq"
    ] in
    let binops_use = [
      "bt";
      "cmpq";
      "test"
    ] in
    let unops_use_plus_def = [
      "incq";
      "decq";
      "negq";
    ] in
    (* TODO: these should go in as a special case since we se CL for the
     * instructions right now.
     * Although for register allocation we probably want to do something smarter
     * and not default to CL but any other 8 bit register *)
    let unops_def = [
      "asete";
      "asetne";
      "asetl";
      "asetg";
      "asetle";
      "asetge";
      "asetz";
      "asetnz";
      "asets";
      "asetns";
      "asetc";
      "asetnc";
      "pop"
    ] in
    let unops_use = [
      "push" ;
      "pushq"
    ] in
    let unops_special = [
      "imulq";
      "idivq"
    ] in
    function
      | Op (name, arg :: []) ->
        let arg_set = set_of_arg arg in
        if List.mem unops_use_plus_def name then
          (arg_set, arg_set)
        else if List.mem unops_def name then
          (Int.Set.empty, arg_set)
        else if List.mem unops_use name then
          (arg_set, Int.Set.empty)
          (* TODO: HANDLE SPECIAL CASES!!! *)
        else if List.mem unops_special name then
          (Int.Set.empty, Int.Set.empty)
        else (Int.Set.empty, Int.Set.empty)
      | Op (name, arg1 :: arg2 :: []) ->
        let arg1_set = set_of_arg arg1 in
        let arg2_set = set_of_arg arg2 in
        let arg_union = Int.Set.union arg1_set arg2_set in
        if List.mem binops_use_plus_def name then
          (arg_union, arg2_set)
        else if List.mem binops_use name then
          (arg_union, Int.Set.empty)
        else (Int.Set.empty, Int.Set.empty)
      | _ -> (Int.Set.empty, Int.Set.empty)

  let transfer (_e: edge) (_d: data) = failwith "TODO"
    (*
    let n_data = V.label n in
    let use_n, def_n = usedvars n_data.asm in
    Int.Set.union use_n (Int.Set.diff d def_n)
    *)
end

module LiveVariableAnalysis = BackwardAnalysis (AsmWithLiveVar)
