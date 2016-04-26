open Core.Std
open Asm

module AbstractRegSet : Set.S with type Elt.t = abstract_reg

(* performs register allocation with move coalescing *)
val reg_alloc : abstract_asm list -> asm list

module LiveVariableLattice : Dataflow.LowerSemilattice with
  type data = AbstractRegSet.t

module AsmWithLiveVar : Dataflow.CFGWithLatticeT
  with module CFG = Cfg.AsmCfg
  and module Lattice = LiveVariableLattice
  and type extra_info = unit

module LiveVariableAnalysis : (module type of Dataflow.GenericAnalysis(AsmWithLiveVar))
