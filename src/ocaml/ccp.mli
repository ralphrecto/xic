module Long = Int64
open Core.Std
open Cfg
open Dataflow
open Ir
open Tiling
open Fresh

module CcpLattice : sig
  type reachable = Reach | Unreach
  type defined = Undef | Def of Int64.t | Overdef
  include Dataflow.LowerSemilattice with
  type data = (reachable * defined String.Map.t)
end

module CcpCFG : sig
  module Lattice = CcpLattice
  module CFG = IrCfg
end

val ccp: Ir.stmt list -> Ir.stmt
val ccp_comp_unit : Ir.comp_unit -> Ir.comp_unit

