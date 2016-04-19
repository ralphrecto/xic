open Graph
open Asm

module type ControlFlowGraph = sig
  include Graph.Sig.I
end

module type AbstractAsmCfgT = sig
  type nodedata = {
    num: int;
    asm: abstract_asm;
  }

  type edgedata = BranchOne | BranchTwo | NoBranch

  include ControlFlowGraph with
    type V.label = nodedata
    and type E.label = edgedata

  val create_cfg : abstract_asm list -> t
end

module AbstractAsmCfg : AbstractAsmCfgT
