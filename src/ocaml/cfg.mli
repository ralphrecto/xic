open Graph
open Asm

module type ControlFlowGraph = sig
  include Graph.Sig.I
end

type nodedata = {
  num: int;
  asm: abstract_asm;
}

type edgedata = BranchOne | BranchTwo | NoBranch

module type AbstractAsmCfgT = sig
  include ControlFlowGraph with
    type V.label = nodedata
    and type E.label = edgedata

  val create_cfg : abstract_asm list -> t
end

module AbstractAsmCfg : AbstractAsmCfgT
