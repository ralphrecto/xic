open Graph
open Cfg

module type LowerSemilattice = sig
  (* data associated with each control flow node *)
  type data

  (* maximal value in semilattice *)
  val top : data 
  (* meet operation in the semilattice *)
  val ( ** ) : data -> data -> data
  (* equality over data values *)
  val ( === ) : data -> data -> bool
end

module type CFGWithLatticeT = sig
  include LowerSemilattice
  include ControlFlowGraph

  type graph = t
  type node = V.t

  val transfer : node -> data -> data
end

module type Analysis = sig
  module CFGL : CFGWithLatticeT

  val iterative : CFGL.graph -> (CFGL.node * CFGL.data) list
end

module ForwardAnalysis (CFGL : CFGWithLatticeT) : Analysis

module BackwardAnalysis (CFGL : CFGWithLatticeT) : Analysis
