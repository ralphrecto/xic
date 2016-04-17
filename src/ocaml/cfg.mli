open Graph
open Asm

module type ControlFlowGraph = sig
  include Graph.Sig.I

  (* program representation using this CFG, e.g. IR AST, asm lists, etc. *)
  type repr

  (* node type should be IR AST nodes, abstract asm stmts, etc. *)
  type nodedata
  (* information that will be kept on each CFG edge
   * we restrict CFG nodes to have at most 2 outgoing edges *)
  type edgedata = BranchOne | BranchTwo | NoBranch

  (* create a CFG from a given program repr *)
  val create_cfg : repr -> t
end

module AbstractAsmCfg : ControlFlowGraph with type repr = abstract_asm list 
