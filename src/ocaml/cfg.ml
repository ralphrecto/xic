open Core.Std
open Graph
open Asm

module type ControlFlowGraph = sig
  include Graph.Sig.I

  (* program representation using this CFG, e.g. IR AST, asm lists, etc. *)
  type representation
  (* node type should be IR AST nodes, abstract asm stmts, etc. *)
  type nodedata
  (* information that will be kept on each CFG edge
   * we restrict CFG nodes to have at most 2 outgoing edges *)
  type edgedata = BranchOne | BranchTwo | NoBranch

  (* create a CFG from a given program representation *)
  val create_cfg : representation -> t
end

module type AbstractAsmCFGT = sig
  include ControlFlowGraph with type representation = abstract_asm list
end

module AbstractAsmCFG : AbstractAsmCFGT = struct
  type representation = abstract_asm list
  type nodedata = {
    num: int;
    asm: abstract_asm;
  }
  type edgedata = BranchOne | BranchTwo | NoBranch

  module NodeLabel : Graph.Sig.ANY_TYPE with type t = nodedata = struct
    type t = nodedata
  end

  module EdgeLabel : Graph.Sig.ORDERED_TYPE_DFT with type t = edgedata = struct
    type t = edgedata
    let compare _ _ = 0
    let default = NoBranch
  end

  include Imperative.Graph.AbstractLabeled (NodeLabel) (EdgeLabel)

  (* TODO: change this to include branches *)
  let create_cfg (asms: abstract_asm list) =
    let cfg = create ~size:(List.length asms) () in
    let nodes =
      let f i asm = V.create { num = i; asm = asm; } in
      List.mapi ~f asms in

    let rec add_structure nodelist = 
      match nodelist with
      | [] -> ()
      | hd :: [] -> add_vertex cfg hd
      | hd1 :: hd2 :: tl -> add_edge cfg hd1 hd2; add_structure (hd2 :: tl) in

    add_structure nodes;
    cfg

end

