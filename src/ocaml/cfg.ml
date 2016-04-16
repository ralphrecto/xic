open Graph
open Asm

module type ControlFlowGraph = sig
  include Graph.Sig.I

  (* program representation using this CFG, e.g. IR AST, asm lists, etc. *)
  type representation
  (* node type should be IR AST nodes, abstract asm stmts, etc. *)
  type node 
  (* information that will be kept on each CFG edge *)
  type edge

  (* create a CFG from a given program representation *)
  val create_cfg : representation -> t
end

module type AbstractAsmCFGT = sig
  include ControlFlowGraph with type representation = abstract_asm list
end

module AbstractAsmCFG : AbstractAsmCFGT = struct
  type representation = abstract_asm list
  type node = abstract_asm
  type edge = TrueBranch | FalseBranch | SingleBranch

  module NodeLabel : Graph.Sig.ANY_TYPE = struct
    type t = node
  end

  module EdgeLabel : Graph.Sig.ORDERED_TYPE_DFT = struct
    type t = edge
    let compare _ _ -> 0
    let default = SingleBranch
  end
    
  include AbstractLabeled (NodeLabel) (EdgeLabel)

  (* TODO: change this to include branches *)
  val create_cfg (asms: abstract_asm list) =
    let cfg = create ~size:(List.length asms) () in

    let node_of (a: abstract_asm) = V.create a in
    let rec add_structure asms' = 
      match asms' with
      | [] -> ()
      | hd :: [] -> add_vertex cfg (node_of hd)
      | hd1 :: hd2 :: tl -> add_edge 

end

