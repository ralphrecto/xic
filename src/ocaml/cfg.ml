open Core.Std
open Graph
open Asm

module type ControlFlowGraph = Graph.Sig.I

module type NodeData = sig
  type t
end

module EdgeData = struct
  type t =
    | Normal
    | True
    | False
  [@@deriving sexp, compare]
  let default = Normal
end

module Make(N: NodeData) = struct
  include Imperative.Graph.AbstractLabeled(N)(EdgeData)
end

(* IR CFG *)
module IrData = struct
  type t = {
    num: int;
    ir:  Ir.stmt;
  }
end
module IrCfg = struct
  include Make(IrData)
  let create_cfg _ss =
    failwith "TODO"
end

(* Asm CFG *)
module AsmData = struct
  type t = {
    num: int;
    asm: Asm.abstract_asm;
  }
end
module AsmCfg = struct
  include Make(AsmData)

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
