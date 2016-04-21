open Core.Std
open Graph
open Asm

module type ControlFlowGraph = Graph.Sig.I

module type NodeData = sig
  type t
end

module StartExit(N: NodeData) = struct
  type t =
    | Node of N.t
    | Start
    | Exit
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
module IrDataStartExit = StartExit(IrData)
module IrCfg = struct
  open IrData
  include Make(IrDataStartExit)

  let create_cfg irs =
    (* Enumerate ir *)
    let enumerated = List.mapi irs ~f:(fun num ir -> {num; ir}) in
    let vertices = List.map ~f:(fun ir -> V.create (Node ir)) enumerated in

    (* Map each label in the graph to it's enumerated counterpart. We'll use
     * this mapping to form the branches in our CFG. *)
    let association = List.filter_map enumerated ~f:(fun {num; ir} ->
      match ir with
      | Ir.Label l -> Some (l, IrDataStartExit.Node {num; ir})
      | _ -> None
    ) in
    let label_map = String.Map.of_alist_exn association in

    (* Create graph *)
    let one_for_start = 1 in
    let one_for_exit  = 1 in
    let size = List.length enumerated + one_for_start + one_for_exit in
    let g = create ~size () in

    (* add vertexes *)
    List.iter vertices ~f:(add_vertex g);

    (* add labels *)
    let rec add_edges irs =
      match irs with
      | ({ir=a; _} as x)::y::irs ->
        begin
          match a with
          | Ir.Exp _ | Ir.Label _ | Ir.Move _ ->
              add_edge g (V.create (Node x)) (V.create (Node y))
          | Ir.Jump (Ir.Name l)
          | Ir.CJumpOne (_, l) -> begin
              let z = String.Map.find_exn label_map l in
              add_edge g (V.create (Node x)) (V.create z)
          end
          | Ir.Return -> ()
          | Ir.Jump _ -> failwith "IR shouldn't jump to something other than a name"
          | Ir.Seq _ -> failwith "lowered IR shouldn't have nested Seqs"
          | Ir.CJump _ -> failwith "lowered IR shouldn't have CJump";
        end;
        add_edges (y::irs)
      | [] | [_] -> ()
    in
    add_edges enumerated;

    (* Add an edge from the start node to every node with 0 in-degree. *)
    let start = V.create Start in
    add_vertex g start;
    let connect_to_start v =
      match V.label v with
      | Node _ ->
          if in_degree g v = 0
            then add_edge g start v
            else ()
      | Start | Exit -> ()
    in
    iter_vertex connect_to_start g;

    (* Add an edge from every node with 0 out-degree to the exit node. *)
    let exit = V.create Start in
    add_vertex g exit;
    let connect_to_exit v =
      match V.label v with
      | Node _ ->
          if out_degree g v = 0
            then add_edge g v exit
            else ()
      | Start | Exit -> ()
    in
    iter_vertex connect_to_exit g;

    g
end

(* Asm CFG *)
module AsmData = struct
  type t = {
    num: int;
    asm: Asm.abstract_asm;
  }
end
module AsmDataStartExit = StartExit(AsmData)
module AsmCfg = struct
  include Make(AsmDataStartExit)

  (* TODO: change this to include branches *)
  let create_cfg (asms: abstract_asm list) =
    let cfg = create ~size:(List.length asms) () in
    let nodes =
      let f i asm = V.(create (Node { num = i; asm = asm; })) in
      List.mapi ~f asms in

    let rec add_structure nodelist =
      match nodelist with
      | [] -> ()
      | hd :: [] -> add_vertex cfg hd
      | hd1 :: hd2 :: tl -> add_edge cfg hd1 hd2; add_structure (hd2 :: tl) in

    add_structure nodes;
    cfg
end
