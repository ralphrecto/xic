open Core.Std
open Graph
open Asm

module type ControlFlowGraph = Graph.Sig.I

module type NodeData = sig
  type t
  val to_string : t -> string
end

module StartExit(N: NodeData) = struct
  type t =
    | Node of N.t
    | Start
    | Exit

  let to_string = function
    | Node n -> N.to_string n
    | Start  -> "start"
    | Exit   -> "exit"
end

module EdgeData = struct
  type t =
    | Normal
    | True
    | False
  [@@deriving compare]

  let default = Normal
end

module Make(N: NodeData) = struct
  include Imperative.Graph.AbstractLabeled(N)(EdgeData)

  module X = struct
    include Imperative.Graph.AbstractLabeled(N)(EdgeData)
    let graph_attributes _ = []
    let default_vertex_attributes _ = []
    let vertex_name v = Int.to_string (Mark.get v)
    let vertex_attributes v = [`Label (N.to_string (V.label v))]
    let get_subgraph _ = None
    let default_edge_attributes _ = []
    let edge_attributes _ = []
  end
  include Graphviz.Dot(X)
  let to_dot = output_graph
end

(* IR CFG *)
module IrData = struct
  type t = {
    num: int;
    ir:  Ir.stmt;
  }
  [@@deriving sexp, compare]

  let to_string {ir; _} = Ir.string_of_stmt ir
end
module IrDataStartExit = StartExit(IrData)
module IrCfg = struct
  open IrData
  include Make(IrDataStartExit)

  let create_cfg irs =
    (* Enumerate ir. Also create a map from each vertex's label to its vertex. *)
    let enumerated = List.mapi irs ~f:(fun num ir -> {num; ir}) in
    let module IrMap = Map.Make(IrData) in
    let association = List.map ~f:(fun ir -> (ir, V.create (Node ir))) enumerated in
    let vertex_map = IrMap.of_alist_exn association in

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
    List.iter (IrMap.data vertex_map) ~f:(add_vertex g);

    (* add labels *)
    let rec add_edges irs =
      match irs with
      | ({ir=a; _} as x)::y::irs ->
        begin
          match a with
          | Ir.Exp _ | Ir.Label _ | Ir.Move _ ->
              add_edge g (IrMap.find_exn vertex_map x) (IrMap.find_exn vertex_map y)
          | Ir.Jump (Ir.Name l)
          | Ir.CJumpOne (_, l) -> begin
              let z = String.Map.find_exn label_map l in
              add_edge g (IrMap.find_exn vertex_map x) (V.create z)
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
      | Node _ -> begin
          printf "%d\n" (in_degree g v);
          if (in_degree g v / 2) = 0
            then add_edge g start v
            else ()
      end
      | Start | Exit -> ()
    in
    iter_vertex connect_to_start g;

    (* Add an edge from every node with 0 out-degree to the exit node. *)
    let exit = V.create Exit in
    add_vertex g exit;
    let connect_to_exit v =
      match V.label v with
      | Node _ -> begin
          printf "%d\n" (in_degree g v);
          if (out_degree g v / 2) = 0
            then add_edge g v exit
            else ()
      end
      | Start | Exit -> ()
    in
    iter_vertex connect_to_exit g;

    (* Mark every node with a unique integer. This is used to pretty print
     * graphs as DOT files.  *)
    ignore (fold_vertex (fun v i -> Mark.set v i; i + 1) g 0);

    g
end

(* Asm CFG *)
module AsmData = struct
  type t = {
    num: int;
    asm: Asm.abstract_asm;
  }

  let to_string _ = "TODO"
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
