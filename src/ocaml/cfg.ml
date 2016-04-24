open Core.Std
open Graph
open Asm

module type ControlFlowGraph = sig
  include Graph.Sig.I
  val string_of_vertex : V.t -> string
  val string_of_edge   : E.t -> string
end

module type NodeData = sig
  type t
  val to_string : t -> string
  val to_int    : t -> int
end

module Poly(N: NodeData) = struct
  include N
  let compare = Pervasives.compare
  let hash    = Hashtbl.hash
  let equal   = (=)
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

  let to_int a =
    match a with
    | Start -> 0
    | Exit -> 1
    | Node n -> N.to_int n + 2
end

module EdgeData = struct
  type t =
    | Normal
    | True
    | False
  [@@deriving compare]

  let default = Normal

  let to_string t =
    match t with
    | Normal -> ""
    | True   -> "true"
    | False  -> "false"
end

module Make(N: NodeData) = struct
  include Imperative.Digraph.ConcreteBidirectionalLabeled(Poly(N))(EdgeData)

  module VertexSet = Set.Make(struct
    type t = V.t
    let compare = V.compare
    let sexp_of_t _ = failwith "VertexSet sexp_of_t not supported"
    let t_of_sexp _ = failwith "VertexSet sexp_of_t not supported"
  end)

  module EdgeSet = Set.Make(struct
    type t = E.t
    let compare = E.compare
    let sexp_of_t _ = failwith "EdgeSet sexp_of_t not supported"
    let t_of_sexp _ = failwith "EdgeSet sexp_of_t not supported"
  end)

  let vertex_set g =
    fold_vertex (fun v g -> VertexSet.add g v) g VertexSet.empty

  let edge_set g =
    fold_edges_e (fun e g -> EdgeSet.add g e) g EdgeSet.empty

  let succs g v =
    fold_succ (fun v' g -> VertexSet.add g v') g v VertexSet.empty

  let preds_e g v =
    fold_pred_e (fun e g -> EdgeSet.add g e) g v EdgeSet.empty

  let equal x y =
    VertexSet.equal (vertex_set x) (vertex_set y) &&
    EdgeSet.equal   (edge_set x)   (edge_set y)

  let string_of_vertex = N.to_string
  let string_of_edge e =
    let src = string_of_vertex (E.src e) in
    let dst = string_of_vertex (E.dst e) in
    let label = EdgeData.to_string (E.label e) in
    sprintf "%s -[%s]-> %s" src label dst

  module X = struct
    include Imperative.Digraph.ConcreteBidirectionalLabeled(Poly(N))(EdgeData)
    let graph_attributes _ = []
    let default_vertex_attributes _ = []
    let vertex_name v = Int.to_string (N.to_int (V.label v))
    let vertex_attributes v = [`Label (N.to_string (V.label v))]
    let get_subgraph _ = None
    let default_edge_attributes _ = []
    let edge_attributes e = [`Label (EdgeData.to_string (E.label e))]
  end
  include Graphviz.Dot(X)
  let to_dot t =
    let arbitarary_size = 4096 in
    let b = Buffer.create arbitarary_size in
    let formatter = Format.formatter_of_buffer b in
    fprint_graph formatter t;
    Format.pp_print_flush formatter ();
    Buffer.contents b
end

(* IR CFG *)
module IrData = struct
  type t = {
    num: int;
    ir:  Ir.stmt;
  }

  let to_string {num; ir} = sprintf "%d: %s" num (Ir.string_of_stmt ir)
  let to_int {num; _} = num
end

module IrDataStartExit = StartExit(IrData)
module IrCfg = struct
  open IrData
  include Make(IrDataStartExit)

  let create_cfg irs =
    (* Enumerate ir. Also create a map from each vertex's label to its vertex. *)
    let enumerated = List.mapi irs ~f:(fun num ir -> {num; ir}) in
    let vertexes = List.map ~f:(fun ir -> V.create (Node ir)) enumerated in

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
    List.iter vertexes ~f:(add_vertex g);

    (* add labels *)
    let rec add_edges irs =
      match irs with
      | ({ir=a; _} as x)::y::irs ->
        begin
          match a with
          | Ir.Exp _ | Ir.Label _ | Ir.Move _ ->
              add_edge g (V.create (Node x)) (V.create (Node y))
          | Ir.Jump (Ir.Name l) -> begin
              let z = String.Map.find_exn label_map l in
              add_edge g (V.create (Node x)) (V.create z);
          end
          | Ir.CJumpOne (_, l) -> begin
              let z = String.Map.find_exn label_map l in
              add_edge_e g (E.create (V.create (Node x)) False (V.create (Node y)));
              add_edge_e g (E.create (V.create (Node x)) True  (V.create z))
          end
          | Ir.Return -> ()
          | Ir.Jump _ -> failwith "IR shouldn't jump to something other than a name"
          | Ir.Seq _ -> failwith "lowered IR shouldn't have nested Seqs"
          | Ir.CJump _ -> failwith "lowered IR shouldn't have CJump";
        end;
        add_edges (y::irs)
      | [{ir=Ir.Jump (Ir.Name l); _} as x] ->
          let z = String.Map.find_exn label_map l in
          add_edge g (V.create (Node x)) (V.create z)
      | [] | [_] -> ()
    in
    add_edges enumerated;

    (* (1) Add an edge from the start node to the first ir node, or to the exit
     *     node if no such node exists.
     * (2) Add an edge from every return node to the exit node. *)
    let start = V.create Start in
    let exit = V.create Exit in
    add_vertex g start;
    add_vertex g exit;

    (match List.hd vertexes with
     | Some v -> add_edge g start v
     | None   -> add_edge g start exit);

    let connect_to_exit v =
      match V.label v with
      | Node {ir=Return; _} -> add_edge g v exit
      | Node _ | Start | Exit -> ()
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

  let to_string _ = failwith "TODO"
  let to_int _ = failwith "TODO"
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
