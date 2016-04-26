open Core.Std
open Graph
open Asm

module type ControlFlowGraph = sig
  include Graph.Sig.I
  val string_of_vertex : V.t -> string
  val string_of_edge   : E.t -> string
end

module type NodeData = sig
  type t [@@deriving sexp, compare]
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
  [@@deriving sexp, compare]

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

  let preds g v =
    fold_pred (fun v' g -> VertexSet.add g v') g v VertexSet.empty

  let preds_e g v =
    fold_pred_e (fun e g -> EdgeSet.add g e) g v EdgeSet.empty

  let succs_e g v =
    fold_succ_e (fun e g -> EdgeSet.add g e) g v EdgeSet.empty

  let equal x y =
    VertexSet.equal (vertex_set x) (vertex_set y) &&
    EdgeSet.equal   (edge_set x)   (edge_set y)

  let swap g ~oldv ~newv =
    let change v =
      if V.compare v oldv = 0
        then newv
        else v
    in

    let edges = EdgeSet.union (preds_e g oldv) (succs_e g oldv) in
    remove_vertex g oldv;
    add_vertex g newv;
    let new_edges = EdgeSet.map edges ~f:(fun e ->
      E.create (change (E.src e)) (E.label e) (change (E.dst e))
    ) in
    EdgeSet.iter new_edges ~f:(add_edge_e g)

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
  } [@@deriving sexp, compare]

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

module IrMap = Map.Make(IrData)

module IrStartExitMap = struct
  include Map.Make(IrDataStartExit)
  let to_string f m =
    to_alist m
    |> List.map ~f:(fun (k, v) ->
         sprintf "  '%s' -> %s" (IrDataStartExit.to_string k) (f v)
       )
    |> String.concat ~sep:",\n"
    |> fun s -> "{\n" ^ s ^ "\n}"
end

(* Asm CFG *)
module AsmData = struct
  type t = {
    num: int;
    asm: Asm.abstract_asm;
  } [@@deriving sexp, compare]

  let to_string asmdata =
    ((string_of_int asmdata.num) ^ ": " ^ (string_of_abstract_asm asmdata.asm))
  let to_int asmdata = asmdata.num
end
module AsmDataStartExit = StartExit(AsmData)
module AsmCfg = struct
  include Make(AsmDataStartExit)

  let create_cfg (asms: abstract_asm list) =
    (* a list of jumping instructions *)
    let jump_instr = [
      "jmp";
      "je";
      "jne";
      "jnz";
      "jz";
      "jg";
      "jge";
      "jl";
      "jle";
      "js";
      "jns";
      "jc";
      "jnc";
    ] in

    (* If asm is a jump, return Some labelstr; otherwise None.
     * Note that we do not include calls here, since in general we may
     * be calling functions that are linked in and are not within the same
     * compilation unit, i.e. we do not know the labels for them, nor do we
     * represent those labels with nodes in our cfg. This is fine as we build
     * CFGs on a per-function basis. That is, we treat calls as effectively
     * doing some magical work that puts stuff into the return registers/
     * return space in stack and then gives control to the next node.
     * Jumps are fine as we should only ever generate jumps within the
     * same compilation unit (indeed, the same function) *)
    let is_jump (asm : abstract_asm) : string option =
      match asm with
      | Op (instr, Label labelstr :: []) when List.mem jump_instr instr ->
          Some labelstr
      | _ -> None in

    let cfg = create () in

    (* a list of asm cfg nodes, a label str -> asm cfg node map *)
    let nodes, label_map =
      let f (nodeacc, mapacc) (num, asm) =
        let node : AsmData.t = { num; asm; } in
        let mapacc' =
          match asm with
          | Lab labelstr -> String.Map.add ~key:labelstr ~data:node mapacc
          | _ -> mapacc in
        (node :: nodeacc, mapacc') in
      let numbered_asms =
        let f i asm = (i, asm) in
        List.mapi ~f asms in
      List.fold_left ~f ~init:([], String.Map.empty) numbered_asms in

    let rec add_structure (nodelist : AsmData.t list) =
      match nodelist with
      | [] -> ()
      | { asm; _; } as hd :: [] ->
        begin
          let target =
            match is_jump asm with
            | Some labelstr ->
                V.create (Node (String.Map.find_exn label_map labelstr))
            (* In this case, we presume that asm is retq *)
            | None -> V.create Exit in
           add_edge cfg (V.create (Node hd)) target
        end
      (* we presume that hd1 cannot be retq (otherwise hd2 is unreachable) *)
      | hd1 :: hd2 :: tl ->
        begin
          let { asm = asm1; _; } : AsmData.t = hd1 in
          let target =
            match is_jump asm1 with
            | Some labelstr -> String.Map.find_exn label_map labelstr
            (* asm1 not a jump = give control to asm2 (i.e. from hd2) *)
            | None -> hd2 in
          add_edge cfg (V.create (Node hd1)) (V.create (Node target));
          add_structure (hd2 :: tl)
        end in

    let add_init (nodelist : AsmData.t list) =
      match nodelist with
      | [] -> add_edge cfg (V.create Start) (V.create Exit)
      | hd1 :: _ -> add_edge cfg (V.create Start) (V.create (Node hd1)) in

    add_init nodes;
    add_structure nodes;
    cfg

end
