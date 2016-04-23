open Cfg
(* we're using ocaml's hashtable instead of jane street's *)
module Hash = Hashtbl
open Core.Std

module type LowerSemilattice = sig
  (* data associated with each control flow node *)
  type data

  (* meet operation in the semilattice *)
  val ( ** ) : data -> data -> data

  (* equality over data values *)
  val ( === ) : data -> data -> bool

  val to_string : data -> string
end

module type CFGWithLatticeT = sig
  module Lattice : LowerSemilattice
  module CFG : ControlFlowGraph
  type graph = CFG.t
  type node = CFG.V.t
  type edge = CFG.E.t
  type data = Lattice.data

  type extra_info

  val init: graph -> node -> data
  val transfer : extra_info -> edge -> data -> data
end

module type Analysis = sig
  module CFGL : CFGWithLatticeT
  open CFGL

  val iterative : extra_info -> graph -> edge -> data
  val worklist : extra_info -> graph -> edge -> data
end

module GenericAnalysis
  (Config: sig val direction : [ `Forward | `Backward ] end)
  (CFGLArg: CFGWithLatticeT) = struct

  module CFGL = CFGLArg
  open CFGL
  open Lattice
  open CFG

  (* meet_fold     is used to fold over edges when computing the meet
   * transfer_fold is used to fold over edges when computing the transfer
   * transfer_iter is used when initializing the data table and iter over edges
   *               when computing the tranfer
   * update_node   is used to determine if the src or the dest of the edge should be added to
   *               the worklist queue after the data has changed
   *)
  let meet_fold, transfer_fold, transfer_iter, update_node  =
    match Config.direction with
    | `Forward -> fold_pred_e, fold_succ_e, iter_succ_e, E.dst
    | `Backward -> fold_succ_e, fold_pred_e, iter_pred_e, E.src

  (* helper function when iterating over edges to calculate the meet *)
  let calc_meet (table: (edge, data) Hash.t) (e: edge) (acc: data option) =
    let edge_datum = Hash.find table e in
    match acc with
    | None -> Some edge_datum
    | Some d' -> Some (d' ** edge_datum)

  let iterative (ei: extra_info) (cfg: graph) : edge -> data =
    (* initializations *)
    (* data table *)
    let table : (edge, data) Hash.t = Hash.create (nb_edges cfg) in
    let init = CFGL.init cfg in
    iter_vertex (fun node -> transfer_iter (fun e -> Hash.add table e (init node)) cfg node) cfg;

    (* helper function when folding over all nodes of graph to calculate fixpoint *)
    let vertex_foldf (n: node) (changed : bool) =
      (* calculate meet *)
      let meet = meet_fold (calc_meet table) cfg n None in
      (* if no predecessors/successors then data does not change
       * otherwise calculate new datum for edges accordingly
       * if the new datum is different, then update data table then mark that
       * the data has changed *)
      match meet with
      | None -> changed
      | Some meet' -> begin
        let update (e:edge) (updated: bool) =
          let edge_datum = Hash.find table e in
          let new_datum = transfer ei e meet' in
          if edge_datum === new_datum then
            updated
          else
            let _ = Hash.replace table e new_datum in
            true
        in
        transfer_fold update cfg n changed
      end
    in

    (* iterate through nodes until data does not change *)
    let rec iterate () : edge -> data =
      (* has the data for the CFG nodes changed? *)
      if fold_vertex vertex_foldf cfg false then
        iterate ()
      else
        fun e' -> Hash.find table e'
    in

    iterate ()

  let worklist (ei: extra_info) (cfg: graph) : edge -> data =
    (* initializations *)
    (* data table *)
    let data_table : (edge, data) Hash.t = Hash.create (nb_edges cfg) in
    (* worklist queue for nodes *)
    let node_set : node Queue.t = Queue.create () in
    let init = CFGL.init cfg in
    iter_vertex
      (fun node ->
        transfer_iter (fun e -> Hash.add data_table e (init node)) cfg node;
        Queue.enqueue node_set node)
      cfg;

    (* calculate data until queue is empty *)
    let rec work () =
      match Queue.dequeue node_set with
      | Some n ->
        begin
          (* calculate meet *)
          let meet = meet_fold (calc_meet data_table) cfg n None in
          (* if no predecessors/successors then data does not change
           * otherwise calculate new datum for edges accordingly
           * if new datum is different, then update data table and add src/dest
           * to queue *)
          match meet with
          | None -> work ()
          | Some meet' -> begin
            let update (e: edge) =
              let edge_datum = Hash.find data_table e in
              let new_datum = transfer ei e meet' in
              if edge_datum === new_datum then
                ()
              else
                let _ = Hash.replace data_table e new_datum in
                if not (Queue.mem node_set (update_node e)) then
                  Queue.enqueue node_set (update_node e)
            in
            transfer_iter update cfg n;
            work ()
          end
        end
      | None ->
        fun e' -> Hash.find data_table e'
    in
    work ()
end

module ForwardAnalysis (CFGL: CFGWithLatticeT) =
  GenericAnalysis (struct let direction = `Forward end) (CFGL)

module BackwardAnalysis (CFGL: CFGWithLatticeT) =
  GenericAnalysis (struct let direction = `Backward end) (CFGL)
