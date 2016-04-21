open Cfg
(* we're using ocaml's hashtable instead of jane street's *)
module Hash = Hashtbl
open Core.Std

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
  module Lattice : LowerSemilattice
  module CFG : ControlFlowGraph
type graph = CFG.t
  type node = CFG.V.t
  type edge = CFG.E.t
  type data = Lattice.data

  val transfer : edge -> data -> data
end

module type Analysis = sig
  module CFGL : CFGWithLatticeT
  open CFGL

  val iterative : (node -> data) -> graph -> edge -> data

  val worklist : (node -> data) -> graph -> edge -> data

end

module GenericAnalysis
  (Config: sig val direction : [ `Forward | `Backward ] end)
  (CFGLArg: CFGWithLatticeT)
  : Analysis = struct

  module CFGL = CFGLArg
  open CFGL
  open Lattice
  open CFG

  let meet_fold, transfer_fold, transfer_iter, update_node  =
    match Config.direction with
    | `Forward -> fold_pred_e, fold_succ_e, iter_succ_e, E.dst
    | `Backward -> fold_succ_e, fold_pred_e, iter_pred_e, E.src

  let calc_meet (table: (edge, data) Hash.t) (e: edge) (acc: data option) =
    let edge_datum = Hash.find table e in
    match acc with
    | None -> Some edge_datum
    | Some d' -> Some (d' ** edge_datum)

  let iterative (init: node -> data) (cfg: graph) : edge -> data =
    (* initializations *)
    let table : (edge, data) Hash.t = Hash.create (nb_edges cfg) in
    iter_vertex (fun node -> transfer_iter (fun e -> Hash.add table e (init node)) cfg node) cfg;

    let vertex_foldf (n: node) (changed : bool) =
      let meet = meet_fold (calc_meet table) cfg n None in
      match meet with
      | None -> changed
      | Some meet' ->
        let update (e:edge) (updated: bool) =
          let edge_datum = Hash.find table e in
          let new_datum = transfer e meet' in
          if edge_datum === new_datum then
            updated
          else
            let _ = Hash.replace table e new_datum in
            true
        in
        transfer_fold update cfg n false
    in

    let rec iterate () : edge -> data =
      (* has the data for the CFG nodes changed? *)
      if fold_vertex vertex_foldf cfg false then
        iterate ()
      else
        fun e' -> Hash.find table e'
    in

    iterate ()

  let worklist (init: node -> data) (cfg: graph) : edge -> data =
    (* initializations *)
    let data_table : (edge, data) Hash.t = Hash.create (nb_edges cfg) in
    let node_set : node Queue.t = Queue.create () in
    iter_vertex
      (fun node ->
        transfer_iter (fun e -> Hash.add data_table e (init node)) cfg node;
        Queue.enqueue node_set node)
      cfg;
    let rec work () =
      match Queue.dequeue node_set with
      | Some n ->
        begin
          let meet = meet_fold (calc_meet data_table) cfg n None in
          match meet with
          | None -> work ()
          | Some meet' ->
            let update (e: edge) =
              let edge_datum = Hash.find data_table e in
              let new_datum = transfer e meet' in
              if edge_datum === new_datum then
                ()
              else
                Queue.enqueue node_set (update_node e)
            in
            let _ = transfer_iter update cfg n in
            work ()
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
