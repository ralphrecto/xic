open Cfg

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

end

module GenericAnalysis
  (Config: sig val direction : [ `Forward | `Backward ] end)
  (CFGLArg: CFGWithLatticeT)
  : Analysis = struct

  module CFGL = CFGLArg
  open CFGL
  open Lattice
  open CFG

  let meet_fold =
    match Config.direction with
    | `Forward -> fold_pred_e
    | `Backward -> fold_succ_e

  let transfer_fold =
    match Config.direction with
    | `Forward -> fold_succ_e
    | `Backward -> fold_pred_e

  let edge_iter =
    match Config.direction with
    | `Forward -> iter_succ_e
    | `Backward -> iter_pred_e

  let iterative (init: node -> data) (cfg: graph) : edge -> data =
    (* initializations *)
    (* in a forward analysis we keep track of the outs and
     * in a backward analysis we keep track of the ins *)
    let table : (edge, data) Hashtbl.t = Hashtbl.create (nb_edges cfg) in
    iter_vertex (fun node -> edge_iter (fun e -> Hashtbl.add table e (init node)) cfg node) cfg;

    let vertex_foldf (n: node) (changed : bool) =
      let calc_meet (e: edge) (acc: data option) =
        let edge_datum = Hashtbl.find table e in
        match acc with
        | None -> Some edge_datum
        | Some d' -> Some (d' ** edge_datum)
      in

      let meet = meet_fold calc_meet cfg n None in
      match meet with
      | None -> changed
      | Some meet' ->
        let update (e:edge) (updated: bool) =
          let edge_datum = Hashtbl.find table e in
          let new_datum = transfer e meet' in
          if edge_datum === new_datum then
            updated
          else
            let _ = Hashtbl.replace table e new_datum in
            true
        in
        transfer_fold update cfg n false
    in

    let rec iterate () : edge -> data =
      (* has the data for the CFG nodes changed? *)
      if fold_vertex vertex_foldf cfg false then
        iterate ()
      else
        fun e' -> Hashtbl.find table e'
    in

    iterate ()
end

module ForwardAnalysis (CFGL: CFGWithLatticeT) =
  GenericAnalysis (struct let direction = `Forward end) (CFGL)

module BackwardAnalysis (CFGL: CFGWithLatticeT) =
  GenericAnalysis (struct let direction = `Backward end) (CFGL)
