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
  include LowerSemilattice
  include ControlFlowGraph

  type graph = t
  type node = V.t

  val transfer : node -> data -> data
end

module type Analysis = sig
  module CFGL : CFGWithLatticeT

  val iterative : CFGL.graph -> (CFGL.node * CFGL.data) list
end

module GenericAnalysis
  (Config: sig val direction : [`Forward | `Backward] end)
  (CFGLArg: CFGWithLatticeT)
  : Analysis = struct

  module CFGL = CFGLArg
  open CFGL

  let neighbor_fold =
    match Config.direction with
    | `Forward -> fold_pred
    | `Backward -> fold_succ 

  let iterative (cfg: graph) : (node * data) list = 
    (* initializations *)
    let table : (node, data) Hashtbl.t = Hashtbl.create (nb_edges cfg) in
    iter_vertex (fun node -> Hashtbl.add table node top) cfg;

    let vertex_foldf (n: node) (changed : bool) =  
      let datum = Hashtbl.find table n in
      let datum' = 
        let neighbor_foldf (n_neighbor: node) (d: data option) =
          (* return None if no predecessors, otherwise take meet *)
          let neighbor_datum = Hashtbl.find table n_neighbor in
          match d with
          | None -> Some neighbor_datum
          | Some d' -> Some (neighbor_datum ** d') in
        (* if None, there are no predecessors; keep datum same *)
        match neighbor_fold neighbor_foldf cfg n None with
        | None -> datum
        | Some neighbor_meet -> transfer n neighbor_meet in
      let v_changed = datum === datum' in
      if v_changed then
        begin Hashtbl.replace table n datum'; true end
      else changed in

    let rec iterate () : (node * data) list =
      (* has the data for the CFG nodes changed? *)
      if fold_vertex vertex_foldf cfg false then
        iterate ()
      else
        let f node data acclist = (node, data) :: acclist in
        Hashtbl.fold f table [] in

    iterate ()
end

module ForwardAnalysis (CFGL: CFGWithLatticeT) =
  GenericAnalysis (struct let direction = `Forward end) (CFGL)

module BackwardAnalysis (CFGL: CFGWithLatticeT) =
  GenericAnalysis (struct let direction = `Backward end) (CFGL)
