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

module CFGWithLattice(CFG: ControlFlowGraph, Data: LowerSemilattice) = struct
  include Data
  include CFG

  type graph = CFG.t
  type node = CFG.V.t

  (* transfer function f_n for a given node n *)
  val transfer : node -> Data.data -> Data.data
end

module ForwardAnalysis(CFGL: CFGWithLattice) = struct
  open CFGL

  let iterative (cfg: graph) : (node * data) list = 
    (* initializations *)
    let table : (node, data) Hashtbl.t = Hashtbl.create (nb_edges cfg) in
    iter_vertex (fun node -> Hashtbl.add table node top) cfg;

    let vertex_foldf (n: node) (changed : bool) =  
      let datum = Hashtbl.find table n in
      let datum' = 
        let pred_foldf (n_pred: node) (d: data option) =
          (* return None if no predecessors, otherwise take meet *)
          let pred_datum = Hashtbl.find table n_pred in
          match d with
          | None -> Some pred_datum
          | Some d' -> Some (pred_datum ** d') in
        (* if None, there are no predecessors; keep datum same *)
        match fold_pred pred_foldf cfg n None with
        | None -> datum
        | Some pred_meet -> transfer n pred_meet in
      let v_changed = datum === datum' in
      if v_changed then
        Hashtbl.replace tbl n datum'; true
      else changed

    let rec iterate () : (node * data) list =
      (* has the data for the CFG nodes changed? *)
      if fold_vertex vertex_foldf cfg false then
        iterate ()
      else
        let f node data acclist = (node, data) :: acclist in
        Hashtbl.fold f table []
end
