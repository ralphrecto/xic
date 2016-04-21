open Cfg
open Dataflow
open Graph
open Fixpoint

module MakeAnalysis
  (Config: sig val direction : [ `Forward | `Backward ] end)
  (C: CFGWithLatticeT)
  : Fixpoint.Analysis = struct

  type data = C.data
  type edge = C.edge
  type vertex = C.node
  type g = C.graph

  let direction =
    match Config.direction with
    | `Forward -> Forward
    | `Backward -> Backward
  let join = C.Lattice.( ** )
  let equal = C.Lattice.( === )
  let analyze = C.transfer
end
