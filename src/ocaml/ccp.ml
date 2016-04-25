open Core.Std
open Cfg
open Dataflow
open Ir
open Tiling
open Fresh

module CCPLattice = struct
  type reachable = Top | Bottom
  type data = Top | Bottom | Const of Int64.t
  let ( ** ) = failwith "l"
  let ( === ) = failwith "l"
  let to_string = function
    | Top -> "top"
    | Bottom -> "bottom"
    | Const i -> "const " ^ (Int64.to_string i)
end
