open Core.Std
open Cfg
open Dataflow
open Ir
open Tiling
open Fresh

(*
module CCPLattice = struct
  type reachable = Reach | Unreach
  type defined = Undef | Def of Int64.t | Overdef
  type data = (reachable * defined String.Map.t)

  let ( ** ) (reach1, defined1) (reach2, defined2) =
    let defined_meet def1 def2 =
      match def1, def2 with
      | Undef, Undef -> Undef
      | Undef, (_ as def)
      | (_ as def), Undef -> def
      | Def i1, Def i2 when Int64.equal i1 i2 -> def1
      | _ -> Overdef
    in
    let f ~key:temp ~data:def1 acc =
      let def2 = String.Map.find_exn defined2 temp in
      let new_def = defined_meet def1 def2 in
      String.Map.add acc ~key:temp ~data:new_def
    in
    let new_defined = String.Map.fold ~f ~init:String.Map.empty defined1 in
    match reach1, reach2 with
    | Unreach, Unreach -> (Unreach, new_defined)
    | _ -> (Reach, new_defined)

  let ( === ) (reach1, defined1) (reach2, defined2) =
    let def_equal def1 def2 =
      match def1, def2 with
      | Undef, Undef -> true
      | Def i1, Def i2 -> i1 = i2
      | Overdef, Overdef -> true
      | _ -> false
    in
    match reach1, reach2 with
    | Reach, Reach
    | Unreach, Unreach -> String.Map.equal def_equal defined1 defined2
    | _ -> false

  let to_string (reach, defined) =
    let def_to_string = function
      | Undef -> "undef"
      | Def i -> "def " ^ (Int64.to_string i)
      | Overdef -> "overdef"
    in
    let f ~key:temp ~data:def acc =
      acc ^ (Printf.sprintf "%s: %s," temp (def_to_string def))
    in
    let defined_string = String.Map.fold ~f ~init:"" defined in
    match reach with
    | Reach -> Printf.sprintf "(reach, [%s])" defined_string
    | Unreach -> Printf.sprintf "(unreach, [%s])" defined_string
end
*)
