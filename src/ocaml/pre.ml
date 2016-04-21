open Core.Std
open Cfg
open Dataflow
open Ir
open Tiling
open Fresh

module ExprSet = Set.Make (struct type t = expr [@@deriving sexp, compare] end)

open ExprSet

(* first element of the tuple is the set of subexpressions
 * second element of the tuple is the set of temps and mems used by expr *)
let rec get_subexpr (e: expr) : (ExprSet.t * ExprSet.t) =
  match e with
  | BinOp (e1, _, e2) ->
    let (subexpr1, mem_temp1) = get_subexpr e1 in
    let (subexpr2, mem_temp2) = get_subexpr e2 in
    (add (union subexpr1 subexpr2) e, union mem_temp1 mem_temp2)
  | Call (_, elst) ->
    let f (acc1, acc2) e =
      let (subexpr, mem_temp) = get_subexpr e in
      (union acc1 subexpr, union acc2 mem_temp)
    in
    List.fold_left ~f ~init: (empty, empty) elst
  | Mem (e1, _) ->
    let (subexpr, mem_temp) = get_subexpr e1 in
    (add subexpr e, add mem_temp e)
  | Temp _ -> (empty, add empty e)
  | Const _ | Name _ -> (empty, empty)
  | ESeq _ -> failwith "shouldn't exist!"

(* first element of the tuple is the set of subexpressions
 * second element of the tuple is the set of temps and mems used by stmt *)
and get_subexpr_stmt (s: stmt) : (ExprSet.t * ExprSet.t) =
  match s with
  | CJumpOne (e1, _) -> get_subexpr e1
  | Jump e1 -> get_subexpr e1
  | Exp e1 -> get_subexpr e1
  | Move (e1, e2) ->
    let (subexpr1, mem_temp1) = get_subexpr e1 in
    let (subexpr2, mem_temp2) = get_subexpr e2 in
    (union subexpr1 subexpr2, union mem_temp1 mem_temp2)
  | Seq slst ->
      let f (acc1, acc2) s =
        let (subexpr, mem_temp) = get_subexpr_stmt s in
        (union acc1 subexpr, union acc2 mem_temp)
      in
      List.fold_left ~f ~init: (empty, empty) slst
  | Label _ | Return -> (empty, empty)
  | CJump _ -> failwith "shouldn't exist!"

let rec kill_func_args (elst: expr list) : ExprSet.t =
  let f acc e =
    match e with
    | Mem _ -> add acc e
    | Call (_, elst') -> kill_func_args elst'
    | _ -> empty
  in
  List.fold_left ~f ~init: empty elst

and kill_expr (e: expr) : ExprSet.t =
  match e with
  | Call (_, elst) -> kill_func_args elst
  | Temp _
  | Mem _ -> add empty e
  | BinOp _
  | Const _
  | Name _ -> empty
  | ESeq _ -> failwith "shouldn't exist!"

and kill_stmt (s: stmt) : ExprSet.t =
  match s with
  | Move ((Temp _ | Mem _) as e1, Call (_, elst)) ->
    let set = add empty e1 in
    union set (kill_func_args elst)
  | Move ((Temp _ | Mem _) as e1, _) -> add empty e1
  | Seq slst ->
      let f acc s =
        let set = kill_stmt s in
        union acc set
      in
      List.fold_left ~f ~init: empty slst
  | Exp e -> kill_expr e
  | CJumpOne _
  | Jump _
  | Label _
  | Return -> empty
  | Move _
  | CJump _ -> failwith "shouldn't exist!"

module BusyExprLattice: LowerSemilattice = struct
  type data = Univ | Set of t

  let top = Univ

  let ( ** ) = fun x y ->
    match x, y with
    | Univ, _ -> y
    | _, Univ -> x
    | Set x', Set y' -> Set (inter x' y')

  let ( === ) = fun x y ->
    match x, y with
    | Univ, Univ -> true
    | Univ, _ -> false
    | _, Univ -> false
    | Set x', Set y' -> equal x' y'
end

(* module BusyExprCFG : CFGWithLatticeT = struct *)
  (* module Lattice = BusyExprLattice *)
  (* module CFG = *)
  (* open Lattice *)
  (* open CFG *)

  (* let transfer (e: edge) (d: data) = *)

(* end *)

module AvailExprLattice : LowerSemilattice = struct
  type data = Univ | Set of t

  let top = Univ

  let ( ** ) = fun x y ->
    match x, y with
    | Univ, _ -> y
    | _, Univ -> x
    | Set x', Set y' -> Set (inter x' y')

  let ( === ) = fun x y ->
    match x, y with
    | Univ, Univ -> true
    | Univ, _ -> false
    | _, Univ -> false
    | Set x', Set y' -> equal x' y'
end

