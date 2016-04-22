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
let rec get_subexpr (e: expr) : ExprSet.t =
  match e with
  | BinOp (e1, _, e2) -> add (union (get_subexpr e1) (get_subexpr e2)) e
  | Call (_, elst) ->
    let f acc e = union acc (get_subexpr e) in
    List.fold_left ~f ~init: empty elst
  | Mem (e1, _) -> add (get_subexpr e1) e
  | Temp _ | Const _ | Name _ -> empty
  | ESeq _ -> failwith "shouldn't exist!"

(* first element of the tuple is the set of subexpressions
 * second element of the tuple is the set of temps and mems used by stmt *)
and get_subexpr_stmt (s: stmt) : ExprSet.t =
  match s with
  | CJumpOne (e1, _) -> get_subexpr e1
  | Jump e1 -> get_subexpr e1
  | Exp e1 -> get_subexpr e1
  | Move (e1, e2) -> union (get_subexpr e1) (get_subexpr e2)
  | Seq slst ->
      let f acc s = union acc (get_subexpr_stmt s) in
      List.fold_left ~f ~init: empty slst
  | Label _ | Return -> empty
  | CJump _ -> failwith "shouldn't exist!"

let rec get_mem_temp (e: expr) : ExprSet.t =
  match e with
  | BinOp (e1, _, e2) -> union (get_mem_temp e1) (get_mem_temp e2)
  | Call (_, elst) ->
    let f acc e = union acc (get_mem_temp e) in
    List.fold_left ~f ~init: empty elst
  | Mem (e1, _) -> add (get_mem_temp e1) e
  | Temp _ -> add empty e
  | Const _ | Name _ -> empty
  | ESeq _ -> failwith "shouldn't exist!"

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
  type data =
    | Univ
    | Set of ExprSet.t

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

(**
 * Anticipated Expressions
 * =======================
 * Domain            : Sets of expressions
 * Direction         : Backwards
 * Transfer function : in(n) = use(n) + (out(n) - kill(n))
 * Boundary          : in[exit] = 0
 * Meet (/\)         : intersection
 * Initialization    : in[n] = U
 *)
module BusyExprCFG : CFGWithLatticeT = struct
  module Lattice = BusyExprLattice
  module CFG = IrCfg
  open Lattice
  open CFG
  open IrDataStartExit

  let transfer (e: edge) (d: data) =
    let node = E.dst e in
    match node with
    | Start -> failwith "TODO"
    | Exit -> failwith "TODO"
    | Node d' ->
      let stmt = d'.ir in
      let use = get_subexpr_stmt stmt in
      let kill = kill_stmt stmt in
      let f acc expr =
        let mem_temps = get_mem_temp expr in
        if ExprSet.is_empty (inter mem_temps kill) then
          add acc expr
        else
          acc
      in
      let diff_expr_kill = fold ~f ~init: empty d in
      union use diff_expr_kill
end

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

