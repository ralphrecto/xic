open Core.Std
open Cfg
open Dataflow
open Ir
open Tiling
open Fresh

module ExprSet = Set.Make (struct type t = expr [@@deriving sexp, compare] end)

let rec get_subexpr (e: expr) : ExprSet.t =
  match e with
  | BinOp (e1, (DIV|MOD), e2) -> ExprSet.union (get_subexpr e1) (get_subexpr e2)
  | BinOp (e1, _, e2) -> ExprSet.add (ExprSet.union (get_subexpr e1) (get_subexpr e2)) e
  | Call (e1, elst) ->
      let exprset1 = get_subexpr e1 in
      let f acc e =
        let set = get_subexpr e in
        ExprSet.union acc set
      in
      List.fold_left ~f ~init: exprset1 elst
  | Mem (e1, _) -> ExprSet.add (get_subexpr e1) e
  | Const _ | Name _ | Temp _ -> ExprSet.empty
  | ESeq _ -> failwith "shouldn't exist!"

and get_subexpr_stmt (s: stmt) : ExprSet.t =
  match s with
  | CJump (e1, _, _) -> get_subexpr e1
  | CJumpOne (e1, _) -> get_subexpr e1
  | Jump e1 -> get_subexpr e1
  | Exp e1 -> get_subexpr e1
  | Move (e1, e2) -> ExprSet.union (get_subexpr e1) (get_subexpr e2)
  | Seq slst ->
      let f acc s =
        let set = get_subexpr_stmt s in
        ExprSet.union acc set
      in
      List.fold_left ~f ~init: ExprSet.empty slst
  | Label _ | Return -> ExprSet.empty

module AvailableExprLattice : LowerSemilattice = struct
  type data = Top | Set of ExprSet.t

  let top = Top

  let ( ** ) = fun x y ->
    match x, y with
    | Top, _ -> y
    | _, Top -> x
    | Set x', Set y' -> Set (ExprSet.inter x' y')

  let ( === ) = fun x y ->
    match x, y with
    | Top, Top -> true
    | Top, _ -> false
    | _, Top -> false
    | Set x', Set y' -> ExprSet.equal x' y'
end
