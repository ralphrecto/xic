open Core.Std
open Async.Std

module Expr: sig
  type t =
    | IntT
    | BoolT
    | UnitT
    | ArrayT of t
    | TupleT of t list (* len >= 2 *)
    | EmptyArray
  [@@deriving sexp]

  val to_string: t -> string
end

module Stmt: sig
  type t =
    | One  (* unit *)
    | Zero (* void *)
  [@@deriving sexp]
end

module Sigma: sig
  type t =
    | Var of Expr.t
    | Function of Expr.t * Expr.t
  [@@deriving sexp]
end

module Tags: sig
  type p = unit             [@@deriving sexp]
  type u = unit             [@@deriving sexp]
  type c = Expr.t * Expr.t  [@@deriving sexp]
  type i = unit             [@@deriving sexp]
  type a = Expr.t           [@@deriving sexp]
  type v = Expr.t           [@@deriving sexp]
  type s = Stmt.t           [@@deriving sexp]
  type e = Expr.t           [@@deriving sexp]
  type t = Expr.t           [@@deriving sexp]
end
include (module type of Ast.Make(Tags))

type error_msg = Pos.pos * string
type context = Sigma.t String.Map.t

val expr_typecheck: context ->           Pos.expr     -> (expr,     error_msg) Result.t
val typ_typecheck:  context ->           Pos.typ      -> (typ,      error_msg) Result.t
val avar_typecheck: context ->           Pos.avar     -> (avar,     error_msg) Result.t
val var_typecheck:  context ->           Pos.var      -> (var,      error_msg) Result.t
val stmt_typecheck: context -> Expr.t -> Pos.stmt     -> (stmt,     error_msg) Result.t
val fst_func_pass:  context ->           Pos.callable -> (context,  error_msg) Result.t
val snd_func_pass:  context ->           Pos.callable -> (callable, error_msg) Result.t
val prog_typecheck:                      Pos.prog     -> (prog,     error_msg) Result.t
