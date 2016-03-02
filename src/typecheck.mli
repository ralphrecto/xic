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
  val of_typ: Pos.typ -> t
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

module Error: sig
  type t = Pos.pos * string
end

type context = Sigma.t String.Map.t
module Context: sig
  include (module type of String.Map)
  val var:  Pos.pos -> context -> string -> (Expr.t, Error.t) Result.t
  val func: Pos.pos -> context -> string -> (Expr.t * Expr.t, Error.t) Result.t
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

val expr_typecheck: context ->           Pos.expr     -> (expr,     Error.t) Result.t
val typ_typecheck:  context ->           Pos.typ      -> (typ,      Error.t) Result.t
val avar_typecheck: context ->           Pos.avar     -> (avar,     Error.t) Result.t
val var_typecheck:  context ->           Pos.var      -> (var,      Error.t) Result.t
val stmt_typecheck: context -> Expr.t -> Pos.stmt     -> (stmt,     Error.t) Result.t
val fst_func_pass:  context ->           Pos.callable -> (context,  Error.t) Result.t
val snd_func_pass:  context ->           Pos.callable -> (callable, Error.t) Result.t
val prog_typecheck:                      Pos.prog     -> (prog,     Error.t) Result.t
