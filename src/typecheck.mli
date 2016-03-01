open Core.Std
open Async.Std

type expr_t =
  | IntT
  | BoolT
  | UnitT
  | ArrayT of expr_t
  | TupleT of expr_t list (* len >= 2 *)
  | EmptyArray
[@@deriving sexp]

type stmt_t =
  | One   (* unit *)
  | Zero  (* void *)
[@@deriving sexp]

type sigma =
  | Var of expr_t
  | Function of expr_t * expr_t
[@@deriving sexp]

module Tags: sig
  type p = unit             [@@deriving sexp]
  type u = unit             [@@deriving sexp]
  type c = expr_t * expr_t  [@@deriving sexp]
  type i = unit             [@@deriving sexp]
  type a = expr_t           [@@deriving sexp]
  type v = expr_t           [@@deriving sexp]
  type s = stmt_t           [@@deriving sexp]
  type e = expr_t           [@@deriving sexp]
  type t = expr_t           [@@deriving sexp]
end
include (module type of Ast.Make(Tags))

type error_msg = string
type context = sigma String.Map.t

val dummy: unit -> unit

val expr_typecheck: context ->           Pos.expr     -> (expr,     error_msg) Result.t
val stmt_typecheck: context -> expr_t -> Pos.stmt     -> (stmt,     error_msg) Result.t
val fst_func_pass:  context ->           Pos.callable -> (context,  error_msg) Result.t
val snd_func_pass:  context ->           Pos.callable -> (callable, error_msg) Result.t
val prog_typecheck:                      Pos.prog     -> (prog,     error_msg) Result.t
