open Core.Std
open Async.Std

(* row * col *)
type t = int * int [@@deriving sexp]

type prog     = t Ast.prog     [@@deriving sexp]
type use      = t Ast.use      [@@deriving sexp]
type callable = t Ast.callable [@@deriving sexp]
type id       = t Ast.id       [@@deriving sexp]
type avar     = t Ast.avar     [@@deriving sexp]
type var      = t Ast.var      [@@deriving sexp]
type stmt     = t Ast.stmt     [@@deriving sexp]
type expr     = t Ast.expr     [@@deriving sexp]
type typ      = t Ast.typ      [@@deriving sexp]
