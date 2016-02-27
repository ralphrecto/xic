open Core.Std
open Async.Std

(* (row, col) *)
type pos = int * int [@@deriving sexp]

module T = struct
  type p = pos [@@deriving sexp]
  type u = pos [@@deriving sexp]
  type c = pos [@@deriving sexp]
  type i = pos [@@deriving sexp]
  type a = pos [@@deriving sexp]
  type v = pos [@@deriving sexp]
  type s = pos [@@deriving sexp]
  type e = pos [@@deriving sexp]
  type t = pos [@@deriving sexp]
end

include Ast.Make(T)
