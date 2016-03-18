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

module D = struct
  include T
  let dummy = (-1, 1)
  let dummy_p = dummy
  let dummy_u = dummy
  let dummy_c = dummy
  let dummy_i = dummy
  let dummy_a = dummy
  let dummy_v = dummy
  let dummy_s = dummy
  let dummy_e = dummy
  let dummy_t = dummy
end

include Ast.Make(T)
include Ast.Abbreviate(D)
