module Long = Int64
open Core.Std
open Async.Std
open Ast
open Typecheck

val ast_constant_folding: Typecheck.typecheck_info -> Typecheck.typecheck_info
