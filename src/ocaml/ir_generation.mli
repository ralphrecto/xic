module Long = Int64
open Core.Std
open Async.Std

(* Xi AST -> IR AST *)
val gen_expr : Typecheck.expr -> Ir.expr
val gen_stmt : Typecheck.stmt -> Ir.stmt

(* IR lowering *)
val lower_expr : Ir.expr -> Ir.stmt list * Ir.expr
val lower_stmt : Ir.stmt -> Ir.stmt list

(* Basic block reordering *)
val block_reorder : Ir.stmt list -> Ir.block list

(* Constant folding @ IR level *)
val constant_folding : Ir.expr -> Ir.expr
