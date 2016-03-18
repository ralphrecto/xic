module Long = Int64
open Core.Std
open Async.Std

(* label * adjacent nodes * mark *)
type node = Node of string * string list
type graph = node list

(* label, stmts in block *)
type block = Block of string * Ir.stmt list

(* naming *)
(* The implementation of certain functions, like lowering, generate fresh temps
 * and labels. To make testing easier, it's nice if fresh temps and labels are
 * generated in a simple and consistent way. The way we accomplish this is by
 * exposing a function temp and label which take in an integer and return a
 * Temp and Label respectively. Internally, fresh temps are generated as temp
 * 0, temp 1, temp 2, etc. The same goes for fresh labels. Moreover, we expose
 * a way to reset the fresh temp and fresh label counts to make testing easier.
 *)
val temp  : int -> string
val label : int -> string
val reset_fresh_temp : unit -> unit
val reset_fresh_label : unit -> unit

(* Xi AST -> IR AST *)
val gen_expr : Typecheck.expr -> Ir.expr
(* the control translation for booleans from lecture notes
 * boolean -> true label -> false label -> resulting jumps *)
val gen_control : Typecheck.expr -> string -> string -> Ir.stmt
val gen_stmt : Typecheck.stmt -> Ir.stmt

(* IR lowering *)
val lower_expr : Ir.expr -> Ir.stmt list * Ir.expr
val lower_stmt : Ir.stmt -> Ir.stmt list

(* Basic block reordering *)
val block_reorder : Ir.stmt list -> block list

(* Constant folding @ IR level *)
val ir_constant_folding : Ir.expr -> Ir.expr
