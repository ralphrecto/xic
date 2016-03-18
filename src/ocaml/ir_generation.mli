module Long = Int64
open Core.Std
open Async.Std

(* label * adjacent nodes *)
type node = Node of string * string list
type graph = node list
(* label, stmts in block *)
type block = Block of string * Ir.stmt list

val string_of_node  : node -> string
val string_of_graph : graph -> string

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
(* `gen_block ss` chunks ss into blocks. The contents of the blocks are
 * reversed to make inspecting the last element of the block easier, and the
 * labels at the beginning of each block are pulled from the stmt list into the
 * block itself.
 *
 *   gen_block [Label "a"; Exp 1; Exp 2] = [Block ("a", [Exp 2; Exp 1])]
 *)
val gen_block : Ir.stmt list -> block list

(* `connect_blocks blocks` iterates through blocks and if a block does not end
 * with a cjump, jump or a return, then it adds a jump to the next block. It
 * also adds a return to the last block if it doesn't end in
 * return/jump/cjump. *)
val connect_blocks : block list -> block list

(* Generates a control flow graph from a list of basic blocks. *)
val create_graph : block list -> graph

(* Given a graph g and a node n, find and return an arbitrary valid trace through
 * g rooted at n. A trace l1 ... ln is valid given it satisfies some
 * properties:
 *     - n > 1
 *     - li != lj for all i \neq j
 *     - l1 -> l2 -> ... -> ln
 *     - li in g for all i
 *)
val valid_trace : graph -> string list -> bool
val find_trace : graph -> node -> string list

(* Repeatedly call find_trace and flatten blocks. Each returned list is a
 * trace. A valid seq is one in which
 *     - first block in first trace of seq is first block in graph
 *     - seq partitions graph
 *     - no duplicate labels
 *     - all traces are valid
 *)
val valid_seq : graph -> string list list -> bool
val find_seq : graph -> string list list

(* Tidies up the reorderd blocks; see Appel page 172. *)
val tidy : block list -> block list

(* Full basic block reordering. *)
val block_reorder : Ir.stmt list -> block list

(* Constant folding @ IR level *)
val ir_constant_folding : Ir.expr -> Ir.expr
