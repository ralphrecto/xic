module Long = Int64
open Core.Std
open Async.Std

(******************************************************************************)
(* Control Flow Graphs                                                        *)
(******************************************************************************)
(* label * adjacent nodes *)
type node = Node of string * string list
type graph = node list
(* label, stmts in block *)
type block = Block of string * Ir.stmt list

val string_of_node   : node -> string
val string_of_graph  : graph -> string
val string_of_block  : block -> string
val string_of_blocks : block list -> string

(******************************************************************************)
(* Naming                                                                     *)
(******************************************************************************)
(* The implementation of certain functions, like lowering, generate fresh temps
 * and labels. To make testing easier, it's nice if fresh temps and labels are
 * generated in a simple and consistent way. The way we accomplish this is by
 * exposing a function temp and label which take in an integer and return a
 * Temp and Label respectively. Internally, fresh temps are generated as temp
 * 0, temp 1, temp 2, etc. The same goes for fresh labels. Moreover, we expose
 * a way to reset the fresh temp and fresh label counts to make testing easier.
 *)
module FreshTemp      : Fresh.S (* __temp   *)
module FreshLabel     : Fresh.S (* __label  *)
module FreshArgReg    : Fresh.S (* _ARG     *)
module FreshRetReg    : Fresh.S (* _RET     *)
module FreshGlobal    : Fresh.S (* _I_g_    *)
module FreshSize      : Fresh.S (* _I_size_ *)
module FreshDV        : Fresh.S (* _I_vt_   *)
module FreshMethod    : Fresh.S (* _I_m_    *)
module FreshClassInit : Fresh.S (* _I_init_ *)

val temp  : int -> string
val label : int -> string
val reset_fresh_temp : unit -> unit
val reset_fresh_label : unit -> unit
val global_temp : string -> Typecheck.Expr.t -> string

(* class_size c = _I_size_c *)
val class_size : string -> string

(* class_dv c = _I_vt_c *)
val class_dv   : string -> string

(* class_method c f = _I_m_c_f *)
val class_method : class_:string -> method_:string -> string

(******************************************************************************)
(* Helpers                                                                    *)
(******************************************************************************)
(* Mallocs n words *)
val malloc_word : int -> Ir.expr

(******************************************************************************)
(* Code Generation                                                            *)
(******************************************************************************)
(* a mapping from function names to their mangled names *)
type callnames = string String.Map.t

type irgen_info = {
  comp_unit  : Ir.comp_unit;
  contexts   : Typecheck.contexts;

  (* symbols to put in .bss section *)
  bss        : string list;

  (* symbols to put in .ctors section *)
  ctors      : string list;
}

(* array concatenation is a pseudo-function *)
val concat_name      : string
val concat_ir        : Ir.stmt
val concat_func_decl : Ir.func_decl

(* Generates global initialization code. global_ir assumes that all multiple
 * declarations and multiple declaration assigments have been flattened into
 * singleton lists. *)
val global_name      : string
val global_ir        : Typecheck.global list -> Typecheck.contexts -> Ir.stmt
val global_func_decl : Typecheck.global list -> Typecheck.contexts -> Ir.func_decl

(* Class initialization code. *)
val class_init_name      : string -> string
val class_init_ir        : string -> Typecheck.contexts -> Ir.stmt
val class_init_func_decl : string -> Typecheck.contexts -> Ir.func_decl

(* gen_control _ _ _ e t f _ = C[[e, t, f]] *)
val gen_control   : callnames -> Typecheck.expr -> string -> string -> Typecheck.contexts -> Ir.stmt
val gen_expr      : callnames -> Typecheck.expr      -> Typecheck.contexts -> Ir.expr
val gen_stmt      : callnames -> Typecheck.stmt      -> Typecheck.contexts -> Ir.stmt
val gen_func_decl : callnames -> Typecheck.callable  -> Typecheck.contexts -> Ir.func_decl
val gen_method    : callnames -> Typecheck.callable  -> Typecheck.contexts -> Ir.func_decl
val gen_class     : callnames -> Typecheck.klass     -> Typecheck.contexts -> Ir.func_decl list
val gen_comp_unit :              Typecheck.full_prog -> Typecheck.contexts -> irgen_info

(******************************************************************************)
(* Lowering                                                                   *)
(******************************************************************************)
val lower_expr : Ir.expr -> Ir.stmt list * Ir.expr
val lower_stmt : Ir.stmt -> Ir.stmt list
val lower_func_decl : Ir.func_decl -> Ir.func_decl
val lower_comp_unit : Ir.comp_unit -> Ir.comp_unit

(******************************************************************************)
(* Basic Block Reordering                                                     *)
(******************************************************************************)
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

(* blocks to stmt *)
val block_to_stmt : block list -> Ir.stmt

(* func_decl block reordering *)
val block_reorder_func_decl : Ir.func_decl -> Ir.func_decl

(* comp_unit block reordering *)
val block_reorder_comp_unit : Ir.comp_unit -> Ir.comp_unit

(******************************************************************************)
(* Constant Folding                                                           *)
(******************************************************************************)
val ir_constant_folding : Ir.comp_unit -> Ir.comp_unit
