open Core.Std
open Async.Std
open Typecheck

(* no ESeq or Call in lowered ir *)
type expr =
  | BinOp of expr * binop_code * expr
  | Call of expr * expr list
  | Const of Int64.t
  | ESeq of stmt * expr
  | Mem of expr * mem_type
  | Name of string
  | Temp of string

and binop_code =
  | ADD
  | SUB
  | MUL
  | HMUL
  | DIV
  | MOD
  | AND
  | OR
  | XOR
  | LSHIFT
  | RSHIFT
  | ARSHIFT
  | EQ
  | NEQ
  | LT
  | GT
  | LEQ
  | GEQ

and mem_type =
  | NORMAL
  | IMMUTABLE

(* in lowered ir:
 * Move (Name of string, expr)
 * Move (Temp of string, Call of expr * expr list * int)
 * Exp (Call of expr * expr list * int)
 * Jump of expr
 * CJump of expr * string * string -- in block reordering second label should be removed
 * Label of string
*)
and stmt =
  | CJump of expr * string * string
  (* only used after block reordering *)
  | CJumpOne of expr * string
  | Jump of expr
  | Exp of expr
  | Label of string
  | Move of expr * expr
  | Seq of stmt list
  | Return

(* function name, block, type *)
and func_decl = string * stmt * (Expr.t * Expr.t)

(* name, functions *)
and comp_unit = string * func_decl String.Map.t

(* abbreviations *)
module Abbreviations: sig
  val call  : expr -> expr list -> expr
  val const : Int64.t -> expr
  val eseq  : stmt -> expr -> expr
  val mem   : expr -> expr
  val name  : string -> expr
  val temp  : string -> expr

  val cjump    : expr -> string -> string -> stmt
  val cjumpone : expr -> string -> stmt
  val jump     : expr -> stmt
  val exp      : expr -> stmt
  val label    : string -> stmt
  val move     : expr -> expr -> stmt
  val seq      : stmt list -> stmt
  val return   : stmt

  val add_     : expr -> expr -> expr
  val sub_     : expr -> expr -> expr
  val mul_     : expr -> expr -> expr
  val hmul_    : expr -> expr -> expr
  val div_     : expr -> expr -> expr
  val mod_     : expr -> expr -> expr
  val and_     : expr -> expr -> expr
  val or_      : expr -> expr -> expr
  val xor_     : expr -> expr -> expr
  val lshift_  : expr -> expr -> expr
  val rshift_  : expr -> expr -> expr
  val arshift_ : expr -> expr -> expr
  val eq_      : expr -> expr -> expr
  val neq_     : expr -> expr -> expr
  val lt_      : expr -> expr -> expr
  val gt_      : expr -> expr -> expr
  val leq_     : expr -> expr -> expr
  val geq_     : expr -> expr -> expr

  val zero  : expr
  val one   : expr
  val two   : expr
  val three : expr
  val four  : expr
  val five  : expr
  val six   : expr
  val seven : expr
  val eight : expr
  val nine  : expr
  val ten   : expr
end

module Infix: sig
  val ( +   ) : expr -> expr -> expr
  val ( -   ) : expr -> expr -> expr
  val ( *   ) : expr -> expr -> expr
  val ( *>> ) : expr -> expr -> expr
  val ( /   ) : expr -> expr -> expr
  val ( %   ) : expr -> expr -> expr
  val ( &   ) : expr -> expr -> expr
  val ( ||  ) : expr -> expr -> expr
  val ( ^   ) : expr -> expr -> expr
  val ( <<  ) : expr -> expr -> expr
  val ( >>  ) : expr -> expr -> expr
  val ( >>> ) : expr -> expr -> expr
  val ( ==  ) : expr -> expr -> expr
  val ( !=  ) : expr -> expr -> expr
  val ( <   ) : expr -> expr -> expr
  val ( >   ) : expr -> expr -> expr
  val ( <=  ) : expr -> expr -> expr
  val ( >=  ) : expr -> expr -> expr
end

(* pretty printing *)
val string_of_binop_code : binop_code -> string
val string_of_expr       : expr       -> string
val string_of_stmt       : stmt       -> string
val string_of_stmts      : stmt list  -> string
