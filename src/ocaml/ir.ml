open Core.Std
open Async.Std

type expr =
  | BinOp of expr * binop_code * expr
  | Call of expr * expr list * int
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

and stmt =
  | CJump of expr * string * string
	| CJumpOne of expr * string
  | Jump of expr
  | Exp of expr
  | Label of string
  | Move of expr * expr
  | Seq of stmt list
  | Return

and func_decl = string * stmt

and comp_unit = string * func_decl String.Map.t

let string_of_binop_code = function
  | ADD     -> "+"
  | SUB     -> "-"
  | MUL     -> "*"
  | HMUL    -> "*>>"
  | DIV     -> "/"
  | MOD     -> "%"
  | AND     -> "&"
  | OR      -> "|"
  | XOR     -> "^"
  | LSHIFT  -> "<<"
  | RSHIFT  -> ">>"
  | ARSHIFT -> ">>>"
  | EQ      -> "=="
  | NEQ     -> "!="
  | LT      -> "<"
  | GT      -> ">"
  | LEQ     -> "<="
  | GEQ     -> ">="

let rec string_of_expr e =
  let sob = string_of_binop_code in
  let soe = string_of_expr in
  let sos = string_of_stmt in
  match e with
  | BinOp (lhs, o, rhs) -> sprintf "%s%s%s" (soe lhs) (sob o) (soe rhs)
  | Call (f, args, _) -> sprintf "%s(%s)" (soe f) (Util.commas (List.map ~f:soe args))
  | Const i -> Int64.to_string i
  | ESeq (s, e) -> sprintf "(%s;%s)" (sos s) (soe e)
  | Mem (e, _) -> sprintf "mem %s" (soe e)
  | Name s -> sprintf "name %s" s
  | Temp t -> sprintf "temp %s" t

and string_of_stmt s =
  let soe = string_of_expr in
  let sos = string_of_stmt in
  match s with
  | CJump (e, t, f) -> sprintf "cjump %s %s %s" (soe e) t f
	| CJumpOne (e, l) -> sprintf "cjump %s %s" (soe e) l
  | Jump e -> sprintf "jump %s" (soe e)
  | Exp e -> sprintf "%s" (soe e)
  | Label l -> sprintf "%s:" l
  | Move (lhs, rhs) -> sprintf "%s:=%s" (soe lhs) (soe rhs)
  | Seq ss -> sprintf "(%s)" (String.concat ~sep:";" (List.map ~f:sos ss))
  | Return -> "return"

let string_of_stmts ss =
  String.concat ~sep:";" (List.map ~f:string_of_stmt ss)
