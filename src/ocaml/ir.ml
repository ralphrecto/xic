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
  | Jump of expr
  | Exp of expr
  | Label of string
  | Move of expr * expr
  | Seq of stmt list
  | Return

and func_decl =
  | ProcDecl of string * stmt
  | FuncDecl of string * stmt

(* name, functions, seq stmt *)
and comp_unit = string * func_decl String.Map.t * stmt
