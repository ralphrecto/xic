open Core.Std
open Async.Std

(* top level terms *)
type 'a prog = 'a * 'a raw_prog
and 'a raw_prog =
  | Prog of 'a use list * 'a callable list

and 'a use = 'a * 'a raw_use
and 'a raw_use =
  | Use of 'a id

and 'a callable = 'a * 'a raw_callable
and 'a raw_callable =
  | Func of 'a id * 'a avar list * 'a typ list * 'a stmt
  | Proc of 'a id * 'a avar list * 'a stmt

(* identifiers, variables, and annotated variables *)
and 'a id = 'a * 'a raw_id
and 'a raw_id = string

and 'a avar = 'a * 'a raw_avar
and 'a raw_avar =
  | AId         of 'a id * 'a typ
  | AUnderscore of 'a typ

and 'a var = 'a * 'a raw_var
and 'a raw_var =
  | AnnotatedVar of 'a avar
  | Underscore

(* statements *)
and 'a stmt = 'a * 'a raw_stmt
and 'a raw_stmt =
  | Decl     of 'a var list
  | DeclAsgn of 'a var list * 'a expr
  | Asgn     of 'a expr * 'a expr
  | Block    of 'a stmt list * 'a expr list option
  | If       of 'a expr * 'a stmt
  | IfElse   of 'a expr * 'a stmt * 'a stmt
  | While    of 'a expr * 'a stmt
  | ProcCall of 'a id * 'a expr list

(* expressions *)
and binop_code =
  | Minus    (* - *)
  | Star     (* * *)
  | Highmult (* *>> *)
  | Div      (* / *)
  | Mod      (* % *)
  | Plus     (* + *)
  | Lt       (* < *)
  | Lte      (* <= *)
  | Gte      (* >= *)
  | Gt       (* > *)
  | Eqeq     (* == *)
  | Neq      (* != *)
  | Amp      (* & *)
  | Bar      (* | *)

and unop_code =
  | Uminus (* - *)
  | Bang   (* ! *)

and 'a expr = 'a * 'a raw_expr
and 'a raw_expr =
  | Int      of Int64.t
  | Bool     of bool
  | String   of string
  | Char     of char
  | Array    of 'a expr list
  | Id       of 'a id
  | BinOp    of 'a expr * binop_code * 'a expr
  | UnOp     of unop_code * 'a expr
  | Index    of 'a expr * 'a expr
  | Length   of 'a expr
  | FuncCall of 'a id * 'a expr list

(* types *)
and 'a typ = 'a * 'a raw_typ
and 'a raw_typ =
  | TInt   of 'a
  | TBool  of 'a
  | TArray of 'a typ * 'a expr option
[@@deriving sexp]
