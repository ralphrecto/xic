open Core.Std
open Async.Std

(* The AST module contains
 *
 *     1. a module S,
 *     2. a module type TAGS,
 *     3. and a functor Make.
 *
 * S defines the type of an AST and is parameterized on 9 types, each
 * corresponding to one type of AST node:
 *
 *     1. 'p: prog
 *     2. 'u: use
 *     3. 'c: callable
 *     4. 'i: id
 *     5. 'a: avar
 *     6. 'v: var
 *     7. 's: stmt
 *     8. 'e: expr
 *     9. 't: typ
 *
 * Each AST node type is paired with its corresponding type parameter. For
 * example, an expression is bundled as ('e, expr) and a statement is bundled
 * as ('s, stmt). Of course, manually writing the type parameters is tedious.
 * Enter TAGS. If you want to specialize an AST to use a set of tags, simply
 * create a TAGS struct specifying the type of each tag. Then, apply the Make
 * functor.
 *
 * For example, say we wanted to annotated every node with an integer. Here's
 * how we'd do it.
 *
 *     module T = struct
 *       type p = int [@@deriving sexp]
 *       type u = int [@@deriving sexp]
 *       type c = int [@@deriving sexp]
 *       type i = int [@@deriving sexp]
 *       type a = int [@@deriving sexp]
 *       type v = int [@@deriving sexp]
 *       type s = int [@@deriving sexp]
 *       type e = int [@@deriving sexp]
 *       type t = int [@@deriving sexp]
 *     end
 *
 *     include Ast.Make(T)
 *)
module S = struct

  (* top level terms *)
  type ('p,'u,'c,'i,'a,'v,'s,'e,'t) prog = 'p * ('u,'c,'i,'a,'v,'s,'e,'t) raw_prog
  and ('u,'c,'i,'a,'v,'s,'e,'t) raw_prog =
    | Prog of ('u,'i) use list * ('c,'i,'a,'v,'s,'e,'t) callable list

  and ('u, 'i) use = 'u * 'i raw_use
  and 'i raw_use =
    | Use of 'i id

  and ('c,'i,'a,'v,'s,'e,'t) callable = 'c * ('i,'a,'v,'s,'e,'t) raw_callable
  and ('i,'a,'v,'s,'e,'t) raw_callable =
    | Func of 'i id * ('i,'a,'e,'t) avar list * ('i,'e,'t) typ list *
              ('i,'a,'v,'s,'e,'t) stmt
    | Proc of 'i id * ('i,'a,'e,'t) avar list * ('i,'a,'v,'s,'e,'t) stmt

  (* identifiers, variaeles, and annotated variables *)
  and 'i id = 'i * string

  and ('i,'a,'e,'t) avar = 'a * ('i,'e,'t) raw_avar
  and ('i,'e,'t) raw_avar =
    | AId         of 'i id * ('i,'e,'t) typ
    | AUnderscore of ('i,'e,'t) typ

  and ('i,'a,'v,'e,'t) var = 'v * ('i,'a,'e,'t) raw_var
  and ('i,'a,'e,'t) raw_var =
    | AVar of ('i,'a,'e,'t) avar
    | Underscore

  (* statements *)
  and ('i,'a,'v,'s,'e,'t) stmt = 's * ('i,'a,'v,'s,'e,'t) raw_stmt
  and ('i,'a,'v,'s,'e,'t) raw_stmt =
    | Decl     of ('i,'a,'v,'e,'t) var list
    | DeclAsgn of ('i,'a,'v,'e,'t) var list * ('i,'e) expr
    | Asgn     of ('i,'e) expr * ('i,'e) expr
    | Block    of ('i,'a,'v,'s,'e,'t) stmt list
    | Return   of ('i,'e) expr list
    | If       of ('i,'e) expr * ('i,'a,'v,'s,'e,'t) stmt
    | IfElse   of ('i,'e) expr * ('i,'a,'v,'s,'e,'t) stmt * ('i,'a,'v,'s,'e,'t) stmt
    | While    of ('i,'e) expr * ('i,'a,'v,'s,'e,'t) stmt
    | ProcCall of 'i id * ('i,'e) expr list

  (* expressions *)
  and binop_code =
    | MINUS    (* - *)
    | STAR     (* * *)
    | HIGHMULT (* *>> *)
    | DIV      (* / *)
    | MOD      (* % *)
    | PLUS     (* + *)
    | LT       (* < *)
    | LTE      (* <= *)
    | GTE      (* >= *)
    | GT       (* > *)
    | EQEQ     (* == *)
    | NEQ      (* != *)
    | AMP      (* & *)
    | BAR      (* | *)

  and unop_code =
    | UMINUS (* - *)
    | BANG   (* ! *)

  and ('i,'e) expr = 'e * ('i,'e) raw_expr
  and ('i,'e) raw_expr =
    | Int      of Int64.t
    | Bool     of bool
    | String   of string
    | Char     of char
    | Array    of ('i,'e) expr list
    | Id       of 'i id
    | BinOp    of ('i,'e) expr * binop_code * ('i,'e) expr
    | UnOp     of unop_code * ('i,'e) expr
    | Index    of ('i,'e) expr * ('i,'e) expr
    | Length   of ('i,'e) expr
    | FuncCall of 'i id * ('i,'e) expr list

  (* types *)
  and ('i,'e,'t) typ = 't * ('i,'e,'t) raw_typ
  and ('i,'e,'t) raw_typ =
    | TInt
    | TBool
    | TArray of ('i,'e,'t) typ * ('i,'e) expr option
  [@@deriving sexp]
end

let string_of_binop_code (c: S.binop_code) : string =
  match c with
  | S.MINUS    -> "-"
  | S.STAR     -> "*"
  | S.HIGHMULT -> "*>>"
  | S.DIV      -> "/"
  | S.MOD      -> "%"
  | S.PLUS     -> "+"
  | S.LT       -> "<"
  | S.LTE      -> "<="
  | S.GTE      -> ">="
  | S.GT       -> ">"
  | S.EQEQ     -> "=="
  | S.NEQ      -> "!="
  | S.AMP      -> "&"
  | S.BAR      -> "|"

let string_of_unop_code (c: S.unop_code) : string =
  match c with
  | S.UMINUS -> "-"
  | S.BANG   -> "!"

module type TAGS = sig
  type p [@@deriving sexp]
  type u [@@deriving sexp]
  type c [@@deriving sexp]
  type i [@@deriving sexp]
  type a [@@deriving sexp]
  type v [@@deriving sexp]
  type s [@@deriving sexp]
  type e [@@deriving sexp]
  type t [@@deriving sexp]
end

module Make(T: TAGS) = struct
  open T
  type prog     = (p,u,c,i,a,v,s,e,t) S.prog     [@@deriving sexp]
  type use      = (  u,  i          ) S.use      [@@deriving sexp]
  type callable = (    c,i,a,v,s,e,t) S.callable [@@deriving sexp]
  type id       =        i            S.id       [@@deriving sexp]
  type avar     = (      i,a,    e,t) S.avar     [@@deriving sexp]
  type var      = (      i,a,v,  e,t) S.var      [@@deriving sexp]
  type stmt     = (      i,a,v,s,e,t) S.stmt     [@@deriving sexp]
  type expr     = (      i,      e  ) S.expr     [@@deriving sexp]
  type typ      = (      i,      e,t) S.typ      [@@deriving sexp]
end
