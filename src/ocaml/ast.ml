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
 *     1. 'p: prog and interface
 *     2. 'u: use
 *     3. 'c: callable and callable_decl
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
  type ('p,'u,'g,'k,'c,'i,'a,'v,'s,'e,'t) full_prog =
      (* program name, prog, interfaces*)
      FullProg of string *
                  ('p,'u,'g,'k,'c,'i,'a,'v,'s,'e,'t) prog *
                  ('p,'u,'k,'c,'i,'a,'v,'s,'e,'t) interface list

  and ('p,'u,'k,'c,'i,'a,'v,'s,'e,'t) interface = 'p * ('u,'k,'c,'i,'a,'v,'s,'e,'t) raw_interface
  and ('u,'k,'c,'i,'a,'v,'s,'e,'t) raw_interface =
      Interface of string * ('u,'i) use list *
        ('k,'c,'i,'a,'v,'s,'e,'t) klass_decl list *
        ('c,'i,'a,'v,'s,'e,'t) callable_decl list

  and ('p,'u,'g,'k,'c,'i,'a,'v,'s,'e,'t) prog = 'p * ('u,'g,'k,'c,'i,'a,'v,'s,'e,'t) raw_prog
  and ('u,'k,'g,'c,'i,'a,'v,'s,'e,'t) raw_prog =
    | Prog of ('u,'i) use list *
              ('g,'i,'a,'v,'e,'t) global list *
              ('k,'c,'i,'a,'v,'s,'e,'t) klass list *
              ('c,'i,'a,'v,'s,'e,'t) callable list

  and ('u,'i) use = 'u * 'i raw_use
  and 'i raw_use =
    | Use of 'i id

  and ('g,'i,'a,'v,'e,'t) global = 'g * ('i,'a,'v,'e,'t) raw_global
  and ('i,'a,'v,'e,'t) raw_global =
    | Gdecl           of ('i,'a,'v,'e,'t) var list
    | GdeclAsgn       of ('i,'a,'v,'e,'t) var list * ('i,'e) expr

  and ('k,'c,'i,'a,'v,'s,'e,'t) klass = 'k * ('c,'i,'a,'v,'s,'e,'t) raw_klass
  and ('c,'i,'a,'v,'s,'e,'t) raw_klass =
    (* classname, optional superclass, fields, methods *)
    | Klass of 'i id * 'i id option * ('i,'a,'e,'t) avar list * ('c,'i,'a,'v,'s,'e,'t) callable list

  and ('k,'c,'i,'a,'v,'s,'e,'t) klass_decl = 'k * ('c,'i,'a,'v,'s,'e,'t) raw_klass_decl
  and ('c,'i,'a,'v,'s,'e,'t) raw_klass_decl =
    | KlassDecl of 'i id * 'i id option * ('c,'i,'a,'v,'s,'e,'t) callable_decl list

  and ('c,'i,'a,'v,'s,'e,'t) callable = 'c * ('i,'a,'v,'s,'e,'t) raw_callable
  and ('i,'a,'v,'s,'e,'t) raw_callable =
    | Func of 'i id * ('i,'a,'e,'t) avar list * ('i,'e,'t) typ list * ('i,'a,'v,'s,'e,'t) stmt
    | Proc of 'i id * ('i,'a,'e,'t) avar list * ('i,'a,'v,'s,'e,'t) stmt

  and ('c,'i,'a,'v,'s,'e,'t) callable_decl = 'c * ('i,'a,'v,'s,'e,'t) raw_callable_decl
  and ('i,'a,'v,'s,'e,'t) raw_callable_decl =
    | FuncDecl of 'i id * ('i,'a,'e,'t) avar list * ('i,'e,'t) typ list
    | ProcDecl of 'i id * ('i,'a,'e,'t) avar list

  (* identifiers, variables, and annotated variables *)
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
    | Break
    | Decl           of ('i,'a,'v,'e,'t) var list
    | DeclAsgn       of ('i,'a,'v,'e,'t) var list * ('i,'e) expr
    | Asgn           of ('i,'e) expr * ('i,'e) expr
    | Block          of ('i,'a,'v,'s,'e,'t) stmt list
    | Return         of ('i,'e) expr list
    | If             of ('i,'e) expr * ('i,'a,'v,'s,'e,'t) stmt
    | IfElse         of ('i,'e) expr * ('i,'a,'v,'s,'e,'t) stmt * ('i,'a,'v,'s,'e,'t) stmt
    | While          of ('i,'e) expr * ('i,'a,'v,'s,'e,'t) stmt
    | ProcCall       of 'i id * ('i,'e) expr list
    | MethodCallStmt of ('i,'e) expr * 'i id * ('i,'e) expr list

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
    | Null
    | Int         of Int64.t
    | Bool        of bool
    | String      of string
    | Char        of char
    | Array       of ('i,'e) expr list
    | Id          of 'i id
    | BinOp       of ('i,'e) expr * binop_code * ('i,'e) expr
    | UnOp        of unop_code * ('i,'e) expr
    | Index       of ('i,'e) expr * ('i,'e) expr
    | Length      of ('i,'e) expr
    | FuncCall    of 'i id * ('i,'e) expr list
    | New         of 'i id
    | FieldAccess of ('i,'e) expr * 'i id
    | MethodCall  of ('i,'e) expr * 'i id * ('i,'e) expr list

  (* types *)
  and ('i,'e,'t) typ = 't * ('i,'e,'t) raw_typ
  and ('i,'e,'t) raw_typ =
    | TInt
    | TBool
    | TArray of ('i,'e,'t) typ * ('i,'e) expr option
    | TKlass of 'i id
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

let rec string_of_expr (_, e) : string =
  let soe = string_of_expr in
  match e with
  | S.Int i -> sprintf "%s" (Int64.to_string_hum i)
  | S.Bool b -> sprintf "%b" b
  | S.String s -> sprintf "\"%s\"" s
  | S.Char c -> sprintf "'%c'" c
  | S.Array es -> sprintf "{%s}" (Util.commas (List.map ~f:soe es))
  | S.Id (_, x) -> x
  | S.BinOp (lhs, c, rhs) -> sprintf "%s%s%s" (soe lhs)
                               (string_of_binop_code c)
                               (soe rhs)
  | S.UnOp (c, e) -> sprintf "%s%s" (string_of_unop_code c) (soe e)
  | S.Index (a, i) -> sprintf "%s[%s]" (soe a) (soe i)
  | S.Length e -> sprintf "length(%s)" (soe e)
  | S.FuncCall ((_, f), args) -> sprintf "%s(%s)" f (Util.commas (List.map ~f:soe args))
  | _ -> failwith "TODO"

let rec string_of_typ (_, t) : string =
  let sot = string_of_typ in
  match t with
  | S.TInt -> "int"
  | S.TBool -> "bool"
  | S.TArray (t, None) -> sprintf "%s[]" (sot t)
  | S.TArray (t, Some e) -> sprintf "%s[%s]" (sot t) (string_of_expr e)
  | _ -> failwith "TODO"

let string_of_avar (_, a) : string =
  match a with
  | S.AId ((_, x), t) -> sprintf "%s: %s" x (string_of_typ t)
  | S.AUnderscore t -> sprintf "_: %s" (string_of_typ t)

let string_of_var (_, v) : string =
  match v with
  | S.AVar a -> string_of_avar a
  | S.Underscore -> "_"

let rec string_of_stmt (_, s) : string =
  let sos = string_of_stmt in
  match s with
  | S.Decl xs -> sprintf "%s;" (Util.commas (List.map ~f:string_of_var xs))
  | S.DeclAsgn (xs, e) -> sprintf "%s=%s;" (Util.commas (List.map ~f:string_of_var xs))
                            (string_of_expr e)
  | S.Asgn (lhs, rhs) -> sprintf "%s=%s;" (string_of_expr lhs) (string_of_expr rhs)
  | S.Block ss -> sprintf "{%s}" (Util.commas (List.map ~f:sos ss))
  | S.Return es -> sprintf "return %s" (Util.commas (List.map ~f:string_of_expr es))
  | S.If (b, t) -> sprintf "if(%s) %s" (string_of_expr b) (sos t)
  | S.IfElse (b, t, f) -> sprintf "if(%s) %s else %s" (string_of_expr b) (sos t) (sos f)
  | S.While (e, s) -> sprintf "while(%s) %s" (string_of_expr e) (sos s)
  | S.ProcCall ((_, p), args) ->
    sprintf "%s(%s)" p (Util.commas (List.map ~f:string_of_expr args))
  | _ -> failwith "TODO"

module type TAGS = sig
  type p [@@deriving sexp]
  type u [@@deriving sexp]
  type g [@@deriving sexp]
  type k [@@deriving sexp]
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
  type full_prog     = (p,u,g,k,c,i,a,v,s,e,t) S.full_prog     [@@deriving sexp]
  type interface     = (p,u,  k,c,i,a,v,s,e,t) S.interface     [@@deriving sexp]
  type prog          = (p,u,g,k,c,i,a,v,s,e,t) S.prog          [@@deriving sexp]
  type use           = (  u,      i          ) S.use           [@@deriving sexp]
  type global        = (    g,    i,a,v,  e,t) S.global        [@@deriving sexp]
  type klass         = (      k,c,i,a,v,s,e,t) S.klass         [@@deriving sexp]
  type klass_decl    = (      k,c,i,a,v,s,e,t) S.klass_decl    [@@deriving sexp]
  type callable      = (        c,i,a,v,s,e,t) S.callable      [@@deriving sexp]
  type callable_decl = (        c,i,a,v,s,e,t) S.callable_decl [@@deriving sexp]
  type id            =            i            S.id            [@@deriving sexp]
  type avar          = (          i,a,    e,t) S.avar          [@@deriving sexp]
  type var           = (          i,a,v,  e,t) S.var           [@@deriving sexp]
  type stmt          = (          i,a,v,s,e,t) S.stmt          [@@deriving sexp]
  type expr          = (          i,      e  ) S.expr          [@@deriving sexp]
  type typ           = (          i,      e,t) S.typ           [@@deriving sexp]

  type raw_interface     = (  u,  k,c,i,a,v,s,e,t) S.raw_interface     [@@deriving sexp]
  type raw_prog          = (  u,g,k,c,i,a,v,s,e,t) S.raw_prog          [@@deriving sexp]
  type raw_use           = (          i          ) S.raw_use           [@@deriving sexp]
  type raw_global        = (          i,a,v,  e,t) S.raw_global        [@@deriving sexp]
  type raw_klass         = (        c,i,a,v,s,e,t) S.raw_klass         [@@deriving sexp]
  type raw_callable      = (          i,a,v,s,e,t) S.raw_callable      [@@deriving sexp]
  type raw_callable_decl = (          i,a,v,s,e,t) S.raw_callable_decl [@@deriving sexp]
  type raw_avar          = (          i,      e,t) S.raw_avar          [@@deriving sexp]
  type raw_var           = (          i,a,    e,t) S.raw_var           [@@deriving sexp]
  type raw_stmt          = (          i,a,v,s,e,t) S.raw_stmt          [@@deriving sexp]
  type raw_expr          = (          i,      e  ) S.raw_expr          [@@deriving sexp]
  type raw_typ           = (          i,      e,t) S.raw_typ           [@@deriving sexp]
end

(* Often times, you want to construct an AST term without really caring about
 * the values that it's tagged with. For example, we sometimes construct a
 * Pos.expr without caring what the position of the expression is. The DUMMIES
 * module type and Abbreviate functor generate helper functions to construct
 * AST terms with dummy tags. Use it like this:
 *
 *     module T = struct
 *       type p = int   [@@deriving sexp]
 *       type u = bool  [@@deriving sexp]
 *       type c = unit  [@@deriving sexp]
 *       type i = float [@@deriving sexp]
 *       type a = char  [@@deriving sexp]
 *       type v = int   [@@deriving sexp]
 *       type s = int   [@@deriving sexp]
 *       type e = int   [@@deriving sexp]
 *       type t = int   [@@deriving sexp]
 *     end
 *     include Ast.Make(T)
 *
 *     module D = struct
 *       include T
 *       include T
 *       let dummy_p = 0
 *       let dummy_u = false
 *       let dummy_c = ()
 *       let dummy_i = 0.0
 *       let dummy_a = 'a'
 *       let dummy_v = 0
 *       let dummy_s = 0
 *       let dummy_e = 0
 *       let dummy_t = 0
 *     end
 *     module Abbreviations = Ast.Abbreviate(D)
 *
 * Then you can build dummy terms like Abbreviations.(one + two - three)
*)
module type DUMMIES = sig
  type p [@@deriving sexp]
  type u [@@deriving sexp]
  type g [@@deriving sexp]
  type k [@@deriving sexp]
  type c [@@deriving sexp]
  type i [@@deriving sexp]
  type a [@@deriving sexp]
  type v [@@deriving sexp]
  type s [@@deriving sexp]
  type e [@@deriving sexp]
  type t [@@deriving sexp]

  val dummy_p: p
  val dummy_u: u
  val dummy_g: g
  val dummy_k: k
  val dummy_c: c
  val dummy_i: i
  val dummy_a: a
  val dummy_v: v
  val dummy_s: s
  val dummy_e: e
  val dummy_t: t
end

module Abbreviate(D: DUMMIES) = struct
  open S
  open D
  let raw_id i = (dummy_i, i)

  let fullprog name prog ilist = FullProg (name, prog, ilist)

  let prog uses globals classes calls = (dummy_p, Prog (uses, globals, classes, calls))
  let interface name ulist klist dlist = (dummy_p, Interface (name, ulist, klist, dlist))

  let gdecl vs = (dummy_s, Gdecl vs)
  let gdeclasgn vs es = (dummy_s, GdeclAsgn (vs, es))

  let klass name super fields methods = (dummy_p, Klass (name, super, fields, methods))
  let klassdecl name super methods = (dummy_p, KlassDecl (name, super, methods))

  let funcdecl f args typs = (dummy_c, FuncDecl ((raw_id f), args, typs))
  let procdecl f args = (dummy_c, ProcDecl ((raw_id f), args))

  let use x = (dummy_u, Use (raw_id x))

  let func f args typs s = (dummy_c, Func ((raw_id f), args, typs, s))
  let proc f args s = (dummy_c, Proc ((raw_id f), args, s))

  let aid x t = (dummy_a, AId ((raw_id x), t))
  let aunderscore t = (dummy_a, AUnderscore t)

  let avar a = (dummy_v, AVar a)
  let underscore = (dummy_v, Underscore)

  let decl vs = (dummy_s, Decl vs)
  let declasgn vs es = (dummy_s, DeclAsgn (vs, es))
  let asgn lhs rhs = (dummy_s, Asgn (lhs, rhs))
  let block ss = (dummy_s, Block ss)
  let return es = (dummy_s, Return es)
  let if_ e t = (dummy_s, If (e, t))
  let ifelse e t f = (dummy_s, IfElse (e, t, f))
  let while_ e s = (dummy_s, While (e, s))
  let proccall f args = (dummy_s, ProcCall ((raw_id f), args))
  let methodcallstmt t i args = (dummy_s, MethodCallStmt (t, i, args))
  let break () = (dummy_s, Break)

  let int i = (dummy_e, Int i)
  let bool b = (dummy_e, Bool b)
  let string s = (dummy_e, String s)
  let char c = (dummy_e, Char c)
  let arr es = (dummy_e, Array es)
  let id x = (dummy_e, Id (raw_id x))
  let index a i = (dummy_e, Index (a, i))
  let length e = (dummy_e, Length e)
  let funccall f args = (dummy_e, FuncCall ((raw_id f), args))
  let fieldaccess this i = (dummy_e, FieldAccess (this, i))
  let methodcall this i args = (dummy_e, MethodCall (this, i, args))
  let null () = (dummy_e, Null)

  let tint = (dummy_t, TInt)
  let tbool = (dummy_t, TBool)
  let tarray t e = (dummy_t, TArray (t, e))

  let ( -   ) a b = (dummy_e, BinOp (a, MINUS,    b ))
  let ( *   ) a b = (dummy_e, BinOp (a, STAR,     b ))
  let ( *>> ) a b = (dummy_e, BinOp (a, HIGHMULT, b ))
  let ( /   ) a b = (dummy_e, BinOp (a, DIV,      b ))
  let ( %   ) a b = (dummy_e, BinOp (a, MOD,      b ))
  let ( +   ) a b = (dummy_e, BinOp (a, PLUS,     b ))
  let ( <   ) a b = (dummy_e, BinOp (a, LT,       b ))
  let ( <=  ) a b = (dummy_e, BinOp (a, LTE,      b ))
  let ( >=  ) a b = (dummy_e, BinOp (a, GTE,      b ))
  let ( >   ) a b = (dummy_e, BinOp (a, GT,       b ))
  let ( ==  ) a b = (dummy_e, BinOp (a, EQEQ,     b ))
  let ( !=  ) a b = (dummy_e, BinOp (a, NEQ,      b ))
  let ( &   ) a b = (dummy_e, BinOp (a, AMP,      b ))
  let ( ||  ) a b = (dummy_e, BinOp (a, BAR,      b ))
  let ( ~~  ) a   = (dummy_e, UnOp (UMINUS, a))
  let ( !   ) a   = (dummy_e, UnOp (BANG,   a))
  let (:=) = declasgn
  let (<--) = asgn

  let zero  = int 0L
  let one   = int 1L
  let two   = int 2L
  let three = int 3L
  let four  = int 4L
  let five  = int 5L

  let tru = bool true
  let fls = bool false
end
