open Core.Std
open Async.Std

(* (row, col) *)
type pos = int * int [@@deriving sexp]

module T = struct
  type p = pos [@@deriving sexp]
  type u = pos [@@deriving sexp]
  type c = pos [@@deriving sexp]
  type i = pos [@@deriving sexp]
  type a = pos [@@deriving sexp]
  type v = pos [@@deriving sexp]
  type s = pos [@@deriving sexp]
  type e = pos [@@deriving sexp]
  type t = pos [@@deriving sexp]
end

include Ast.Make(T)

open Ast.S

let dummy = (-1, -1)
let raw_id i = (dummy, i)

let fullprog prog ilist = FullProg (prog, ilist)
let interface dlist = (dummy, Interface dlist)
let funcdecl f args typs = (dummy, FuncDecl ((raw_id f), args, typs))
let procdecl f args = (dummy, ProcDecl ((raw_id f), args))
let prog uses calls = (dummy, Prog (uses, calls))
let use x = (dummy, Use (raw_id x))
let func f args typs s = (dummy, Func ((raw_id f), args, typs, s))
let proc f args s = (dummy, Proc ((raw_id f), args, s))
let aid x t = (dummy, AId ((raw_id x), t))
let aunderscore t = (dummy, AUnderscore t)
let avar a = (dummy, AVar a)
let underscore = (dummy, Underscore)
let decl vs = (dummy, Decl vs)
let declasgn vs es = (dummy, DeclAsgn (vs, es))
let asgn lhs rhs = (dummy, Asgn (lhs, rhs))
let block ss = (dummy, Block ss)
let return es = (dummy, Return es)
let if_ e t = (dummy, If (e, t))
let ifelse e t f = (dummy, IfElse (e, t, f))
let while_ e s = (dummy, While (e, s))
let proccall f args = (dummy, ProcCall ((raw_id f), args))


let int i = (dummy, Int i)
let bool b = (dummy, Bool b)
let string s = (dummy, String s)
let char c = (dummy, Char c)
let arr es = (dummy, Array es)
let id x = (dummy, Id (raw_id x))
let index a i = (dummy, Index (a, i))
let length e = (dummy, Length e)
let funccall f args = (dummy, FuncCall ((raw_id f), args))

let ( -   ) a b = (dummy, BinOp (a, MINUS,    b ))
let ( *   ) a b = (dummy, BinOp (a, STAR,     b ))
let ( *>> ) a b = (dummy, BinOp (a, HIGHMULT, b ))
let ( /   ) a b = (dummy, BinOp (a, DIV,      b ))
let ( %   ) a b = (dummy, BinOp (a, MOD,      b ))
let ( +   ) a b = (dummy, BinOp (a, PLUS,     b ))
let ( <   ) a b = (dummy, BinOp (a, LT,       b ))
let ( <=  ) a b = (dummy, BinOp (a, LTE,      b ))
let ( >=  ) a b = (dummy, BinOp (a, GTE,      b ))
let ( >   ) a b = (dummy, BinOp (a, GT,       b ))
let ( ==  ) a b = (dummy, BinOp (a, EQEQ,     b ))
let ( !=  ) a b = (dummy, BinOp (a, NEQ,      b ))
let ( &   ) a b = (dummy, BinOp (a, AMP,      b ))
let ( ||  ) a b = (dummy, BinOp (a, BAR,      b ))
let ( ~~  ) a   = (dummy, UnOp (UMINUS, a))
let ( !   ) a   = (dummy, UnOp (BANG,   a))

let tint = (dummy, TInt)
let tbool = (dummy, TBool)
let tarray t e = (dummy, TArray (t, e))
