open Core.Std
open Async.Std
open Typecheck

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

and stmt =
  | CJump of expr * string * string
  | CJumpOne of expr * string
  | Jump of expr
  | Exp of expr
  | Label of string
  | Move of expr * expr
  | Seq of stmt list
  | Return

and func_decl = string * stmt * (Expr.t * Expr.t)

and comp_unit = string * func_decl String.Map.t

(* abbreviations *)
module Abbreviations = struct
  let call f args = Call (f, args)
  let const n = Const n
  let eseq s e = ESeq (s, e)
  let mem e = Mem (e, NORMAL)
  let name s = Name s
  let temp s = Temp s

  let cjump b t f = CJump (b, t, f)
  let cjumpone e l = CJumpOne (e, l)
  let jump l = Jump l
  let exp e = Exp e
  let label s = Label s
  let move x e = Move (x, e)
  let seq ss = Seq ss
  let return = Return

  let add_     lhs rhs = BinOp (lhs, ADD,     rhs)
  let sub_     lhs rhs = BinOp (lhs, SUB,     rhs)
  let mul_     lhs rhs = BinOp (lhs, MUL,     rhs)
  let hmul_    lhs rhs = BinOp (lhs, HMUL,    rhs)
  let div_     lhs rhs = BinOp (lhs, DIV,     rhs)
  let mod_     lhs rhs = BinOp (lhs, MOD,     rhs)
  let and_     lhs rhs = BinOp (lhs, AND,     rhs)
  let or_      lhs rhs = BinOp (lhs, OR,      rhs)
  let xor_     lhs rhs = BinOp (lhs, XOR,     rhs)
  let lshift_  lhs rhs = BinOp (lhs, LSHIFT,  rhs)
  let rshift_  lhs rhs = BinOp (lhs, RSHIFT,  rhs)
  let arshift_ lhs rhs = BinOp (lhs, ARSHIFT, rhs)
  let eq_      lhs rhs = BinOp (lhs, EQ,      rhs)
  let neq_     lhs rhs = BinOp (lhs, NEQ,     rhs)
  let lt_      lhs rhs = BinOp (lhs, LT,      rhs)
  let gt_      lhs rhs = BinOp (lhs, GT,      rhs)
  let leq_     lhs rhs = BinOp (lhs, LEQ,     rhs)
  let geq_     lhs rhs = BinOp (lhs, GEQ,     rhs)

  let zero  = const 0L
  let one   = const 1L
  let two   = const 2L
  let three = const 3L
  let four  = const 4L
  let five  = const 5L
  let six   = const 6L
  let seven = const 7L
  let eight = const 8L
  let nine  = const 9L
  let ten   = const 10L
end

module Infix = struct
  let ( +   ) = Abbreviations.add_
  let ( -   ) = Abbreviations.sub_
  let ( *   ) = Abbreviations.mul_
  let ( *>> ) = Abbreviations.hmul_
  let ( /   ) = Abbreviations.div_
  let ( %   ) = Abbreviations.mod_
  let ( &   ) = Abbreviations.and_
  let ( ||  ) = Abbreviations.or_
  let ( ^   ) = Abbreviations.xor_
  let ( <<  ) = Abbreviations.lshift_
  let ( >>  ) = Abbreviations.rshift_
  let ( >>> ) = Abbreviations.arshift_
  let ( ==  ) = Abbreviations.eq_
  let ( !=  ) = Abbreviations.neq_
  let ( <   ) = Abbreviations.lt_
  let ( >   ) = Abbreviations.gt_
  let ( <=  ) = Abbreviations.leq_
  let ( >=  ) = Abbreviations.geq_
end

(* pretty printing *)
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
  | Call (f, args) -> sprintf "%s(%s)" (soe f) (Util.commas (List.map ~f:soe args))
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
