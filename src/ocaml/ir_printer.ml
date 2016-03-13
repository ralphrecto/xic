open Core.Std
open Async.Std
open Ir
open Printf

(* SExp printer matching the format of the released IR parser *)

let of_atoms (sl: string list) : string =
  "(" ^ (String.concat ~sep:" " sl) ^ ")"

(* no paren *)
let of_atoms_np : string list -> string =
  String.concat ~sep:" "

let string_of_binop_code = function
  | ADD -> "ADD"
  | SUB -> "SUB"
  | MUL -> "MUL"
  | HMUL -> "HMUL"
  | DIV -> "DIV"
  | MOD -> "MOD"
  | AND -> "AND"
  | OR -> "OR"
  | XOR -> "XOR"
  | LSHIFT -> "LSHIFT"
  | RSHIFT -> "RSHIFT"
  | ARSHIFT -> "ARSHIFT"
  | EQ -> "EQ"
  | NEQ -> "NEQ"
  | LT -> "LT"
  | GT -> "GT"
  | LEQ -> "LEQ"
  | GEQ -> "GEQ"

let rec sexp_of_expr = function
  | BinOp (e1, opcode, e2) -> begin
      of_atoms [string_of_binop_code opcode;
                sexp_of_expr e1;
                sexp_of_expr e2]
    end
  | Call (target, args) -> begin
      of_atoms ["CALL";
                sexp_of_expr target;
                List.map ~f:sexp_of_expr args |> of_atoms_np]
    end
  | Const i -> of_atoms ["CONST"; Int64.to_string i]
  | ESeq (s, e) -> of_atoms ["ESEQ"; sexp_of_stmt s; sexp_of_expr e]
  | Mem (e, _) -> of_atoms ["MEM"; sexp_of_expr e]
  | Name t -> of_atoms ["NAME"; t]
  | Temp t -> of_atoms ["TEMP"; t]

and sexp_of_stmt = function
  | CJump (pred, label1, label2) ->
    of_atoms ["CJUMP"; sexp_of_expr pred; label1; label2]
  | CJumpOne (pred, label) ->
    of_atoms ["CJUMP"; sexp_of_expr pred; label]
  | Jump label -> of_atoms ["JUMP"; sexp_of_expr label]
  | Exp e -> of_atoms ["EXP"; sexp_of_expr e]
  | Label s -> of_atoms ["LABEL"; s]
  | Move ((Temp _ | Mem _) as dest, e) ->
    of_atoms ["MOVE"; sexp_of_expr dest; sexp_of_expr e]
  | Move (_, _) -> failwith "Malformed Move IR node"
  | Seq (st_list) -> begin
      of_atoms [
        "SEQ";
        List.map ~f:sexp_of_stmt st_list |> of_atoms_np]
    end
  | Return -> of_atoms ["RETURN"]

and sexp_of_func_decl (name, block) =
  of_atoms ["FUNC"; name; sexp_of_stmt block]

and sexp_of_comp_unit ((name, func_decls): comp_unit) =
  let fs = func_decls |> String.Map.data |> List.map ~f:sexp_of_func_decl |> of_atoms_np in
  of_atoms [ "COMPUNIT"; name; fs]
