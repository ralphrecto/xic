module Long = Int64
open Core.Std
open Async.Std
open Ast.S
open Typecheck

let ast_constant_folding (FullProg (name, (prog_type, prog), interfaces): Typecheck.full_prog) =
  let rec fold_expr ((t, e): Typecheck.expr) =
    let open Long in
    let open Big_int in
    match e with
    (* don't fold failing expressions *)
    | BinOp ((_, Int _), DIV, (_, Int 0L))
    | BinOp ((_, Int _), MOD, (_, Int 0L)) -> (t, e)
    | BinOp ((_, Int i1), MINUS, (_, Int i2)) -> (t, Int (sub i1 i2))
    | BinOp ((_, Int i1), STAR, (_, Int i2)) -> (t, Int (mul i1 i2))
    | BinOp ((_, Int i1), HIGHMULT, (_, Int i2)) -> 
      let i1' = big_int_of_int64 i1 in
      let i2' = big_int_of_int64 i2 in
      let mult = mult_big_int i1' i2' in
      let shifted = shift_right_big_int mult 64 in
      let result = int64_of_big_int shifted in
      (t, Int result)
    | BinOp ((_, Int i1), DIV, (_, Int i2)) -> (t, Int (div i1 i2))
    | BinOp ((_, Int i1), MOD, (_, Int i2)) -> (t, Int (rem i1 i2))
    | BinOp ((_, Int i1), PLUS, (_, Int i2)) -> (t, Int (add i1 i2))
    | BinOp ((_, Int i1), LT, (_, Int i2)) -> (t, Bool ((compare i1 i2) < 0))
    | BinOp ((_, Int i1), LTE, (_, Int i2)) -> (t, Bool ((compare i1 i2) <= 0))
    | BinOp ((_, Int i1), GTE, (_, Int i2)) -> (t, Bool ((compare i1 i2) >= 0))
    | BinOp ((_, Int i1), GT, (_, Int i2)) -> (t, Bool ((compare i1 i2) > 0))
    | BinOp ((_, Int i1), EQEQ, (_, Int i2)) -> (t, Bool ((compare i1 i2) = 0))
    | BinOp ((_, Int i1), NEQ, (_, Int i2)) -> (t, Bool ((compare i1 i2) <> 0))
    | BinOp ((_, Bool b1), EQEQ, (_, Bool b2)) -> (t, Bool (b1 = b2))
    | BinOp ((_, Bool b1), NEQ, (_, Bool b2)) -> (t, Bool (b1 <> b2))
    | BinOp ((_, Bool b1), AMP, (_, Bool b2)) -> (t, Bool (b1 && b2))
    | BinOp ((_, Bool b1), BAR, (_, Bool b2)) -> (t, Bool (b1 || b2))
    | BinOp ((_, Array l1), PLUS, (_, Array l2)) -> (t, Array (l1 @ l2))
    | UnOp (UMINUS, (_, Int i)) -> (t, Int (neg i))
    | UnOp (BANG, (_, Bool b))  -> (t, Bool (not b))
    | BinOp (e1, op, e2) ->
      begin
        match (fold_expr e1), (fold_expr e2) with
        | (((_, Int _) | (_, Bool _ )) as c1), (((_, Int _ ) | (_, Bool _ )) as c2) ->
          fold_expr ((t, BinOp (c1, op, c2)))
        | e1', e2' -> (t, BinOp (e1', op, e2'))
      end
    | UnOp (op, e') ->
      begin
        match fold_expr e' with
        | ((_, Int _) | (_, Bool _)) as c -> fold_expr ((t, UnOp (op, c)))
        | e'' -> (t, UnOp (op, e''))
      end
    | Array elist -> (t, Array (List.map ~f: fold_expr elist))
    | Index (e1, e2) -> (t, Index (fold_expr e1, fold_expr e2))
    | Length e' -> (t, Length (fold_expr e'))
    | FuncCall (id, elist) -> (t, FuncCall (id, List.map ~f: fold_expr elist))
    | Int _
    | Bool _
    | String _
    | Char _
    | Id _ -> (t, e) in
  let rec fold_stmt ((ts, s): Typecheck.stmt) =
    match s with
    | DeclAsgn (varlist, e) -> (ts, DeclAsgn (varlist, fold_expr e))
    | Asgn (e1, e2) -> (ts, Asgn (fold_expr e1, fold_expr e2))
    | Block stmtlist -> (ts, Block (List.map ~f:fold_stmt stmtlist))
    | Return exprlist -> (ts, Return (List.map ~f:fold_expr exprlist))
    | If (pred, branch) ->
			begin
				match (fold_expr pred) with
				| (_, Bool true) -> fold_stmt branch
				| (_, Bool false) -> (ts, Block [])
				| b -> (ts, If (b, fold_stmt branch))
			end
    | IfElse (pred, tbr, fbr) ->
			begin
				match (fold_expr pred) with
				| (_, Bool true) -> fold_stmt tbr
				| (_, Bool false) -> fold_stmt fbr
				| b -> (ts, IfElse (b, fold_stmt tbr, fold_stmt fbr))
			end
    | While (pred, stmt) ->
			begin
				match (fold_expr pred) with
				| (_, Bool false) -> (ts, Block [])
				| b -> (ts, While (b, fold_stmt stmt))
			end
    | ProcCall (id, args) -> (ts, ProcCall (id, List.map ~f:fold_expr args))
    | _ -> (ts, s) in
  let fold_callable ((tc, c): Typecheck.callable) =
    match c with
    | Func (id, avars, typs, block) ->
      (tc, Func (id, avars, typs, fold_stmt block))
    | Proc (id, avars, block) ->
      (tc, Proc (id, avars, fold_stmt block)) in
  let (Prog (uses, callables)) = prog in
  let prog' = (prog_type, Prog (uses, List.map ~f:fold_callable callables)) in
  FullProg (name, prog', interfaces)
