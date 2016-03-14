module Long = Int64
open Core.Std
open Async.Std
open Ast.S
open Typecheck

let rec ast_constant_folding ((t, e): Typecheck.expr) =
	let open Long in
	let open Big_int in
	match e with
	| BinOp ((_, Int 0L), PLUS, (_, Int i))
	| BinOp ((_, Int i), (PLUS|MINUS), (_, Int 0L))
	| BinOp ((_, Int 1L), STAR, (_, Int i))
	| BinOp ((_, Int i), (STAR|DIV), (_, Int 1L)) -> (t, Int i)
	| BinOp ((_, Int 0L), MINUS, (_, Int i)) -> (t, Int (neg i))
	| BinOp ((_, Int i1), MINUS, (_, Int i2)) -> (t, Int (sub i1 i2))
	| BinOp ((_, Int i1), STAR, (_, Int i2)) -> (t, Int (mul i1 i2))
	| BinOp ((_, Int i1), HIGHMULT, (_, Int i2)) ->
		let i1' = big_int_of_int64 i1 in
		let i2' = big_int_of_int64 i2 in
		let mult = mult_big_int i1' i2' in
		let max_long = big_int_of_int64 max_int in
		let divided = div_big_int mult max_long in
		let result = int64_of_big_int divided in
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
	| BinOp ((_, Bool false), AMP, _) 
	| BinOp (_, AMP, (_, Bool false)) -> (t, Bool false)
	| BinOp ((_, Bool true), BAR, _)
	| BinOp (_, BAR, (_, Bool true)) -> (t, Bool true)
	| BinOp ((_, Bool b1), AMP, (_, Bool b2)) -> (t, Bool (b1 && b2))
	| BinOp ((_, Bool b1), BAR, (_, Bool b2)) -> (t, Bool (b1 || b2))
	| UnOp (UMINUS, (_, Int i)) -> (t, Int (neg i))
	| UnOp (BANG, (_, Bool b))  -> (t, Bool (not b))
	| BinOp (e1, op, e2) ->
		begin
			match (ast_constant_folding e1), (ast_constant_folding e2) with
			| (((_, Int _) | (_, Bool _ )) as c1), (((_, Int _ ) | (_, Bool _ )) as c2) -> 
				ast_constant_folding ((t, BinOp (c1, op, c2)))
			| e1', e2' -> (t, BinOp (e1', op, e2'))
		end
	| UnOp (op, e') ->
		begin
			match ast_constant_folding e' with
			| ((_, Int _) | (_, Bool _)) as c -> ast_constant_folding ((t, UnOp (op, c)))
			| e'' -> (t, UnOp (op, e''))
		end
	| Array elist -> (t, Array (List.map ~f: ast_constant_folding elist))
	| Index (e1, e2) -> (t, Index (ast_constant_folding e1, ast_constant_folding e2))
	| Length e' -> (t, Length (ast_constant_folding e'))
	| FuncCall (id, elist) -> (t, FuncCall (id, List.map ~f: ast_constant_folding elist))
	| Int _
	| Bool _
	| String _
	| Char _
	| Id _ -> (t, e)
