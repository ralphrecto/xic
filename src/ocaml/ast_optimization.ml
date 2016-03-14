module Long = Int64
open Core.Std
open Async.Std
open Ast
open Typecheck

let rec ast_constant_folding (_, e) =
	let open Long in
	let open Big_int in
	match e with
	| BinOp (Int 0L, ADD, Int i)
	| BinOp (Int i, (ADD|SUB), Int 0L)
	| BinOp (Int 1L, MUL, Int i)
	| BinOp (Int i, (MUL|DIV), Int 1L) -> Int i
	| BinOp (Int 0L, SUB, Int i) -> Int (neg i)
	| BinOp (Int i1, MINUS, Int i2) -> Int (sub i1 i2)
	| BinOp (Int i1, STAR, Int i2) -> Int (mul i1 i2) 
	| BinOp (Int i1, HIGHMULT, Int i2) ->
		let i1' = big_int_of_int64 i1 in
		let i2' = big_int_of_int64 i2 in
		let mult = mult_big_int i1' i2' in
		let max_long = big_int_of_int64 max_int in
		let divided = div_big_int mult max_long in
		let result = int64_of_big_int divided in
		Int result
	| BinOp (Int i1, DIV, Int i2) -> Int (div i1 i2)
	| BinOp (Int i1, MOD, Int i2) -> Int (rem i1 i2) 
	| BinOp (Int i1, PLUS, Int i2) -> Int (add i1 i2) 
	| BinOp (Int i1, LT, Int i2) -> Bool ((compare i1 i2) < 0) 
	| BinOp (Int i1, LTE, Int i2) -> Bool ((compare i1 i2) <= 0) 
	| BinOp (Int i1, GTE, Int i2) -> Bool ((compare i1 i2) >= 0) 
	| BinOp (Int i1, GT, Int i2) -> Bool ((compare i1 i2) > 0) 
	| BinOp (Int i1, EQEQ, Int i2) -> Bool ((compare i1 i2) = 0) 
	| BinOp (Int i1, NEQ, Int i2) -> Bool ((compare i1 i2) <> 0
	| BinOp (Bool b1, EQEQ, Bool b2) -> Bool (b1 = b2) 
	| BinOp (Bool b1, NEQ, Bool b2) -> Bool (b1 <> b2) 
	| BinOp (Bool false, AMP, _) 
	| BinOp (_, AMP, Bool false) -> Bool false
	| BinOp (Bool true, OR, _)
	| BinOP (_, OR, Bool true) -> Bool true
	| BinOp (Bool b1, AMP, Bool b2) -> Bool (b1 && b2) 
	| BinOp (Bool b1, BAR, Bool b2) -> Bool (b1 || b2) 
	| UnOp (UMINUS, Int i) -> Int (neg i)
	| UnOp (BANG, Bool b) -> Bool (not b)
	| BinOp (e1, op, e2) ->
		begin
			match (ast_constant_folding e1), (ast_constant_folding e2) with
			| ((Int _ | Bool _ ) as c1), ((Int _ | Bool _ ) as c2) -> ast_constant_folding (BinOp (c1, op, c2))
			| e1', e2' -> BinOp (e1', op, e2')
		end
	| UnOp (op, e') ->
		begin
			match ast_constant_folding e' with
			| (Int _ | Bool _) as c -> ast_constant_folding (UnOp (op, c))
			| e'' -> UnOp (op, e'')
		end
	| Array elist -> Array (List.map ~f: ast_constant_folding elist)	
	| Index (e1, e2) -> Index (ast_constant_folding e1, ast_constant_folding e2)
	| Length e' -> Length (ast_constant_folding e')
	| FuncCall (id, elist) -> FuncCall (id, List.map ~f: ast_constant_folding elist)
	| Int _
	| Bool _
	| String _
	| Char _
	| Id _ -> e
