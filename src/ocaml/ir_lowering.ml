open Core.Std
open Async.Std
open Ir

let rec lower_exp (e: expr) : (stmt list * expr) =
	match e with
	| BinOp (e1, binop, e2) ->
	| Call (e', es, i) ->
		
	| ESeq (s, e') ->
		let s1 = lower_stmt s in
		let (s2, e2) = lower_exp e' in
		(s1 @ s2, e2)
	| Mem (e', t) ->
		let (s', e') = lower_exp e' in
		(s', Mem (e', t))
	| Name _
	| Temp _ 
	| Const of _ -> ([], e)

and lower_stmt (s: stmt) : stmt list =
	match s with
	| CJump (e, l1, l2) ->
		let (s', e') = lower_exp e in
		s' @ [CJump (e', l1, l2)]
	| Jump e ->
		let (s', e') = lower_exp e in
		s' @ [Jump e']
	| Exp e -> fst (lower_exp e)
	| Move (e1, e2) -> failwith "TODO"
	| Seq ss ->
		List.fold_left ~f:(fun acc s' -> (lower_stmt s') @ acc) ~init:[] ss 
		|> List.rev
	| Label _
	| Return ->	s
