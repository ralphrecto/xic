open Core.Std
open Async.Std
open Ir

let num = ref 0 in
let fresh () =
	let str = "t" ^ (string_of_int (!num) in
	num := !num + 1;
	str

let rec lower_exp (e: expr) : (stmt list * expr) =
	match e with
	| BinOp (e1, binop, e2) ->
		let (s1, e1') = lower_exp e1 in
		let (s2, e2') = lower_exp e2 in
		let temp = fresh () in
		let temp_move = Move (Temp temp, e1') in
		(s1 @ temp_move @ s2, BinOp(e1', e2'))
	| Call (e', es, i) ->
		let call_fold (acc, temps) elm =
			let (s1, e1) = lower_exp elm in
			let temp = fresh () in
			let temp_move = Move (Temp temp, e1) in
			(temp_move::s1::acc, (Temp temp)::temps)
		in
		let (arg_stmts, arg_temps) = List.fold_left ~f: call_fold ~init: ([], []) es in
		let (name_s, name_e) = lower_exp e' in
		let temp_name = fresh () in
		let temp_move_name = Move (Temp temp_name, name_e) in
		let fn_stmts = name_s :: temp_move_name :: (List.rev arg_stmts) in
		let fn_args = List.rev arg_temps in
		let temp_fn = fresh () in
		let temp_move_fn = Move (Temp temp_fn, Call(Temp temp_name, fn_args, i)) in
		(fn_stmts @ [temp_move_fn], Temp temp_fn)
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
	| Move (dest, e') ->
		let (dest_s, dest') = lower_exp dest in
		let (s'', e'') = lower_exp e' in	
		let temp = fresh () in
		let temp_move = Move (Temp temp, e'') in
		s'' @ [temp_move] @ dest_s @ [Move(dest', Temp temp)]
	| Seq ss ->
		List.fold_left ~f:(fun acc s' -> (lower_stmt s') @ acc) ~init:[] ss 
		|> List.rev
	| Label _
	| Return ->	s
