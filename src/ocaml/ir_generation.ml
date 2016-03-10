open Core.Std
open Async.Std
open Ir

(* label * adjacent nodes * mark *)
type node = Node of string * string list * false
type graph = node list

let num_temp = ref 0 in
let fresh_temp () =
	let str = "t" ^ (string_of_int (!num_temp)) in
	num_temp := !num_temp + 1;
	str

let num_label = ref 0 in
let fresh_label () =
	let str = "l" ^ (string_of_int (!num_label)) in
	num_label := !num_label + 1;
	str 

let rec lower_exp (e: expr) : (stmt list * expr) =
	match e with
	| BinOp (e1, binop, e2) ->
		let (s1, e1') = lower_exp e1 in
		let (s2, e2') = lower_exp e2 in
		let temp = fresh_temp () in
		let temp_move = Move (Temp temp, e1') in
		(s1 @ temp_move @ s2, BinOp(e1', e2'))
	| Call (e', es, i) ->
		let call_fold (acc, temps) elm =
			let (s1, e1) = lower_exp elm in
			let temp = fresh_temp () in
			let temp_move = Move (Temp temp, e1) in
			(temp_move::s1::acc, (Temp temp)::temps)
		in
		let (arg_stmts, arg_temps) = List.fold_left ~f: call_fold ~init: ([], []) es in
		let (name_s, name_e) = lower_exp e' in
		let temp_name = fresh_temp () in
		let temp_move_name = Move (Temp temp_name, name_e) in
		let fn_stmts = name_s :: temp_move_name :: (List.rev arg_stmts) in
		let fn_args = List.rev arg_temps in
		let temp_fn = fresh_temp () in
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
		let temp = fresh_temp () in
		let temp_move = Move (Temp temp, e'') in
		s'' @ [temp_move] @ dest_s @ [Move(dest', Temp temp)]
	| Seq ss ->
		List.fold_left ~f:(fun acc s' -> (lower_stmt s') @ acc) ~init:[] ss 
		|> List.rev
	| Label _
	| Return ->	s

let block_reorder (stmts: stmt list) : block list =
	(* order of stmts in blocks are reversed
	   to make looking at conditionals easier *)
	let gen_block (blocks, acc, label) elm =
		match elm, label, acc with
		| Label s, Some l, _ -> 
			(Block of (l, acc)::blocks, [], Some s)
		| Label s, None, [] -> 
			(blocks, [], Some s)	
		| Label s, None, _ ->
			let fresh_label = fresh_label () in
			(Block (fresh_label, acc)::blocks, [], Some s)	
		| CJump _, Some l, _ -> 
			(Block of (l, elm::acc)::blocks, [], None)
		| CJump _, None, _-> 
			let fresh_label = fresh_label () in
			(Block (fresh_label, elm::acc)::blocks, [], None)
		| Jump _, Some l, _ -> 
			(Block of (l, elm::acc)::blocks, [], None)
		| Jump _, None, _ ->
			let fresh_label = fresh_label () in
			(Block of (fresh_label, elm::acc)::blocks, [], None)
		| _ -> (blocks, elm::acc, label)
	in 
	let (b, a, l) = List.fold_left ~f: gen_block ~init: ([], [], None) stmts in
	let blocks = 
		match l with
		| None ->
			let fresh_label = fresh_label () in
			(Block (fresh_label, List.rev a)) :: b
		| Some l' -> (Block (l', List.rev a)) :: b
	in
	let check_dup Block (l1, _) Block (l1, _) = 
		compare l1 l2
	in
	(* sanity check to make sure there aren't duplicate labels *)
	assert (not (List.contains_dup ~compare: check_dup blocks));
	let find_remove l f =
		let helper (elm_found, acc) e =
			if f e then (Some e, acc)
			else (elm_found, e::acc)
		in
		List.fold_left ~f: helper ~init: (None, []) l
	in
	let create_graph blocks graph =
		match blocks with
		| Block (l1,s1)::Block (l2,s2)::tl ->
				begin
					match s1 with
					| CJump (_, tru, fls) -> create_graph (Block (l2, s2)::tl) (Node (l1, [tru; fls], false)::graph)
					| Jump (Name l') -> create_graph (Block (l2, s2)::tl) (Node (l1, [l'], false)::graph)
					| Jump _ -> failwith "error -- invalid jump"
					| _ -> create_graph (Block (l2, s2)::tl) (Node (l1, [l2], false)::graph)
				end
		| Block(l,s)::[]-> 
				begin
					match s with
					| CJump (_, tru, fls) -> Node (l, [tru;fls], false)::graph
					| Jump (Name l') -> Node (l, [l'], false)::graph
					| Jump _ -> failwith "error -- invalid jump"
					| _ -> Node (l, [], false)::graph
				end
		| [] ->	graph
	in	
	let graph = create_graph blocks [] in
	blocks
