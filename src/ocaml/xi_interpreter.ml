module Long = Int64
open Core.Std
open Ast
open Ast.S
open Typecheck

type id = string
type result = 
		Int of int64
	|	Bool of bool
	| String of string
	| Char of char
	| Array of result list
	| Tuple of result list
type store = Value of result | Function of (id option) list * stmt 
type context = (store option) String.Map.t

let id_of_avar ((_, av): avar) = 
	match av with
	| AId ((_,id), _) -> Some id
	| _ -> None

let id_of_var ((_, v): var) =
	match v with
	| AVar av -> id_of_avar av
	| Underscore -> None

let bind_ids store ids =
	let helper s i =
		match i with
		| Some i' -> String.Map.add s ~key:i' ~data: None
		| None -> s 
	in
	List.fold_left ~f:helper ~init:store ids

let bind_ids_vals store ids_vals =
	let helper s (i,v) =
		match i with
		| Some i' -> String.Map.add s ~key:i' ~data: (Some (Value v))
		| None -> s
	in
	List.fold_left ~f:helper ~init:store ids_vals	

let ids_of_vars vlist = List.map ~f: id_of_var vlist

let ids_of_avars avlist = List.map ~f: id_of_avar avlist

(* conv, io interface file functions *)
(* NOTE: eof() function in io not included idk how to do it *)

let unparseInt (n: int64) : string =
	let open Long in	
	to_string n

let parseInt (s: string) : int64 =
	let open Long in
	of_string s

let print =
	print_string

let println =
	print_endline

let readln = 
	read_line 

let getchar () : char =
	let i = read_int () in
	char_of_int i	

(* interpreter *)

let rec eval_full_prog (store: context) (FullProg (prog, _): Typecheck.full_prog) : result option = 
	let updated_store = eval_prog store prog in
	match String.Map.find store "main" with
	| Some (Some (Function (ids, stmt))) -> snd (eval_stmt store stmt)
	| Some _ -> failwith "main is a variable? lol"
	| None -> failwith "no main function lol" 

and eval_prog (store: context) ((_, Prog (_, calls)): Typecheck.prog) : context = 
	List.fold_left ~f: (fun store' call -> eval_callable store' call) ~init: store calls

and eval_callable (store: context) ((_, c): Typecheck.callable) : context = 
	match c with
	| Func ((_, id), avars, _, stmt)
	| Proc ((_, id), avars, stmt) ->
		let args = ids_of_avars avars in
		String.Map.add store ~key: id ~data: (Some (Function (args, stmt)))

and eval_stmts store ss = 
	List.fold_left ~f:(fun (store', _) s -> eval_stmt store' s) ~init:(store, None) ss

and eval_stmt (store: context) ((_,s): Typecheck.stmt) : context * result option =
	match s with
	| Decl vlist ->
		let ids = ids_of_vars vlist in
		let store' = bind_ids store ids in
		(store', None)
	| DeclAsgn (vlist, e) ->
		begin
			match vlist, (eval_expr store e) with	
			| _::_, Tuple elist ->
				let ids = ids_of_vars vlist in
				begin
					match List.zip ids elist with
					| Some l -> 
						let store' = bind_ids_vals store l in
						(store', None)
					| None -> failwith "shouldn't happen -- declasgn var list and e list do not match"
				end
			| [v], e' ->
				let id = id_of_var v in
				let store' = bind_ids_vals store [(id, e')] in
				(store', None)
			| _ -> failwith "shouldn't happen -- declasgn no var declared" 
		end
	| Asgn (e1, e2) ->
		begin
			match e1, (eval_expr store e2) with	
			| (_, Id (_,i)), e2' -> 
				let store' = bind_ids_vals store [(Some i, e2')] in
				(store', None)
			| _ -> failwith "shouldn't happen - asgn left is not a var"
		end
	| Block slist -> eval_stmts store slist
	| Return elist ->
		begin
			match elist with
			|_::_ ->
				let res = List.map ~f:(eval_expr store) elist in
				(store, Some (Tuple res))
			|[e] -> 
				let e' = eval_expr store e in
				(store, Some e')
			|[] -> (store, None) 
		end
	| If (e1, slist) ->
		begin
			match eval_expr store e1 with
			| Bool true -> eval_stmt store slist
			| Bool false -> (store, None)
			| _ -> failwith "shouldn't happen -- if"
		end
	| IfElse (e1, slist1, slist2) ->
		begin
			match eval_expr store e1 with
			| Bool true -> eval_stmt store slist1
			| Bool false -> eval_stmt store slist2
			| _ -> failwith "shouldn't happen -- ifelse"
		end
	| While (e1, slist) ->
		let rec helper b (store', ret) =
			match ret, (eval_expr store' b) with
			| Some _, _ -> (store', ret)
			| None, Bool true ->
				let updated = eval_stmt store' slist in
				helper b updated
			| None, Bool false -> (store', ret)
			| _ -> failwith "shouldn't happen -- while not a boolean"
		in
		helper e1 (store, None)
	| ProcCall ((_,id), elist) ->
		begin
			match (String.Map.find store id) with
			| Some (Some (Function (params, body))) ->
				let vals = List.map ~f:(eval_expr store) elist in
				begin
					match (List.zip params vals) with
					| Some l -> 
						let store' = bind_ids_vals store l in
						eval_stmt store' body
					| None -> failwith "shouldn't happen -- proccall params and args don't match"
				end
			| Some _ -> failwith "shouldn't happen -- proccall not a function"
			| None -> 
				let vals = List.map ~f:(eval_expr store) elist in
				match id, vals with
				| "print", [String s] -> print s; (store, None)
				| "println", [String s] -> println s; (store, None)
				| _ -> failwith "shouldn't happen -- proccall function not delcared" 
		end

and eval_expr (store: context) ((_,e): Typecheck.expr) : result = 
	let open Long in
	match e with
	| Int i -> Int i
	| Bool b -> Bool b
	| String s -> String s
	| Char c -> Char c
	| Array l -> Array (List.map ~f:(eval_expr store) l) 
	| Id (_, i) ->
		begin
			match String.Map.find store i with
			| Some (Some (Value v)) -> v
			| Some _ -> failwith "shouldn't happen -- id"
			| None -> failwith "variable has been declared but not assigned to a value"
		end 
	| BinOp (e1, op, e2) -> eval_binop (eval_expr store e1) op (eval_expr store e2)
	| UnOp (op, e1) -> eval_unop op (eval_expr store e1)
	| Index (e1, e2) -> 
		begin
			match (eval_expr store e1), (eval_expr store e2) with 
			| Array l, Int i ->
				let i' = to_int i in
				begin
					match List.nth l i' with
					| Some x -> x
					| None -> failwith "invalid index"
				end
			| _ -> failwith "shouldn't happen -- index"
		end
	| Length e1 ->
		begin
			match eval_expr store e1 with
			| Array l -> 
				let len = List.length l	in
				Int (of_int len)
			| _ -> failwith "shouldn't happen -- length"
		end
	| FuncCall ((_, id), elist) -> 
		match String.Map.find store id with
		| Some (Some (Function (params, body))) ->
			let vals = List.map ~f:(eval_expr store) elist in
			begin
				match List.zip params vals with
				| Some l -> 
					let store' = bind_ids_vals store l in
					begin
						match eval_stmt store' body with
						| (_, Some ret) -> ret
						| _ -> failwith "function has no return"
					end
				| None -> failwith "shouldn't happen -- funccall params and args don't match"
			end
		| Some _ -> failwith "shouldn't happen -- funccall not a function"
		| None ->
			let vals = List.map ~f:(eval_expr store) elist in 
			match id, vals with
			| "unparseInt", [Int i] -> String (unparseInt i) 
			| "parseInt", [String s] -> Int (parseInt s)
			| "readln", [] -> String (readln ())
			| "getchar", [] -> Char (getchar ())
			| _ -> failwith "shouldn't happen -- funccall function not delcared"
	
and eval_binop e1 op e2 = 
	let open Long in
	match e1, op, e2 with
	| Int i1, MINUS, Int i2 -> Int (sub i1 i2)
	| Int i1, STAR, Int i2 -> Int (mul i1 i2)
	| Int i1, HIGHMULT, Int i2 -> failwith "implement later"
	| Int i1, DIV, Int i2 -> Int (div i1 i2)
	| Int i1, MOD, Int i2 -> Int (rem i1 i2)
	| Int i1, PLUS, Int i2 -> Int (add i1 i2) 
	| Int i1, LT, Int i2 -> Bool ((compare i1 i2) < 0)
	| Int i1, LTE, Int i2 -> Bool ((compare i1 i2) <= 0)
	| Int i1, GTE, Int i2 -> Bool ((compare i1 i2) >= 0) 
	| Int i1, GT, Int i2 -> Bool ((compare i1 i2) > 0)  
	| Int i1, EQEQ, Int i2 -> Bool ((compare i1 i2) = 0) 
	| Int i1, NEQ, Int i2 -> Bool ((compare i1 i2) <> 0) 
	| Bool b1, AMP, Bool b2 -> Bool (b1 && b2)
	| Bool b1, BAR, Bool b2 -> Bool (b1 || b2) 
	| _ -> failwith "shouldn't happen -- binop" 

and eval_unop op e1 =
	let open Long in
	match op, e1 with
	| UMINUS, Int i -> Int (neg i)
	| BANG, Bool b -> Bool (not b) 
	| _ -> failwith "shouldn't happen -- unop"

