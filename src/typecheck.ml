open Core.Std
open Async.Std
open Result

type t = IntT
  | BoolT 
  | UnitT
  | ArrayT of t
  | EmptyArray

type return = UnitR
  | Void
  
type sigma = Var of t
  | Function of t list * t list

type context = sigma String.Map.t

let string_of_binopcode op =
  op |> Ast.sexp_of_binopcode |> Sexp.to_string

let string_of_unopcode op =
  op |> Ast.sexp_of_unopcode |> Sexp.to_string

let rec expr_typecheck (c: context) ((p, expr): Pos.expr)  : ((t Ast.expr) * (Pos * string)) Result.t =
  match expr with
  | Int n -> OK (IntT, expr)
  | Bool b -> OK (BoolT, expr)
  | String s -> OK (ArrayT IntT, expr)
  | Char c -> OK (IntT, expr)
  | Array [] -> OK (EmptyArray, expr)
  | Array (hd::tl) -> begin
    expr_typecheck c hd >>= fun (typ, e) ->
      let tl_res = List.map ~f:(expr_typecheck c) tl in
      if List.for_all (fun (y_typ, _) -> typ = y_typ) tl_res then
        OK (ArrayT typ, expr)
      else
        Error (p, "Array elements have different type.")
  end
  | Id s -> begin
    match String.Map.find c s with
    (* just variables - no function lookup *)
    | Some (Var typ) -> OK (typ, expr)
    | _ -> Error (p, Printf.sprintf "Variable %s unbound" s)
  end
  | BinOp (e1,(MINUS|STAR|HIGHMULT|DIV|MOD) as opcode,e2) -> begin
    expr_typecheck c e1 >>= fun (typ1, e1') ->
    expr_typecheck c e2 >>= fun (typ2, e2') ->
      match typ1, typ2 with
      | IntT, IntT -> OK (IntT, expr)
      | _ ->
        let ops = string_of_binopcode opcode in
        Error (p, Printf.sprintf "Expecting int operands for %s" ops)
  end
  | BinOp (e1, PLUS, e2) -> begin
    expr_typecheck c e1 >>= fun (typ1, e1') ->
    expr_typecheck c e2 >>= fun (typ2, e2') ->
      match typ1, typ2 with
      | IntT, IntT -> OK (IntT, expr)
      | ArrayT t1, Array t2 when t1 = t2 -> OK (ArrayT t1, expr)
      | ArrayT t, EmptyArray
      | EmptyArray, ArrayT t -> OK (ArrayT t, expr)
      | EmptyArray, EmptyArray -> OK (EmptyArray, expr)
      | _ -> Error (p, "Inconsistent operand types for +")
  end
  | UnOp (opcode, e) -> begin
    expr_typecheck c e >>= fun (typ, e') ->  
      match opcode, typ with
      | UMINUS, IntT -> OK (IntT, expr)
      | BANG, BoolT -> OK (BoolT, expr)
      | _ ->
        let emsg = Printf.sprintf "Inconsistent operand type for %s" opcode in
        Error (p, emsg)
  end
  | Index (e1, e2) -> begin
    expr_typecheck c e1 >>= fun (typ1, e1') ->
    expr_typecheck c e2 >>= fun (typ2, e2') ->
      match typ1, typ2 with
      | ArrayT t, IntT -> OK (t, expr)
      | EmptyArray, _ -> Error (p, "Indexing into empty array")
      | _ -> Error (p, "Bad indexing into array")
  end
  | Length e -> begin
    expr_typecheck c e >>= fun (typ, (_, e')) ->
      match typ with
      | Array _ | EmptyArray -> OK (IntT, expr)
      | _ -> Error (p, "Using length() on a non-array")
  end
  | FuncCall ((_, name), args) -> begin
    match String.Map.find c name, args with
    | Function (UnitT, t), [] when t <> Unit -> OK (t, expr)
    | Function (TupleT lst, t), _ :: _ :: tl when t <> Unit ->
        List.zip lst args 
    | Function (t1, t2) ->
  end

let give_types ((p, av): 'a avar) : t =
	let rec typ_to_t (typ: 'a typ) : t =
		match typ with
		| TInt -> IntT
		| TBool -> BoolT
		| TArray (typ', _) -> 
			let t' = typ_to_t typ' in
			ArrayT t'
	in
	match av with	
	| AId (_, typ) -> typ_to_t typ
	| AUnderscore typ -> typ_to_t typ	

let rec toplevel_func_typecheck (c: context) ((p, call): Pos.callable) : (context * (Pos * string)) Result.t =
	match call with
	| Func ((pid, id), (pargs, args), (prets, rets), (pbody, body)) ->
		if String.Map.mem context id then 
			Error(p, "Function already exists")
		else
			let args_t_list = List.map ~f:give_types args in
			let rets_t_list = List.map ~f:give_types rets in
			let c' = String.Map.add context id (Function (args_t_list, rets_t_list)) in
			Ok c'	
	| Proc (id, args, body) ->
		if String.Map.mem context id then
			Error(p, "Procedure already exists")
		else
			let args_t_list = List.map ~f:give_types args in	
			let c' = String.Map.add context id (Function (args_t_list, UnitR)) in
			Ok c'	

let rec stmt_typecheck (c: context) ((p, stmt): Pos.stmt) : ((t Ast.stmt) * (Pos * string)) Result.t =
	failwith "lol"





















