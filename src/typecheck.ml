open Core.Std
open Async.Std
open Result
open Ast

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

let dummy () = ()

let string_of_binopcode op =
  op |> Ast.sexp_of_binop_code |> Sexp.to_string

let string_of_unopcode op =
  op |> Ast.sexp_of_unop_code |> Sexp.to_string

let rec expr_typecheck (c : context) ((p, expr) : Pos.expr) =
  match expr with
  | Int _ -> Ok (IntT, expr)
  | Bool _ -> Ok (BoolT, expr)
  | String _ -> Ok (ArrayT IntT, expr)
  | Char _ -> Ok (IntT, expr)
  | Array [] -> Ok (EmptyArray, expr)
  | Array (hd::tl) -> begin
    expr_typecheck c hd >>= fun (typ, _) ->
    List.map ~f:(expr_typecheck c) tl |> Result.all >>= fun typlist ->
      if List.for_all ~f:(fun (el_typ, _) -> el_typ = typ) typlist then
        Ok (ArrayT typ, expr)
      else Error (p, "Array elements have different types")
  end
  | Id s -> begin
    match String.Map.find c s with
    (* just variables - no function lookup *)
    | Some (Var typ) -> Ok (typ, expr)
    | _ -> Error (p, Printf.sprintf "Variable %s unbound" s)
  end
  | BinOp (e1, opcode, e2) -> begin
    expr_typecheck c e1 >>= fun (typ1, _) ->
    expr_typecheck c e2 >>= fun (typ2, _) ->
      match typ1, typ2, opcode with
      | IntT, IntT, (MINUS|STAR|HIGHMULT|DIV|MOD) -> Ok (IntT, expr)
      | IntT, IntT, (LT|LTE|GTE|GT|EQEQ|NEQ) -> Ok (BoolT, expr)
      | BoolT, BoolT, (AMP|BAR|EQEQ|NEQ) -> Ok (BoolT, expr)
      | (ArrayT _ | EmptyArray), (ArrayT _ | EmptyArray), (EQEQ|NEQ) -> Ok (BoolT, expr)
      | IntT, IntT, PLUS -> Ok (IntT, expr)
      | ArrayT t1, ArrayT t2, PLUS when t1 = t2 -> Ok (ArrayT t1, expr)
      | ArrayT t, EmptyArray, PLUS
      | EmptyArray, ArrayT t, PLUS -> Ok (ArrayT t, expr)
      | EmptyArray, EmptyArray, PLUS -> Ok (EmptyArray, expr)
      | _ ->
          let binop_str = string_of_binopcode opcode in
          Error (p, Printf.sprintf "Wrong operand types for %s" binop_str)
  end
  | UnOp (opcode, e) -> begin
    expr_typecheck c e >>= fun (typ, _) ->  
      match opcode, typ with
      | UMINUS, IntT -> Ok (IntT, expr)
      | BANG, BoolT -> Ok (BoolT, expr)
      | _ ->
          let unop_str = string_of_unopcode opcode in
          Error (p, Printf.sprintf "Wrong operand type for %s" unop_str)
  end
  | Index (e1, e2) -> begin
    expr_typecheck c e1 >>= fun (typ1, _) ->
    expr_typecheck c e2 >>= fun (typ2, _) ->
      match typ1, typ2 with
      | ArrayT t, IntT -> Ok (t, expr)
      | EmptyArray, IntT -> Error (p, "Indexing into empty array")
      | _, IntT -> Error (p, "Indexing into non-array value")
      | (ArrayT _ | EmptyArray), _ -> Error (p, "Non-integer index")
      | _ -> Error (p, "Invalid types for indexing expr")
  end
  | Length e -> begin
    expr_typecheck c e >>= fun (typ, _) ->
      match typ with
      | ArrayT _ | EmptyArray -> Ok (IntT, expr)
      | _ -> Error (p, "Using length() on a non-array expr")
  end
  | FuncCall ((_, name), args) -> failwith "f"
	(*
	begin
    match String.Map.find c name, args with
    | Some (Function ([UnitT], t)), [] when t <> [UnitT] -> Ok (t, expr)
    | Some (Function (lst, t)), _ :: _ :: _ when t <> [UnitT] -> begin
      List.map ~f:(expr_typecheck c) args |> Result.all >>= fun typlist ->
        List.zip lst typlist |> function
          | Some zipped ->
              if List.for_all ~f:(fun (t1, (t2, _)) -> t1 = t2) zipped then
                Ok (t, expr)
              else Error (p, "Function args do not match parameter type")
          | None -> Error (p, "Incorrect number of arguments")
    end
    | Some (Function (t1, t2)), arg :: [] when t2 <> UnitT -> begin
      expr_typecheck c arg >>= function
        | arg_t, _ when arg_t = t1 -> Ok (t2, expr)
        | _ -> Error (p, "Function arg does not match parameter type")
    end
    | None, _ -> Error (p, Printf.sprintf "Variable %s not in scope" name)
    | _ -> Error (p, "Function call type error")
  end
	*)
let rec typ_to_t ((_,typ): 'a typ) : t =
		match typ with
		| TInt -> IntT
		| TBool -> BoolT
		| TArray (typ', _) -> 
			let t' = typ_to_t typ' in
			ArrayT t'
	
let avar_to_t ((p, av): 'a avar) : t =
	match av with	
	| AId (_, typ) -> typ_to_t typ
	| AUnderscore typ -> typ_to_t typ	

let fst_func_pass (c: context) ((p, call): Pos.callable) = 
	match call with
	| Func ((_, id), args, rets, _) ->
		if String.Map.mem c id then 
			Error (p, "Function already exists")
		else
			let args_t_list = List.map ~f:avar_to_t args in
			let rets_t_list = List.map ~f:typ_to_t rets in
			let c' = String.Map.add c id (Function (args_t_list, rets_t_list)) in
			Ok c'	
	| Proc ((_, id), args, _) ->
		if String.Map.mem c id then
			Error (p, "Procedure already exists")
		else
			let args_t_list = List.map ~f:avar_to_t args in	
			let c' = String.Map.add c id (Function (args_t_list, [UnitT])) in
			Ok c'

let is_var_shadowed c args = 
    List.fold_left (fun acc elm -> acc || (String.Map.mem c elm))
                   false
                   args

(* TODO: should the position of the errors be more accurate? i.e. the actual
 * position of the arg that was already defined *)
(* Ensures parameters do not shadow and body is well-typed *)
let snd_func_pass (c: context) ((p, call): Pos.callable) =
    match call with
    | Func ((_, id), args, rets, s) ->
        let rest_t_list = List.map ~f:typ_to_t rets in
        stmt_typecheck c s rets_t_list >>= fun r ->
        match is_var_shadowed c args, r with
        | false, Void -> Ok ()
        | _ -> Error (p, "Var has already been defined")
    | Proc ((_, id), args, s) ->
        stmt_typecheck c s [UnitT] >>= fun r ->
        if is_var_shadowed c args then
            Error (p, "Var has already been defined")
        else
            Ok ()             

let rec stmt_typecheck (c: context) ((p, stmt): Pos.stmt) (rho: t list) : (return * ('a * string)) Result =
	failwith "lol"

