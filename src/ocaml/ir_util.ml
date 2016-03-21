open Core.Std
open Async.Std
open Typecheck

let rec abi_type_name (is_arg: bool) (e: Typecheck.Expr.t) = match e with
  | IntT -> "i"
  | BoolT -> "b"
  | UnitT -> if is_arg then "" else "p" 
  | ArrayT t' -> "a" ^ (abi_type_name is_arg t')
  | TupleT tlist ->
    let open List in
    let tnames = fold_right ~f:( ^ ) ~init:"" (map ~f:(abi_type_name is_arg) tlist) in
    "t" ^ (string_of_int (length tlist)) ^ tnames
  | EmptyArray -> failwith "impossible"

let abi_function_name =
  let f c = if c = '_' then "__" else String.of_char c in
  String.concat_map ~f

(* Format callable names according to Xi ABI *)
let abi_callable_decl_name (c: Pos.callable_decl) : string =
  let (args_t, ret_t) = func_decl_types c in
  let func_name = match c with
    | (_, FuncDecl ((_, idstr), _, _))
    | (_, ProcDecl ((_, idstr), _)) -> idstr in
  Printf.sprintf "_I%s_%s%s"
    (abi_function_name func_name)
    (abi_type_name false ret_t)
    (abi_type_name true args_t)

let abi_callable_name (c: Typecheck.callable) : string =
  let (args_t, ret_t, func_name) =
    match c with
    | ((arg_t, ret_t), Func ((_, idstr), _, _, _))
    | ((arg_t, ret_t), Proc ((_, idstr), _, _)) -> (arg_t, ret_t, idstr) in
  Printf.sprintf "_I%s_%s%s"
    (abi_function_name func_name)
    (abi_type_name false ret_t)
    (abi_type_name true args_t)

(* id name -> ABI compliant name *)
let abi_callable_decl_names (callables: Pos.callable_decl list) =
  let f map (callable: Pos.callable_decl) = 
    let name = match callable with
      | (_, FuncDecl ((_, idstr), _, _))
      | (_, ProcDecl ((_, idstr), _)) -> idstr in
    String.Map.add map ~key:name ~data:(abi_callable_decl_name callable) in
  List.fold_left ~f ~init:String.Map.empty callables

let abi_callable_names (callables: Typecheck.callable list) =
  let f map (callable: Typecheck.callable) = 
    let name = match callable with
      | (_, Func ((_, idstr), _, _, _))
      | (_, Proc ((_, idstr), _, _)) -> idstr in
    String.Map.add map ~key:name ~data:(abi_callable_name callable) in
  List.fold_left ~f ~init:String.Map.empty callables
