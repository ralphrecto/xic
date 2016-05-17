open Core.Std

let double_underscore s =
  String.substr_replace_all s ~pattern:"_"  ~with_:"__"

let rec abi_type_name (is_arg: bool) (e: Typecheck.Expr.t) =
  let open Typecheck.Expr in
  match e with
  | IntT -> "i"
  | BoolT -> "b"
  | UnitT -> if is_arg then "" else "p"
  | ArrayT t' -> "a" ^ (abi_type_name is_arg t')
  | TupleT tlist ->
    let open List in
    let tnames = fold_right ~f:( ^ ) ~init:"" (map ~f:(abi_type_name is_arg) tlist) in
    "t" ^ (string_of_int (length tlist)) ^ tnames
  | KlassT c ->
      let escaped = double_underscore c in
      sprintf "o%d%s" (String.length escaped) (escaped)
  | EmptyArray -> failwith "impossible"
  | NullT -> failwith "impossible"

(* Format callable names according to Xi ABI *)
let abi_callable_decl_name ((typ, c): Typecheck.callable_decl) : string =
  let open Ast.S in
  let args_t, ret_t = typ in
  let func_name = match c with
    | FuncDecl ((_, idstr), _, _)
    | ProcDecl ((_, idstr), _) -> idstr in
  Printf.sprintf "_I%s_%s%s"
    (double_underscore func_name)
    (abi_type_name false ret_t)
    (abi_type_name true args_t)

let abi_callable_name (c: Typecheck.callable) : string =
  let open Ast.S in
  let (args_t, ret_t, func_name) =
    match c with
    | ((arg_t, ret_t), Func ((_, idstr), _, _, _))
    | ((arg_t, ret_t), Proc ((_, idstr), _, _)) -> (arg_t, ret_t, idstr) in
  Printf.sprintf "_I%s_%s%s"
    (double_underscore func_name)
    (abi_type_name false ret_t)
    (abi_type_name true args_t)

(* id name -> ABI compliant name *)
let abi_callable_decl_names (callables: Typecheck.callable_decl list) =
  let open Ast.S in
  let f map (callable: Typecheck.callable_decl) =
    let name = match callable with
      | (_, FuncDecl ((_, idstr), _, _))
      | (_, ProcDecl ((_, idstr), _)) -> idstr in
    String.Map.add map ~key:name ~data:(abi_callable_decl_name callable) in
  List.fold_left ~f ~init:String.Map.empty callables

let abi_callable_names (callables: Typecheck.callable list) =
  let open Ast.S in
  let f map (callable: Typecheck.callable) =
    let name = match callable with
      | (_, Func ((_, idstr), _, _, _))
      | (_, Proc ((_, idstr), _, _)) -> idstr in
    String.Map.add map ~key:name ~data:(abi_callable_name callable) in
  List.fold_left ~f ~init:String.Map.empty callables

(* ABI compliant name -> id name *)
let mangled_to_name(callables: Typecheck.callable list) =
  let open Ast.S in
  let f map (callable: Typecheck.callable) =
    let name = match callable with
      | (_, Func ((_, idstr), _, _, _))
      | (_, Proc ((_, idstr), _, _)) -> idstr in
    String.Map.add map ~key:(abi_callable_name callable) ~data:name in
  List.fold_left ~f ~init:String.Map.empty callables
