open Async.Std
open Core.Std
open Typecheck
open Ir
open Ir_util

type func_context = {
  num_args : int;
  num_rets : int;
  max_args : int;
  max_rets : int;
}

type func_contexts = func_context String.Map.t

let type_size (e: Expr.t) : int =
  match e with
  | TupleT tlist -> List.length tlist
  | _ -> 1

(* special functions defined by the abi and not defined in an interface *)
let special_abi_functions = [
  ("_I_alloc_i", (Expr.IntT, Expr.IntT))
]

let get_context (map: func_contexts) (name: string) =
  match String.Map.find map name with
  (* TODO: values are coming from _I_alloc_i. fix this to include
   * _I_alloc_i in the map properly. *)
  | None -> {
      num_args = 1;
      num_rets = 1;
      max_args = 0;
      max_rets = 0;
    }
      (* failwith ("func_contexts: binding " ^ name ^ " not found") *) 
  | Some x -> x

let cmp_max (a1, b1) (a2, b2) = (max a1 a2, max b1 b2)

let get_context_map
    (int_call_decls: Typecheck.callable_decl list)
    ((_, func_decl_map): Ir.comp_unit) =

  let ir_func_decls = String.Map.data func_decl_map in
  let init_context_map =
    begin
    let ir_proj (name, _, (arg_t, ret_t)) =
      (name, (arg_t, ret_t)) in
    let int_decl_proj ((typ, c): Typecheck.callable_decl) =
      (abi_callable_decl_name (typ, c), typ) in
    let all_funcs =
      (List.map ~f:ir_proj ir_func_decls) @
      (List.map ~f:int_decl_proj int_call_decls) @
      special_abi_functions in
    let f ctxmap (name, (arg_t, ret_t)) =
      let newctx = {
        num_args = type_size arg_t;
        num_rets = type_size ret_t;
        max_args = 0;
        max_rets = 0;
      } in
      String.Map.add ctxmap ~key:name ~data:newctx in
    List.fold_left ~f ~init:String.Map.empty all_funcs
    end in

  let rec ctx_max_expr (e: Ir.expr) : (int * int) =
    match e with
    | BinOp (e1, _, e2) ->
        cmp_max (ctx_max_expr e1) (ctx_max_expr e2)
    | Call (Name fname, _) ->
        let {num_args; num_rets; _} = get_context init_context_map fname in
        (num_args, num_rets)
    | ESeq (stmt, e) ->
        cmp_max (ctx_max_stmt stmt) (ctx_max_expr e)
    | Mem (e, _) -> ctx_max_expr e
    | _ -> (0, 0)

  and ctx_max_stmt (s: Ir.stmt) : (int * int) =
    match s with
    | CJump (e, _, _)
    | CJumpOne (e, _)
    | Jump e
    | Exp e -> ctx_max_expr e
    | Move (e1, e2) ->
        cmp_max (ctx_max_expr e1) (ctx_max_expr e2)
    | Seq stmts ->
        let l = List.map ~f:ctx_max_stmt stmts in
        List.fold_left ~f:cmp_max ~init:(0,0) l
    | _ -> (0, 0) in

  let map_update ctxmap (fname, stmt, _) =
    let f = function
      | Some ctx ->
          let (max_args, max_rets) = ctx_max_stmt stmt in
          let max_rets' = max 0 (max_rets - 2) in
          (* include ret pointers as args *)
          let ret_ptrs = if max_rets > 0 then 1 else 0 in
          let max_args' = (max 0 (max_args + ret_ptrs - 6)) in
          Some {ctx with max_args = max_args'; max_rets = max_rets'}
      | None -> None in
    String.Map.change ctxmap fname ~f in

  List.fold_left ~f:map_update ~init:init_context_map ir_func_decls
