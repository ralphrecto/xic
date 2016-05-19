open Async.Std
open Core.Std
open Typecheck
open Ir
open Ir_generation
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
  | UnitT -> 0
  | _ -> 1

(* special functions defined by the abi and not defined in an interface *)
let special_abi_functions = [
  ("_I_alloc_i", (Expr.IntT, Expr.IntT), false);
  ("_I_outOfBounds_p", (Expr.UnitT, Expr.UnitT), false);
  ("__concat", (Expr.TupleT ([Expr.ArrayT IntT; Expr.ArrayT IntT]), Expr.ArrayT IntT), false)
]

let get_context (map: func_contexts) (name: string) =
  match String.Map.find map name with
  (* TODO: values are coming from _I_alloc_i. fix this to include
   * _I_alloc_i in the map properly. *)
  | None ->
      if String.is_prefix ~prefix:"_I_init" name ||
         FreshGlobal.mem name || FreshSize.mem name ||
         FreshDV.mem name then
           { num_args = 0;
             num_rets = 0;
             max_args = 0;
             max_rets = 0; }
      else
        begin
        let ms = String.concat ~sep:", " (String.Map.keys map) in
        failwith (sprintf "get_context: cannot find %s; %s" name ms)
        (* failwith ("func_contexts: binding " ^ name ^ " not found") *)
        end
  | Some x -> x

let cmp_max (a1, b1) (a2, b2) = (max a1 a2, max b1 b2)

let get_context_map
    (_ : contexts)
    (fullprog : Typecheck.full_prog)
    ((_, func_decl_map): Ir.comp_unit) =

  (* pulling out info *)
  let Ast.S.FullProg (_, prog, interfaces) = fullprog in
  let (_, Ast.S.Prog (_, _, prog_classes, prog_funcs)) = prog in
  let int_class_decls, int_func_decls =
    let f (cl_acc, f_acc) (_, Ast.S.Interface (_, _, classes, calldecls)) =
      (classes @ cl_acc, calldecls @ f_acc) in
    List.fold_left ~f ~init:([], []) interfaces in

  let init_context_map =
    begin
    let open Ast.S in
    let prog_class_proj ((_, Klass ((_, class_), _, _, methods)) : Typecheck.klass) =
      let f (t, m) =
        let method_ =
          match m with
          | Func ((_, fname), _, _, _)
          | Proc ((_, fname), _, _) -> fname in
        (class_method ~class_ ~method_, t, true) in
      List.map ~f methods in
    let int_class_decl_proj (kd : Typecheck.klass_decl) =
      let (_, KlassDecl ((_, class_), _, methods)) = kd in
      let f (t, m) =
        let method_ =
          match m with
          | FuncDecl ((_, fname), _, _)
          | ProcDecl ((_, fname), _) -> fname in
        (class_method ~class_ ~method_, t, true) in
      List.map ~f methods in
    let prog_func_proj ((t, func) : Typecheck.callable) =
      (abi_callable_name (t, func), t, false) in
    let int_func_decl_proj ((typ, c): Typecheck.callable_decl) =
      (abi_callable_decl_name (typ, c), typ, false) in

    let all_funcs =
      (List.concat_map ~f:prog_class_proj prog_classes) @
      (List.concat_map ~f:int_class_decl_proj int_class_decls) @
      (List.map ~f:prog_func_proj prog_funcs) @
      (List.map ~f:int_func_decl_proj int_func_decls) @
      special_abi_functions in
    let f ctxmap (name, (arg_t, ret_t), is_method) =
      let newctx = {
        num_args = type_size arg_t + (if is_method then 1 else 0);
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

  List.fold_left ~f:map_update ~init:init_context_map (String.Map.data func_decl_map)
