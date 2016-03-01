open Core.Std
open Async.Std
open Ast.S

type expr_t =
  | IntT
  | BoolT
  | UnitT
  | ArrayT of expr_t
  | TupleT of expr_t list (* len >= 2 *)
  | EmptyArray
[@@deriving sexp]

type stmt_t =
  | One   (* unit *)
  | Zero  (* void *)
[@@deriving sexp]

type sigma =
  | Var of expr_t
  | Function of expr_t * expr_t
[@@deriving sexp]

module Tags = struct
  type p = unit             [@@deriving sexp]
  type u = unit             [@@deriving sexp]
  type c = expr_t * expr_t  [@@deriving sexp]
  type i = unit             [@@deriving sexp]
  type a = expr_t           [@@deriving sexp]
  type v = expr_t           [@@deriving sexp]
  type s = stmt_t           [@@deriving sexp]
  type e = expr_t           [@@deriving sexp]
  type t = expr_t           [@@deriving sexp]
end
include Ast.Make(Tags)

type context = sigma String.Map.t
type error_msg = string

let dummy () = ()

let string_of_binopcode op =
  op |> Ast.S.sexp_of_binop_code |> Sexp.to_string

let string_of_unopcode op =
  op |> Ast.S.sexp_of_unop_code |> Sexp.to_string

let rec expr_typecheck c (p, expr) =
  failwith "a"
  (*
  match expr with
  | Int i -> Ok (IntT, Int i)
  | Bool b -> Ok (BoolT, Bool b)
  | String s -> Ok (ArrayT IntT, String s)
  | Char c -> Ok (IntT, Char c)
  | _ -> failwith "a"
  | Array [] -> Ok (EmptyArray, expr)
  | Array (hd::tl) -> begin
    expr_typecheck c hd >>= fun (typ, _) ->
    List.map ~f:(expr_typecheck c) tl |> Result.all >>= fun typlist ->
      let array_eq (el_typ, _) = match el_typ, typ with | ArrayT _, EmptyArray | EmptyArray, ArrayT _ -> true
        | _ -> el_typ = typ in
      if List.for_all ~f:array_eq typlist then
        Ok (ArrayT typ, expr)
      else Error (p, "Array elements have different types")
  end
  | Id s -> begin
    match String.Map.find c s with
    (*just variables - no function lookup*)
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
  | FuncCall ((_, name), args) -> begin
    match String.Map.find c name, args with
    | Some (Function (UnitT, t)), [] when t <> UnitT -> Ok (t, expr)
    | Some (Function (TupleT lst, t)), _ :: _ :: _ when t <> UnitT -> begin
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

let rec stmt_typecheck c rho (p, stmt) =
  failwith "lol"

  (*
let rec typ_to_expr_t ((_,typ): Pos.typ) : expr_t =
    match typ with
    | TInt -> IntT
    | TBool -> BoolT
    | TArray (typ', _) ->
      let t' = typ_to_expr_t typ' in
      ArrayT t'
      *)

  (*
let avar_to_expr_t ((_, av): Pos.avar) : expr_t =
  match av with
  | AId (_, typ) -> typ_to_expr_t typ
  | AUnderscore typ -> typ_to_expr_t typ
  *)

let fst_func_pass c (p, call) =
  failwith "A"
  (*
  match call with
  | Func ((_, id), args, rets, _) ->
    begin
      if String.Map.mem c id then
        Error (p, (Printf.sprintf "Function %s has already been defined" id))
      else
        match args, rets with
        | [], [ret_typ] ->
          let ret_t = typ_to_expr_t ret_typ in
          let c' = String.Map.add c ~key:id ~data:(Function (UnitT, ret_t)) in
          Ok c'
        | [arg_avar], [ret_typ] ->
          let arg_t = avar_to_expr_t arg_avar in
          let ret_t = typ_to_expr_t ret_typ in
          let c' = String.Map.add c ~key:id ~data:(Function (arg_t, ret_t)) in
          Ok c'
        | _::_, [ret_typ] ->
          let args_t = TupleT (List.map ~f:avar_to_expr_t args) in
          let ret_t = typ_to_expr_t ret_typ in
          let c' = String.Map.add c ~key:id ~data:(Function (args_t, ret_t)) in
          Ok c'
        | [], _::_ ->
          let rets_t = TupleT (List.map ~f:typ_to_expr_t rets) in
          let c' = String.Map.add c ~key:id ~data:(Function (UnitT, rets_t)) in
          Ok c'
        | [arg_avar], _::_ ->
          let arg_t = avar_to_expr_t arg_avar in
          let rets_t = TupleT (List.map ~f:typ_to_expr_t rets) in
          let c' = String.Map.add c ~key:id ~data:(Function (arg_t, rets_t)) in
          Ok c'
        | _::_, _::_ ->
          let args_t = TupleT (List.map ~f:avar_to_expr_t args) in
          let rets_t = TupleT (List.map ~f:typ_to_expr_t rets) in
          let c' = String.Map.add c ~key:id ~data:(Function (args_t, rets_t)) in
          Ok c'
        | _ -> Error (p, "Invalid function type! -- shouldn't hit this case")
    end
  | Proc ((_, id), args, _) ->
    begin
      if String.Map.mem c id then
        Error (p, (Printf.sprintf "Procedure %s has already been defined" id))
      else
        match args with
        |[] ->
          let c' = String.Map.add c ~key:id ~data:(Function (UnitT, UnitT)) in
          Ok c'
        |[arg_avar] ->
          let arg_t = avar_to_expr_t arg_avar in
          let c' = String.Map.add c ~key:id ~data:(Function (arg_t, UnitT)) in
          Ok c'
        |_::_ ->
          let args_t = TupleT (List.map ~f:avar_to_expr_t args) in
          let c' = String.Map.add c ~key:id ~data:(Function (args_t, UnitT)) in
          Ok c'
    end
*)

(* let check_var_shadow c ((_, av): Pos.avar) = *)
  (* match av with *)
  (* | AId ((p, id), _) -> *)
    (* if String.Map.mem c id then *)
      (* Error (p, (Printf.sprintf "Variable %s has already been defined" id)) *)
    (* else *)
      (* Ok () *)
  (* | _ -> Ok () *)

(* let check_varlist_shadow c args = *)
  (* let fold acc e = *)
    (* acc >>= fun _ -> check_var_shadow c e *)
  (* in *)
  (* List.fold_left ~f:fold ~init:(Ok ()) args *)

  (*
TODO: should the position of the errors be more accurate? i.e. the actual
 position of the arg that was already defined
Ensures parameters do not shadow and body is well-typed
*)
let snd_func_pass c (p, call) =
  failwith "yolo"
  (*
    match call with
    | Func (_, args, rets, s) ->
      begin
        match args, rets with
        | [], [ret_typ] ->
          let ret_t = typ_to_expr_t ret_typ in
          stmt_typecheck c s ret_t >>= fun r ->
          begin
            match r with
            | Void -> Ok ()
            | _ -> Error (p, "Missing return")
          end
        | [arg_avar], [ret_typ] ->
          let ret_t = typ_to_expr_t ret_typ in
          stmt_typecheck c s ret_t >>= fun r ->
          check_var_shadow c arg_avar >>= fun _ ->
          begin
            match r with
            | Void -> Ok ()
            | _ -> Error (p, "Missing return")
          end
        | _::_, [ret_typ] ->
          let ret_t = typ_to_expr_t ret_typ in
          stmt_typecheck c s ret_t >>= fun r ->
          check_varlist_shadow c args >>= fun _ ->
          begin
            match r with
            | Void -> Ok ()
            | _ -> Error (p, "Missing return")
          end
        | [], _::_ ->
          let rets_t = TupleT (List.map ~f:typ_to_expr_t rets) in
          stmt_typecheck c s rets_t >>= fun r ->
          begin
            match r with
            | Void -> Ok ()
            | _ -> Error (p, "Missing return")
          end
        | [arg_avar], _::_ ->
          let rets_t = TupleT (List.map ~f:typ_to_expr_t rets) in
          stmt_typecheck c s rets_t >>= fun r ->
          check_var_shadow c arg_avar >>= fun _ ->
          begin
            match r with
            | Void -> Ok ()
            | _ -> Error (p, "Missing return")
          end
        | _::_, _::_ ->
          let rets_t = TupleT (List.map ~f:typ_to_expr_t rets) in
          stmt_typecheck c s rets_t >>= fun r ->
          check_varlist_shadow c args >>= fun _ ->
          begin
            match r with
            | Void -> Ok ()
            | _ -> Error (p, "Missing return")
          end
        | _ -> Error (p, "Invalid function type! -- shouldn't hit this case")
      end
    | Proc (_, args, s) ->
      begin
        match args with
        | [] ->
          stmt_typecheck c s UnitT >>= fun _ ->
          Ok ()
        | [arg_avar] ->
          stmt_typecheck c s UnitT >>= fun _ ->
          check_var_shadow c arg_avar >>= fun _ ->
          Ok ()
        | _::_ ->
          stmt_typecheck c s UnitT >>= fun _ ->
          check_varlist_shadow c args >>= fun _ ->
          Ok ()
      end
*)

let prog_typecheck (_, Prog(_, funcs)) =
  failwith "a"
  (*
  let fst_func_fold acc e =
    acc >>= fun g -> fst_func_pass g e
  in
  List.fold_left ~init: (Ok String.Map.empty) ~f:fst_func_fold funcs >>= fun gamma ->
  let snd_func_fold acc e =
    acc >>= fun _ -> snd_func_pass gamma e
  in
  List.fold_left ~init: (Ok ()) ~f: snd_func_fold funcs
  *)
