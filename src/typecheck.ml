open Core.Std
open Ast.S

(******************************************************************************)
(* Types                                                                      *)
(******************************************************************************)
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
type error_msg = Pos.pos * string

(******************************************************************************)
(* helpers                                                                    *)
(******************************************************************************)
(* Ok and Error constructors are defined in Core.Std. If we open Result, we get
 * shadowed constructor warnings. We manually "open" map and bind to avoid the
 * warnings. *)
let (>>=) = Result.bind
let (>>|) = Result.map

module Errors = struct
  let num_f_args     = "Incorrect number of function arguments"
  let typ_f_args     = "Ill typed function arguments"
  let num_p_args     = "Incorrect number of procedure arguments"
  let typ_p_args     = "Ill typed procedure arguments"
  let num_ret_args   = "Incorrect number of return expressions"
  let typ_ret_args   = "Ill typed return expressions"
  let unbound_var x  = sprintf "Unbound variable %s" x
  let unbound_call x = sprintf "Unbound callable %s" x
  let dup_var_decl   = "Duplicate variable declaration"
  let bound_var_decl = "Cannot rebind variable"
  let num_decl_vars  = "Incorrect number of variables in declassign"
  let typ_decl_vars  = "Ill typed variable declassign"
end
open Errors

let rec string_of_expr_t t =
  match t with
  | IntT -> "int"
  | BoolT -> "bool"
  | UnitT -> "unit"
  | ArrayT t' -> sprintf "%s[]" (string_of_expr_t t')
  | TupleT ts ->
      sprintf "(%s)" (String.concat ~sep:", " (List.map ~f:string_of_expr_t ts))
  | EmptyArray -> "{}"

let lookup_var (p: Pos.pos) (c: context) (x: string):
               (expr_t, error_msg) Result.t =
  match String.Map.find c x with
  | Some (Var t) -> Ok t
  | _ -> Error (p, unbound_var x)

let lookup_func (p: Pos.pos) (c: context) (x: string):
                (expr_t * expr_t, error_msg) Result.t =
  match String.Map.find c x with
  | Some (Function (a, b)) -> Ok (a, b)
  | _ -> Error (p, unbound_var x)

let (<=) (a: expr_t) (b: expr_t) : bool =
  match a, b with
  | _, UnitT -> true
  | EmptyArray, ArrayT _ -> true (* TODO: is this right? *)
  | _ -> a = b

(******************************************************************************)
(* expr                                                                       *)
(******************************************************************************)
let rec expr_ts_typecheck (p: Pos.pos)
                          (xs: expr_t list)
                          (ys: expr_t list)
                          (unequal_num: string)
                          (mistyped: string)
                          : (unit, error_msg) Result.t =
  match List.zip xs ys with
  | Some zipped ->
      if List.for_all ~f:(fun (x, y) -> x <= y) zipped
        then Ok ()
        else Error (p, mistyped)
  | None -> Error (p, unequal_num)

and exprs_typecheck (p: Pos.pos)
                    (c: context)
                    (ts: expr_t list)
                    (args: Pos.expr list)
                    (unequal_num: string)
                    (mistyped: string)
                    : (expr list, error_msg) Result.t =
  Result.all (List.map ~f:(expr_typecheck c) args) >>= fun args' ->
  expr_ts_typecheck p ts (List.map ~f:fst args') unequal_num mistyped >>= fun () ->
  Ok args'

and expr_typecheck c (p, expr) =
  match expr with
  | Int i -> Ok (IntT, Int i)
  | Bool b -> Ok (BoolT, Bool b)
  | String s -> Ok (ArrayT IntT, String s)
  | Char c -> Ok (IntT, Char c)
  | Array [] -> Ok (EmptyArray, Array [])
  | Array (e::es) -> begin
    expr_typecheck c e >>= fun (t, e) ->
    Result.all (List.map ~f:(expr_typecheck c) es) >>= fun es ->
    let array_eq (t', _) =
      match t', t with
      | ArrayT _, EmptyArray
      | EmptyArray, ArrayT _ -> true
      | _ -> t' = t
    in
    if List.for_all ~f:array_eq es
      then Ok (ArrayT t, Array ((t,e)::es))
      else Error (p, "Array elements have different types")
  end
  | Id (_, s) -> lookup_var p c s >>= fun typ -> Ok (typ, Id ((), s))
  | BinOp (l, opcode, r) -> begin
    expr_typecheck c l >>= fun (lt, l) ->
    expr_typecheck c r >>= fun (rt, r) ->
    let e = BinOp ((lt, l), opcode, (rt, r)) in
    match lt, rt, opcode with
    | IntT, IntT, (MINUS|STAR|HIGHMULT|DIV|MOD) -> Ok (IntT, e)
    | IntT, IntT, (LT|LTE|GTE|GT|EQEQ|NEQ) -> Ok (BoolT, e)
    | BoolT, BoolT, (AMP|BAR|EQEQ|NEQ) -> Ok (BoolT, e)
    | (ArrayT _ | EmptyArray), (ArrayT _ | EmptyArray), (EQEQ|NEQ) -> Ok (BoolT, e)
    | IntT, IntT, PLUS -> Ok (IntT, e)
    | ArrayT t1, ArrayT t2, PLUS when t1 = t2 -> Ok (ArrayT t1, e)
    | ArrayT t, EmptyArray, PLUS
    | EmptyArray, ArrayT t, PLUS -> Ok (ArrayT t, e)
    | EmptyArray, EmptyArray, PLUS -> Ok (EmptyArray, e)
    | _ ->
        let binop_str = Ast.string_of_binop_code opcode in
        Error (p, Printf.sprintf "Wrong operand types for %s" binop_str)
  end
  | UnOp (opcode, e) -> begin
    expr_typecheck c e >>= fun (t, e) ->
    let e' = UnOp (opcode, (t, e)) in
    match opcode, t with
    | UMINUS, IntT -> Ok (IntT, e')
    | BANG, BoolT -> Ok (BoolT, e')
    | _ ->
        let unop_str = Ast.string_of_unop_code opcode in
        Error (p, Printf.sprintf "Wrong operand type for %s" unop_str)
  end
  | Index (a, i) -> begin
    expr_typecheck c a >>= fun (at, a) ->
    expr_typecheck c i >>= fun (it, i) ->
      match at, it with
      | ArrayT t, IntT -> Ok (t, Index ((at, a), (it, i)))
      | EmptyArray, IntT -> Error (p, "Indexing into empty array")
      | _, IntT -> Error (p, "Indexing into non-array value")
      | (ArrayT _ | EmptyArray), _ -> Error (p, "Non-integer index")
      | _ -> Error (p, "Invalid types for indexing expr")
  end
  | Length e -> begin
    expr_typecheck c e >>= fun (t, e) ->
      match t with
      | ArrayT _
      | EmptyArray -> Ok (IntT, Length (t, e))
      | _ -> Error (p, "Using length() on a non-array expr")
  end
  | FuncCall ((_, f), args) -> begin
    lookup_func p c f >>= fun (a, b) ->
    match (a, b), args with
    | (UnitT, t), [] when t <> UnitT -> Ok (t, FuncCall (((), f), []))
    | (TupleT t1, t2), _::_::_ when t2 <> UnitT ->
        exprs_typecheck p c t1 args num_f_args typ_f_args >>= fun args' ->
        Ok (t2, FuncCall (((), f), args'))
    | (t1, t2), [arg] when t2 <> UnitT ->
        exprs_typecheck p c [t1] [arg] num_f_args typ_f_args >>= fun args' ->
        Ok (t2, FuncCall (((), f), args'))
    | _ -> Error (p, "Function call type error")
  end

(******************************************************************************)
(* typ                                                                        *)
(******************************************************************************)
let rec typ_typecheck c (p, t) =
  match t with
  | TInt -> Ok (IntT, TInt)
  | TBool -> Ok (BoolT, TBool)
  | TArray (t, None) ->
      typ_typecheck c t >>= fun t' ->
      Ok (ArrayT (fst t'), TArray (t', None))
  | TArray (t, Some e) -> begin
      typ_typecheck c t >>= fun t' ->
      expr_typecheck c e >>= fun e' ->
      match fst e' with
      | IntT -> Ok (ArrayT (fst t'), TArray (t', Some e'))
      | _ -> Error (p, "Array size is not an int")
  end

(******************************************************************************)
(* avar                                                                       *)
(******************************************************************************)
let rec avar_typecheck c (p, a) =
  match a with
  | AId ((_, x), t) -> typ_typecheck c t >>= fun t' -> Ok (fst t', AId (((), x), t'))
  | AUnderscore t -> typ_typecheck c t >>= fun t' -> Ok (fst t', AUnderscore t')

(******************************************************************************)
(* var                                                                        *)
(******************************************************************************)
let rec var_typecheck c (p, v) =
  match v with
  | AVar a -> avar_typecheck c a >>= fun a' -> Ok (fst a', AVar a')
  | Underscore -> Ok (UnitT, Underscore)

(******************************************************************************)
(* stmt                                                                       *)
(******************************************************************************)
let rec expr_t_of_typ t =
  match t with
  | TInt -> IntT
  | TBool -> BoolT
  | TArray ((_, t), _) -> ArrayT (expr_t_of_typ t)

let lub a b =
  match a, b with
  | One, _
  | _, One -> One
  | Zero, Zero -> Zero

let varsof v =
  match v with
  | AVar (_, AId  ((_, x), _)) -> Some x
  | AVar (_, AUnderscore _)
  | Underscore -> None

let typeof v =
  match v with
  | AVar (_, AId  ((_, _), (_, t))) -> expr_t_of_typ t
  | AVar (_, AUnderscore (_, t)) -> expr_t_of_typ t
  | Underscore -> UnitT

let bind_all (c: context) (vs: var list) : context =
    List.fold_left vs ~init:c ~f:(fun c v ->
      match varsof (snd v) with
      | Some x -> String.Map.add c x (Var (fst v))
      | None -> c
    )

let vars_typecheck (p: Pos.pos)
                   (c: context)
                   (vs: Pos.var list)
                   (dup_var: string)
                   (bound_var: string)
                   : (var list, error_msg) Result.t =
  let xs = List.filter_map ~f:varsof (List.map ~f:snd vs) in
  let disjoint = not (List.contains_dup xs) in
  let unbound = List.for_all xs ~f:(fun x -> not (String.Map.mem c x)) in
  match disjoint, unbound with
  | true, true -> Result.all (List.map ~f:(var_typecheck c) vs)
  | false, _ -> Error (p, dup_var)
  | true, false -> Error (p, bound_var)

let stmt_typecheck c rho s =
  let rec (|-) (c, rho) (p, s) : (stmt * context, error_msg) Result.t =
    let err s = Error (p, s) in
    match s with
    | Block ss -> begin
        (* iteratively typecheck all the statements in the block *)
        let f s ssc =
          ssc >>= fun (ss, c) ->
          (c, rho) |- s >>= fun (s', c') ->
          Ok (s'::ss, c')
        in
        List.fold_right ss ~f ~init:(Ok ([], c)) >>= fun (ss, c) ->

        (* make sure that all but the last stmt is of type One *)
        if List.for_all (Util.init ss) ~f:(fun (t, _) -> t = One)
          then begin
            match List.last ss with
            | Some (r, sn) -> Ok ((r, (Block ss)), c)
            | None -> Ok ((One, Block ss), c)
          end
          else err "Unreachable code"
    end
    | If (b, t) -> begin
        expr_typecheck c b >>= fun b' ->
        (c, rho) |- t >>= fun (t', _) ->
        match fst b'  with
        | BoolT -> Ok ((One, If (b', t')), c)
        | _ -> err "If conditional not a boolean."
    end
    | IfElse (b, t, f) -> begin
      expr_typecheck c b >>= fun b' ->
      (c, rho) |- t >>= fun (t', _) ->
      (c, rho) |- f >>= fun (f', _) ->
      match fst b'  with
      | BoolT -> Ok ((lub (fst t') (fst f'), IfElse (b', t', f')), c)
      | _ -> err "If conditional not a boolean."
    end
    | While (b, s) ->
        expr_typecheck c b >>= fun b' ->
        (c, rho) |- s >>= fun (s', _) ->
        Ok ((One, While (b', s')), c)
    | ProcCall ((_, f), args) -> begin
      lookup_func p c f >>= fun (a, b) ->
      match (a, b), args with
      | (UnitT, UnitT), [] -> Ok ((One, ProcCall (((), f), [])), c)
      | (TupleT arg_types, UnitT), _::_::_ ->
          exprs_typecheck p c arg_types args num_p_args typ_p_args >>= fun args' ->
          Ok ((One, ProcCall (((), f), args')), c)
      | (arg_t, UnitT), [arg] ->
          exprs_typecheck p c [arg_t] [arg] num_p_args typ_p_args >>= fun args' ->
          Ok ((One, ProcCall (((), f), args')), c)
      | _, _ -> err "Procedure call type error"
    end
    | Return es -> begin
        match rho, es with
        | UnitT, [] -> Ok ((Zero, Return []), c)
        | UnitT, _ -> err "Non-empty return inside procedure call"
        | TupleT ts, es ->
            exprs_typecheck p c ts es num_ret_args typ_ret_args >>= fun es' ->
            Ok ((Zero, Return es'), c)
        | t, [e] ->
            exprs_typecheck p c [t] [e] num_ret_args typ_ret_args >>= fun es' ->
            Ok ((Zero, Return es'), c)
        | t, _ -> err num_ret_args
    end
    | Asgn (l, r) -> begin
        match snd l with
        | Id (_, _)
        | Index (_, _) ->
            expr_typecheck c l >>= fun l' ->
            expr_typecheck c r >>= fun r' ->
            if fst l' = fst r'
              then Ok ((One, Asgn (l', r')), c)
              else
                let ls = string_of_expr_t (fst l') in
                let rs = string_of_expr_t (fst r') in
                err (sprintf "Cannot assign type %s to type %s" ls rs)
        | _ -> err "Inalid left-hand side of assignment"
    end
    | Decl vs -> begin
        vars_typecheck p c vs dup_var_decl bound_var_decl >>= fun vs' ->
        Ok ((One, Decl vs'), bind_all c vs')
    end
    | DeclAsgn (vs, e) -> begin
        vars_typecheck p c vs dup_var_decl bound_var_decl >>= fun vs' ->
        expr_typecheck c e >>= fun e' ->
        match vs', fst e' with
        | _, TupleT ets' ->
            let vts' = List.map ~f:fst vs' in
            expr_ts_typecheck p ets' vts' num_decl_vars typ_decl_vars >>= fun () ->
            Ok ((One, DeclAsgn (vs', e')), bind_all c vs')
        | [v'], _ ->
            expr_ts_typecheck p [fst e'] [fst v'] num_decl_vars typ_decl_vars
            >>= fun () -> Ok ((One, DeclAsgn ([v'], e')), bind_all c vs')
        | _, _ -> err "Invalid declassign"
    end
  in

  (c, rho) |- s >>| fst

(******************************************************************************)
(* callables                                                                  *)
(******************************************************************************)
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

(******************************************************************************)
(* prog                                                                       *)
(******************************************************************************)
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
