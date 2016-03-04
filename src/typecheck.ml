open Core.Std
open Ast.S

(******************************************************************************)
(* Types                                                                      *)
(******************************************************************************)
module Error = struct
  type t = Pos.pos * string
  type 'a result = ('a, t) Result.t
end

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
let dup_func_decl x = sprintf "Function %s has already been declared" x


module Expr = struct
  type t =
    | IntT
    | BoolT
    | UnitT
    | ArrayT of t
    | TupleT of t list (* len >= 2 *)
    | EmptyArray
  [@@deriving sexp]

  let rec to_string t =
    match t with
    | IntT -> "int"
    | BoolT -> "bool"
    | UnitT -> "unit"
    | ArrayT t' -> sprintf "%s[]" (to_string t')
    | TupleT ts -> sprintf "(%s)" (String.concat ~sep:", " (List.map ~f:to_string ts))
    | EmptyArray -> "{}"

  let rec of_typ (_, t) =
    match t with
    | TInt -> IntT
    | TBool -> BoolT
    | TArray (t, _) -> ArrayT (of_typ t)

  let rec (<=) a b =
    match a, b with
    | _, UnitT -> true
    | EmptyArray, ArrayT _ -> true (* TODO: is this right? *)
    | ArrayT t1, ArrayT t2 -> t1 <= t2
    | _ -> a = b

  let comparable t1 t2 =
    t1 <= t2 || t2 <= t1

  let type_max p t1 t2 : t Error.result =
    if t1 <= t2 then Ok t2
    else if t2 <= t1 then Ok t1
    else Error (p, "Incomparable types")

  let eqs p xs ys unequal_num mistyped =
    match List.zip xs ys with
    | Some zipped ->
        if List.for_all ~f:(fun (x, y) -> y <= x) zipped
          then Ok ()
          else Error (p, mistyped)
    | None -> Error (p, unequal_num)
end
open Expr

module Stmt = struct
  type t =
    | One
    | Zero
  [@@deriving sexp]

  let lub a b =
    match a, b with
    | One, _
    | _, One -> One
    | Zero, Zero -> Zero
end
open Stmt

module Sigma = struct
  type t =
    | Var of Expr.t
    | Function of Expr.t * Expr.t
  [@@deriving sexp]
end
open Sigma

module Tags = struct
  type p = unit             [@@deriving sexp]
  type u = unit             [@@deriving sexp]
  type c = Expr.t * Expr.t  [@@deriving sexp]
  type i = unit             [@@deriving sexp]
  type a = Expr.t           [@@deriving sexp]
  type v = Expr.t           [@@deriving sexp]
  type s = Stmt.t           [@@deriving sexp]
  type e = Expr.t           [@@deriving sexp]
  type t = Expr.t           [@@deriving sexp]
end
include Ast.Make(Tags)

let varsofavar av =
	match av with
	| AId ((_,id), _) -> Some id
	| _ -> None

let varsofvar v =
  match v with
  | AVar (_, AId  ((_, x), _)) -> Some x
  | AVar (_, AUnderscore _)
  | Underscore -> None

let typeofavar av =
	match av with
	| AId (_, t)
	| AUnderscore t -> Expr.of_typ t

type context = Sigma.t String.Map.t
module Context = struct
  include String.Map

  let var p c x =
    match find c x with
    | Some (Var t) -> Ok t
    | _ -> Error (p, unbound_var x)

  let func p c x =
    match find c x with
    | Some (Function (a, b)) -> Ok (a, b)
    | _ -> Error (p, unbound_call x)

  let bind c x t =
    add c ~key:x ~data:t

  let bind_all_vars c vs =
    List.fold_left vs ~init:c ~f:(fun c v ->
      match varsofvar (snd v) with
      | Some x -> bind c x (Var (fst v))
      | None -> c
    )

	let bind_all_avars c avs =
		List.fold_left avs ~init:c ~f:(fun c av ->
			match varsofavar (snd av) with
			| Some x -> bind c x (Var (fst av))
			| None -> c
	 )
end

(******************************************************************************)
(* helpers                                                                    *)
(******************************************************************************)
(* Ok and Error constructors are defined in Core.Std. If we open Result, we get
 * shadowed constructor warnings. We manually "open" map and bind to avoid the
 * warnings. *)
let (>>=) = Result.(>>=)
let (>>|) = Result.(>>|)

(******************************************************************************)
(* expr                                                                       *)
(******************************************************************************)
(* see Expr.eqs *)
let rec exprs_typecheck (p: Pos.pos)
                        (c: context)
                        (ts: Expr.t list)
                        (args: Pos.expr list)
                        (unequal_num: string)
                        (mistyped: string)
                        : expr list Error.result =
  Result.all (List.map ~f:(expr_typecheck c) args) >>= fun args' ->
  Expr.eqs p ts (List.map ~f:fst args') unequal_num mistyped >>= fun () ->
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
    let f acc (t1, _) = acc >>= type_max p t1 in
    match List.fold_left es ~f ~init:(Ok t) with
      | Ok max_t -> Ok (ArrayT max_t, Array ((t, e)::es))
      | Error _ -> Error (p, "Array elements have different types")
  end
  | Id (_, s) -> Context.var p c s >>= fun typ -> Ok (typ, Id ((), s))
  | BinOp (l, opcode, r) -> begin
    expr_typecheck c l >>= fun (lt, l) ->
    expr_typecheck c r >>= fun (rt, r) ->
    let e = BinOp ((lt, l), opcode, (rt, r)) in
    match lt, rt, opcode with
    | IntT, IntT, (MINUS|STAR|HIGHMULT|DIV|MOD) -> Ok (IntT, e)
    | IntT, IntT, (LT|LTE|GTE|GT|EQEQ|NEQ) -> Ok (BoolT, e)
    | BoolT, BoolT, (AMP|BAR|EQEQ|NEQ) -> Ok (BoolT, e)
    | ArrayT t1, ArrayT t2, (EQEQ|NEQ) when comparable t1 t2 -> Ok (BoolT, e)
    | EmptyArray, ArrayT _, (EQEQ|NEQ)
    | ArrayT _, EmptyArray, (EQEQ|NEQ)
    | EmptyArray, EmptyArray, (EQEQ|NEQ) -> Ok (BoolT, e)
    | IntT, IntT, PLUS -> Ok (IntT, e)
    | ArrayT t1, ArrayT t2, PLUS when comparable t1 t2 ->
        type_max p t1 t2 >>= fun max_t -> Ok (ArrayT max_t, e)
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
    Context.func p c f >>= fun (a, b) ->
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
let avar_typecheck c (_, a) =
  match a with
  | AId ((_, x), t) -> typ_typecheck c t >>= fun t' -> Ok (fst t', AId (((), x), t'))
  | AUnderscore t -> typ_typecheck c t >>= fun t' -> Ok (fst t', AUnderscore t')

(******************************************************************************)
(* var                                                                        *)
(******************************************************************************)
let var_typecheck c (_, v) =
  match v with
  | AVar a -> avar_typecheck c a >>= fun a' -> Ok (fst a', AVar a')
  | Underscore -> Ok (UnitT, Underscore)

(******************************************************************************)
(* stmt                                                                       *)
(******************************************************************************)
(* see Expr.eqs *)
let avars_typecheck (p: Pos.pos)
					(c: context)
					(avs: Pos.avar list)
					(dup_var: string)
					(bound_var: string)
					: avar list Error.result =
	let xs = List.filter_map ~f:varsofavar (List.map ~f:snd avs) in
	let disjoint = not (List.contains_dup xs) in
	let unbound = List.for_all xs ~f:(fun x -> not (Context.mem c x)) in
	match disjoint, unbound with
	| true, true -> Result.all (List.map ~f:(avar_typecheck c) avs)
	| false, _ -> Error (p, dup_var)
	| true, false -> Error (p, bound_var)

let vars_typecheck (p: Pos.pos)
                   (c: context)
                   (vs: Pos.var list)
                   (dup_var: string)
                   (bound_var: string)
                   : var list Error.result =
  let xs = List.filter_map ~f:varsofvar (List.map ~f:snd vs) in
  let disjoint = not (List.contains_dup xs) in
  let unbound = List.for_all xs ~f:(fun x -> not (Context.mem c x)) in
  match disjoint, unbound with
  | true, true -> Result.all (List.map ~f:(var_typecheck c) vs)
  | false, _ -> Error (p, dup_var)
  | true, false -> Error (p, bound_var)

let stmt_typecheck c rho s =
  let rec (|-) (c, rho) (p, s) : (stmt * context) Error.result =
    let err s = Error (p, s) in
    match s with
    | Block ss -> begin
        (* iteratively typecheck all the statements in the block *)
        let f ssc s =
          ssc >>= fun (ss, c) ->
          (c, rho) |- s >>= fun (s', c') ->
          Ok (s'::ss, c')
        in
        List.fold_left ss ~f ~init:(Ok ([], c)) >>= fun (ss, c) ->
        let ss = List.rev ss in

        (* make sure that all but the last stmt is of type One *)
        if List.for_all (Util.init ss) ~f:(fun (t, _) -> t = One)
          then begin
            match List.last ss with
            | Some (r, _) -> Ok ((r, (Block ss)), c)
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
      expr_typecheck c b >>= fun b' -> (c, rho) |- t >>= fun (t', _) ->
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
      Context.func p c f >>= fun (a, b) ->
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
        | _, _ -> err num_ret_args
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
                let ls = Expr.to_string (fst l') in
                let rs = Expr.to_string (fst r') in
                err (sprintf "Cannot assign type %s to type %s" ls rs)
        | _ -> err "Invalid left-hand side of assignment"
    end
    | Decl vs -> begin
        vars_typecheck p c vs dup_var_decl bound_var_decl >>= fun vs' ->
        Ok ((One, Decl vs'), Context.bind_all_vars c vs')
    end
    | DeclAsgn (vs, e) -> begin
        vars_typecheck p c vs dup_var_decl bound_var_decl >>= fun vs' ->
        expr_typecheck c e >>= fun e' ->
        match vs', fst e' with
        | _, TupleT ets' ->
            let vts' = List.map ~f:fst vs' in
            Expr.eqs p ets' vts' num_decl_vars typ_decl_vars >>= fun () ->
            Ok ((One, DeclAsgn (vs', e')), Context.bind_all_vars c vs')
        | [v'], _ ->
            Expr.eqs p [fst e'] [fst v'] num_decl_vars typ_decl_vars
            >>= fun () -> Ok ((One, DeclAsgn ([v'], e')), Context.bind_all_vars c vs')
        | _, _ -> err "Invalid declassign"
    end
  in

  (c, rho) |- s >>| fst

(******************************************************************************)
(* callables                                                                  *)
(******************************************************************************)
let avar_to_expr_t ((_, av): Pos.avar) : Expr.t =
  match av with
  | AId (_, typ) -> Expr.of_typ typ
  | AUnderscore typ -> Expr.of_typ typ

let fst_func_pass (c: context) ((p, call): Pos.callable) =
  match call with
  | Func ((_, id), args, rets, _) ->
    begin
      if Context.mem c id then
        Error (p, dup_func_decl id)
      else
        match args, rets with
        | [], [ret_typ] ->
          let ret_t = Expr.of_typ ret_typ in
          let c' = Context.add c ~key:id ~data:(Function (UnitT, ret_t)) in
          Ok c'
        | [arg_avar], [ret_typ] ->
          let arg_t = avar_to_expr_t arg_avar in
          let ret_t = Expr.of_typ ret_typ in
          let c' = Context.add c ~key:id ~data:(Function (arg_t, ret_t)) in
          Ok c'
        | _::_, [ret_typ] ->
          let args_t = TupleT (List.map ~f:avar_to_expr_t args) in
          let ret_t = Expr.of_typ ret_typ in
          let c' = Context.add c ~key:id ~data:(Function (args_t, ret_t)) in
          Ok c'
        | [], _::_ ->
          let rets_t = TupleT (List.map ~f:Expr.of_typ rets) in
          let c' = Context.add c ~key:id ~data:(Function (UnitT, rets_t)) in
          Ok c'
        | [arg_avar], _::_ ->
          let arg_t = avar_to_expr_t arg_avar in
          let rets_t = TupleT (List.map ~f:Expr.of_typ rets) in
          let c' = Context.add c ~key:id ~data:(Function (arg_t, rets_t)) in
          Ok c'
        | _::_, _::_ ->
          let args_t = TupleT (List.map ~f:avar_to_expr_t args) in
          let rets_t = TupleT (List.map ~f:Expr.of_typ rets) in
          let c' = Context.add c ~key:id ~data:(Function (args_t, rets_t)) in
          Ok c'
        | _ -> Error (p, "Invalid function type! -- shouldn't hit this case")
    end
  | Proc ((_, id), args, _) ->
    begin
      if Context.mem c id then
        Error (p, dup_func_decl id)
      else
        match args with
        |[] ->
          let c' = Context.add c ~key:id ~data:(Function (UnitT, UnitT)) in
          Ok c'
        |[arg_avar] ->
          let arg_t = avar_to_expr_t arg_avar in
          let c' = Context.add c ~key:id ~data:(Function (arg_t, UnitT)) in
          Ok c'
        |_::_ ->
          let args_t = TupleT (List.map ~f:avar_to_expr_t args) in
          let c' = Context.add c ~key:id ~data:(Function (args_t, UnitT)) in
          Ok c'
    end
(*
let check_var_shadow c ((_, av): Pos.avar) =
  match av with
  | AId ((p, id), _) ->
    if Context.mem c id then
      Error (p, (Printf.sprintf "Variable %s has already been defined" id))
    else
      Ok ()
  | _ -> Ok ()
*)
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
    match call with
    | Func ((_,id), args, rets, s) ->
      begin
        match args, rets with
        | [], [ret_typ] ->
					let ret_t = Expr.of_typ ret_typ in
          stmt_typecheck c ret_t s >>= fun stmt ->
					typ_typecheck c ret_typ >>= fun ret ->
					let call_type = (UnitT, ret_t) in
					Ok (call_type, Func (((), id), [], [ret], stmt))
        | [args'], [ret_typ] ->
					let ret_t = Expr.of_typ ret_typ in
          avars_typecheck p c args dup_var_decl bound_var_decl >>= fun avs ->
					let c' = Context.bind_all_avars c avs in
          stmt_typecheck c' ret_t s >>= fun stmt ->
					typ_typecheck c' ret_typ >>= fun ret ->
					let call_type = (typeofavar (snd args'), ret_t) in
        	Ok (call_type, Func (((), id), avs, [ret], stmt))
				| _::_, [ret_typ] ->
          let ret_t = Expr.of_typ ret_typ in
          avars_typecheck p c args dup_var_decl bound_var_decl >>= fun avs ->
         	let c' = Context.bind_all_avars c avs in
					stmt_typecheck c' ret_t s >>= fun stmt ->
					typ_typecheck c' ret_typ >>= fun ret ->
					let args_t = TupleT (List.map ~f:(fun e -> typeofavar (snd e)) args) in
					let call_type = (args_t, ret_t) in
					Ok (call_type, Func (((), id), avs, [ret], stmt))
        | [], _::_ ->
          let rets_t = TupleT (List.map ~f:Expr.of_typ rets) in
          stmt_typecheck c rets_t s >>= fun stmt ->
					Result.all (List.map ~f:(typ_typecheck c) rets) >>= fun ret_list ->
					let call_type = (UnitT, rets_t) in
					Ok (call_type, Func (((), id), [], ret_list, stmt))
        | [args'], _::_ ->
          let rets_t = TupleT (List.map ~f:Expr.of_typ rets) in
          avars_typecheck p c args dup_var_decl bound_var_decl >>= fun avs ->
          let c' = Context.bind_all_avars c avs in
					stmt_typecheck c' rets_t s >>= fun stmt ->
					Result.all (List.map ~f:(typ_typecheck c') rets) >>= fun ret_list ->
					let arg_t = typeofavar (snd args') in
					let call_type = (arg_t, rets_t) in
					Ok (call_type, Func(((), id), avs, ret_list, stmt))
        | _::_, _::_ ->
          let rets_t = TupleT (List.map ~f:Expr.of_typ rets) in
          avars_typecheck p c args dup_var_decl bound_var_decl >>= fun avs ->
         	let c' = Context.bind_all_avars c avs in
					stmt_typecheck c' rets_t s >>= fun stmt ->
					Result.all (List.map ~f:(typ_typecheck c') rets) >>= fun ret_list ->
					let args_t = TupleT (List.map ~f:(fun e -> typeofavar (snd e)) args) in
					let call_type = (args_t, rets_t) in
					Ok (call_type, Func(((), id), avs, ret_list, stmt))
        | _ -> Error (p, "Invalid function type! -- shouldn't hit this case")
      end
    | Proc ((_,id), args, s) ->
      begin
        match args with
        | [] ->
          stmt_typecheck c UnitT s >>= fun stmt ->
					let call_type = (UnitT, UnitT) in
					Ok (call_type, Proc(((), id), [], stmt))
				| [arg_avar] ->
					avars_typecheck p c args dup_var_decl bound_var_decl >>= fun avs ->
					let c' = Context.bind_all_avars c avs in
					stmt_typecheck c' UnitT s >>= fun stmt ->
					let arg_t = typeofavar (snd arg_avar) in
					let call_type = (arg_t, UnitT) in
					Ok (call_type, Proc(((), id), avs, stmt))	
        | _ ->
          avars_typecheck p c args dup_var_decl bound_var_decl >>= fun avs ->
         	let c' = Context.bind_all_avars c avs in
					stmt_typecheck c' UnitT s >>= fun stmt ->
					let args_t = TupleT (List.map ~f:(fun e -> typeofavar (snd e)) args) in
					let call_type = (args_t, UnitT) in
					Ok (call_type, Proc(((), id), avs, stmt))
      end

(******************************************************************************)
(* prog                                                                       *)
(******************************************************************************)
let prog_typecheck (_, Prog(uses, funcs)) =
  let fst_func_fold acc e =
    acc >>= fun g -> fst_func_pass g e
  in
  List.fold_left ~init: (Ok Context.empty) ~f:fst_func_fold funcs >>= fun gamma ->
	Result.all(List.map ~f: (snd_func_pass gamma) funcs) >>= fun func_list ->
	let use_typecheck use =
		match snd use with
		|Use (_, id) -> ((), Use ((), id))
	in
	let use_list = List.map ~f: use_typecheck uses in
	Ok ((), Prog (use_list, func_list))
