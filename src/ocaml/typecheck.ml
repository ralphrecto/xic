open Core.Std
open Ast.S

(******************************************************************************)
(* Types                                                                      *)
(******************************************************************************)
module Error = struct
  type t = Pos.pos * string
  type 'a result = ('a, t) Result.t
end

let num_f_args       = "Incorrect number of function arguments"
let typ_f_args       = "Ill typed function arguments"
let num_p_args       = "Incorrect number of procedure arguments"
let typ_p_args       = "Ill typed procedure arguments"
let num_ret_args     = "Incorrect number of return expressions"
let typ_ret_args     = "Ill typed return expressions"
let unbound_var x    = sprintf "Unbound variable %s" x
let unbound_call x   = sprintf "Unbound callable %s" x
let dup_global_decl  = "Duplicate global variable declaration"
let dup_var_decl     = "Duplicate variable declaration"
let _dup_field_decl   = "Duplicate field declaration"
let _dup_method_decl  = "Duplicate method declaration"
let _field_shadow     = "Field declaration shadows global variable"
let _field_this       = "Invalid field name \"this\""
let _field_underscore = "Field name must be declared"
let bound_var_decl   = "Cannot rebind variable"
let num_decl_vars    = "Incorrect number of variables in declassign"
let typ_decl_vars    = "Ill typed variable declassign"
let dup_func_decl x  = sprintf "Function %s has already been declared" x
let no_return        = "Function is missing return"

module Expr = struct
  type t =
    | IntT
    | BoolT
    | UnitT
    | ArrayT of t
    | TupleT of t list (* len >= 2 *)
    | EmptyArray
    | NullT
    | KlassT of string
    [@@deriving sexp, compare]

  type subtyping = t -> t -> bool

  let rec to_string t =
    match t with
    | IntT -> "int"
    | BoolT -> "bool"
    | UnitT -> "unit"
    | ArrayT t' -> sprintf "[%s]" (to_string t')
    | TupleT ts -> sprintf "(%s)" (String.concat ~sep:", " (List.map ~f:to_string ts))
    | EmptyArray -> "{}"
    | KlassT name -> sprintf "class %s" name
    | NullT -> "null"

  let rec of_typ (_, t) =
    match t with
    | TInt -> IntT
    | TBool -> BoolT
    | TArray (t, _) -> ArrayT (of_typ t)
    | TKlass (_, name) -> KlassT name

  let rec array_subtyping : subtyping =
    fun t1 t2 ->
    match t1, t2 with
    | NullT, EmptyArray
    | NullT, ArrayT _
    | NullT, KlassT _
    | EmptyArray, ArrayT _ -> true
    | ArrayT t1', ArrayT t2' -> array_subtyping t1' t2'
    | _ -> false

  (* classmap is a map from class name -> superclass name *)
  let make_subtype_rel (classmap : string String.Map.t) : subtyping =
    let rec class_subtype (c1 : string) (c2 : string) : bool =
      match String.Map.find classmap c1 with
      | _ when c1 = c2 -> true
      | Some super ->
          if super = c2 then true else
          class_subtype super c2
      | None -> false in
    fun t1 t2 ->
      match t1, t2 with
      | KlassT c1, KlassT c2 -> class_subtype c1 c2
      | _ -> array_subtyping t1 t2

  let comparable (( <= ) : subtyping) (t1 : t) (t2 : t) =
    t1 <= t2 || t2 <= t1

  let type_max (( <= ) : subtyping) (p : Pos.pos) (t1 : t) (t2 : t) =
    if t1 <= t2 then Ok t2
    else if t2 <= t1 then Ok t1
    else Error (p, "Incomparable types")

  let eqs (( <= ) : subtyping) p xs ys unequal_num mistyped =
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

module Eta = struct
  type t =
    | Var of Expr.t
  [@@deriving sexp]
end

module KlassM = struct
  type t = {
    name      : string;
    super     : string option;
    fields    : (string * Pos.typ) list;
    methods   : Pos.callable_decl list;
    overrides : Pos.callable_decl list;
  } [@@deriving sexp]
end
open KlassM

module T = struct
  type p = unit             [@@deriving sexp]
  type u = unit             [@@deriving sexp]
  type g = unit             [@@deriving sexp]
  type k = unit             [@@deriving sexp]
  type c = Expr.t * Expr.t  [@@deriving sexp]
  type i = unit             [@@deriving sexp]
  type a = Expr.t           [@@deriving sexp]
  type v = Expr.t           [@@deriving sexp]
  type s = Stmt.t           [@@deriving sexp]
  type e = Expr.t           [@@deriving sexp]
  type t = Expr.t           [@@deriving sexp]
end
include Ast.Make(T)

module D = struct
  include T
  let dummy_p = ()
  let dummy_u = ()
  let dummy_g = ()
  let dummy_k = ()
  let dummy_c = (EmptyArray, EmptyArray)
  let dummy_i = ()
  let dummy_a = EmptyArray
  let dummy_v = EmptyArray
  let dummy_s = Zero
  let dummy_e = EmptyArray
  let dummy_t = EmptyArray
end
module Abbreviations = Ast.Abbreviate(D)

let varsofavar av =
  match av with
  | Ast.S.AId ((_,id), _) -> Some id
  | _ -> None

let varsofvar v =
  match v with
  | AVar (_, AId  ((_, x), _)) -> Some x
  | AVar (_, AUnderscore _)
  | Underscore -> None

let varsofvars vs =
  List.map ~f:(fun x -> varsofvar (snd x)) vs
  |> List.filter ~f:is_some
  |> List.map ~f:(function | Some x -> x | None -> failwith "can't happen")

let typeofavar av =
  match av with
  | Ast.S.AId (_, t)
  | Ast.S.AUnderscore t -> Expr.of_typ t

let _ids_of_callables (_, c) =
  match c with
  | Func (i, _, _, _)
  | Proc (i, _, _) -> i

let typeof_callable ((_, c) : Pos.callable_decl) : Expr.t * Expr.t =
  let tuplefy (tl : Expr.t list) =
    match tl with
    | [] -> UnitT
    | [t] -> t
    | _ :: _ :: _ -> TupleT tl in
  match c with
  | FuncDecl (_, avars, raw_rettypes) ->
    let avars_t = List.map ~f:(fun (_, av) -> typeofavar av) avars in
    let ret_types = List.map ~f:of_typ raw_rettypes in
    tuplefy avars_t, tuplefy ret_types
  | ProcDecl (_, avars) ->
    let avars_t = List.map ~f:(fun (_, av) -> typeofavar av) avars in
    tuplefy avars_t, UnitT

type context = Sigma.t String.Map.t
type global_context = Eta.t String.Map.t
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

  let bind_all_vars_no_underscore c vs =
    let (>>=) = Result.(>>=) in
    List.fold_left vs ~init:(Ok c) ~f:(fun c ((p, v): Pos.var) ->
      c >>= fun c ->
      match v with
      | AVar (_, AId ((_, x), t)) -> Ok (bind c x (Var (Expr.of_typ t)))
      | AVar (_, AUnderscore _)
      | Underscore -> Error (p, "unexpected underscore!")
    )

  let bind_all_avars c avs =
    List.fold_left avs ~init:c ~f:(fun c av ->
        match varsofavar (snd av) with
        | Some x -> bind c x (Var (fst av))
        | None -> c
      )
end

type contexts = {
  locals        : context;
  globals       : global_context;
  delta_m       : KlassM.t String.Map.t;
  class_context : string option;
  delta_i       : KlassM.t String.Map.t;
  (* subtyping relation, including class hierarchy *)
  subtype       : Expr.t -> Expr.t -> bool;
}

(******************************************************************************)
(* helpers                                                                    *)
(******************************************************************************)
(* Ok and Error constructors are defined in Core.Std. If we open Result, we get
 * shadowed constructor warnings. We manually "open" map and bind to avoid the
 * warnings. *)
let (>>=) = Result.(>>=)
let (>>|) = Result.(>>|)

let get_klass_info (c: contexts) (typ, _) pos : KlassM.t Error.result =
  match typ with
  | KlassT classname -> begin
      String.Map.find c.delta_m classname |> function
      | None -> Error (pos, sprintf "Class %s not declared in module" classname)
      | Some kl_info -> Ok kl_info
  end
  | _ -> Error (pos, "Not an object expression")

let find_callable (clist : Pos.callable_decl list) (name : string) =
  let f (_, c) =
    match c with
    | FuncDecl ((_, fname), _, _)
    | ProcDecl ((_, fname), _) -> fname = name in
  List.find ~f clist

(******************************************************************************)
(* expr                                                                       *)
(******************************************************************************)
(* see Expr.eqs *)
let rec exprs_typecheck (p: Pos.pos)
    (c: contexts)
    (ts: Expr.t list)
    (args: Pos.expr list)
    (unequal_num: string)
    (mistyped: string)
  : expr list Error.result =
  Result.all (List.map ~f:(expr_typecheck c) args) >>= fun args' ->
  Expr.eqs c.subtype p ts (List.map ~f:fst args') unequal_num mistyped >>= fun () ->
  Ok args'

and call_helper (c : contexts) p argtype rettype args : expr list Error.result =
  match argtype, rettype, args with
  (* proc *)
  | _, UnitT, _ -> Error (p, "Using proc call as an expr")
  (* impossible types / invariant failures *)
  | EmptyArray, _, _
  | _, EmptyArray, _ ->
    Error (p, "Typechecking: invariant failure (FuncCall empty array)")
  (* no args *)
  | UnitT, _, _::_ -> Error (p, "Giving args to a function with no params")
  | UnitT, _, [] -> Ok []
  (* one arg *)
  | (IntT | BoolT | ArrayT _ | NullT | KlassT _ ), _, _ when List.length args <> 1 ->
    Error (p, num_f_args)
  | (IntT | BoolT | ArrayT _ | NullT | KlassT _ ) as t1, _, [arg] ->
    exprs_typecheck p c [t1] [arg] num_f_args typ_f_args >>= fun args' ->
    Ok args'
  (* multiple args *)
  | TupleT argtypes, _, _ -> begin
    if List.length argtypes <> List.length args then
      Error (p, num_f_args)
    else
      exprs_typecheck p c argtypes args num_f_args typ_f_args >>= fun args' ->
      Ok args'
  end
  | _ -> Error (p, "Invalid arguments to call")


and expr_typecheck (c : contexts) (p, expr) =
  match expr with
  | Int i -> Ok (IntT, Int i)
  | Bool b -> Ok (BoolT, Bool b)
  | String "" -> Ok (EmptyArray, String "")
  | String s -> Ok (ArrayT IntT, String s)
  | Char c -> Ok (IntT, Char c)
  | Array [] -> Ok (EmptyArray, Array [])
  | Array (e :: es) -> begin
      expr_typecheck c e >>= fun (t, e) ->
      Result.all (List.map ~f:(expr_typecheck c) es) >>= fun es ->
      let f acc (t1, _) = acc >>= type_max array_subtyping p t1 in
      match List.fold_left es ~f ~init:(Ok t) with
      | Ok max_t -> Ok (ArrayT max_t, Array ((t, e)::es))
      | Error _ -> Error (p, "Array elements must have invariant types")
    end
  | Id (_, s) -> Context.var p c.locals s >>= fun typ -> Ok (typ, Id ((), s))
  | BinOp (l, opcode, r) -> begin
      expr_typecheck c l >>= fun (lt, l) ->
      expr_typecheck c r >>= fun (rt, r) ->
      let e = BinOp ((lt, l), opcode, (rt, r)) in
      match lt, rt, opcode with
      | IntT, IntT, (MINUS|STAR|HIGHMULT|DIV|MOD) -> Ok (IntT, e)
      | IntT, IntT, (LT|LTE|GTE|GT|EQEQ|NEQ) -> Ok (BoolT, e)
      | BoolT, BoolT, (AMP|BAR|EQEQ|NEQ) -> Ok (BoolT, e)
      | IntT, IntT, PLUS -> Ok (IntT, e)
      (* array equality *)
      | _, _, (EQEQ|NEQ) when comparable array_subtyping lt rt -> Ok (BoolT, e)
      (* array concat *)
      | _, _, PLUS when array_subtyping EmptyArray lt && array_subtyping EmptyArray rt ->
        type_max array_subtyping p lt rt >>= fun max_t -> Ok (ArrayT max_t, e)
      | _ ->
        (* TODO: handle equality over objects *)
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
      | NullT, IntT -> Error (p, "Indexing into null array")
      | _, IntT -> Error (p, "Indexing into non-array value")
      | _ -> Error (p, "Invalid types for indexing expr")
    end
  | Length e -> begin
      expr_typecheck c e >>= fun (t, e) ->
      match t with
      | ArrayT _
      | EmptyArray -> Ok (IntT, Length (t, e))
      | NullT -> Error (p, "Calling length() on null expr")
      | _ -> Error (p, "Using length() on a non-array expr")
    end
  | FuncCall ((_, f), args) ->
      Context.func p c.locals f >>= fun (argtype, rettype) ->
      call_helper c p argtype rettype args >>= fun args' ->
      Ok (rettype, FuncCall (((), f), args'))
  | Null -> Ok (NullT, Null)
  | New (_, classname) ->
      if String.Map.mem c.delta_m classname then
        Ok (KlassT classname, New ((), classname))
      else
        Error (p, sprintf "Class %s not declared in module" classname)
  | FieldAccess (receiver, (_, fname)) -> begin
      expr_typecheck c receiver >>= fun receiver' ->
      get_klass_info c receiver' p >>= fun klass_info ->
      List.Assoc.find klass_info.fields fname |> function
      | None ->
          Error (p, sprintf "Class %s does not have field %s" klass_info.name fname)
      | Some typ ->
          Ok (of_typ typ, FieldAccess (receiver', ((), fname)))
  end
  | MethodCall (receiver, (_, mname), args) -> begin
      expr_typecheck c receiver >>= fun receiver' ->
      get_klass_info c receiver' p >>= fun klass_info ->
      find_callable klass_info.methods mname |> function
      | None ->
          Error (p, sprintf "Class %s does not have method %s" klass_info.name mname)
      | Some callable ->
          let argtype, rettype = typeof_callable callable in
          call_helper c p argtype rettype args >>= fun args' ->
          Ok (rettype, MethodCall (receiver', ((), mname), args'))
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
  | TKlass (_, name) -> Ok (KlassT name, TKlass ((), name))

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
(* Checks that the avars are disjoint and have not been declared before. *)
let avars_typecheck (p: Pos.pos)
    (c: contexts)
    (avs: Pos.avar list)
    (dup_var: string)
    (bound_var: string)
  : avar list Error.result =
  let xs = List.filter_map ~f:varsofavar (List.map ~f:snd avs) in
  let disjoint = not (List.contains_dup xs) in
  let unbound = List.for_all xs ~f:(fun x -> not (Context.mem c.locals x)) in
  match disjoint, unbound with
  | true, true -> Result.all (List.map ~f:(avar_typecheck c) avs)
  | false, _ -> Error (p, dup_var)
  | true, false -> Error (p, bound_var)

(* Checks that the vars are disjoint and have not been declared before. *)
let vars_typecheck (p: Pos.pos)
    (c: contexts)
    (vs: Pos.var list)
    (dup_var: string)
    (bound_var: string)
  : var list Error.result =
  let xs = List.filter_map ~f:varsofvar (List.map ~f:snd vs) in
  let disjoint = not (List.contains_dup xs) in
  let unbound = List.for_all xs ~f:(fun x -> not (Context.mem c.locals x)) in
  match disjoint, unbound with
  | true, true -> Result.all (List.map ~f:(var_typecheck c) vs)
  | false, _ -> Error (p, dup_var)
  | true, false -> Error (p, bound_var)

let stmt_typecheck (c : contexts) rho s =
  let rec (|-) (c, rho) (p, s) : (stmt * contexts) Error.result =
    let err s = Error (p, s) in
    match s with
    | Block ss -> begin
        (* iteratively typecheck all the statements in the block *)
        let f ssc s =
          ssc >>= fun (ss, c) ->
          (c, rho) |- s >>= fun (s', c') ->
          Ok (s'::ss, c')
        in
        List.fold_left ss ~f ~init:(Ok ([], c)) >>= fun (ss, _) ->
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
    | While (b, s) -> begin
        expr_typecheck c b >>= fun b' ->
        (c, rho) |- s >>= fun (s', _) ->
        match fst b'  with
        | BoolT -> Ok ((One, While (b', s')), c)
        | _ -> err "While conditional not a boolean."
      end
    | ProcCall ((_, f), args) -> begin
        Context.func p c.locals f >>= fun (a, b) ->
        match (a, b), args with
        | (UnitT, _), _::_ -> Error (p, "Giving args to a proc with no params")
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
          if fst r' <= fst l'
          then Ok ((One, Asgn (l', r')), c)
          else
            let ls = Expr.to_string (fst l') in
            let rs = Expr.to_string (fst r') in
            err (sprintf "Cannot assign type %s to type %s" ls rs)
        | _ -> err "Invalid left-hand side of assignment"
      end
    | Decl vs -> begin
        vars_typecheck p c vs dup_var_decl bound_var_decl >>= fun vs' ->
        Ok ((One, Decl vs'), {c with locals = Context.bind_all_vars c.locals vs'})
      end
    | DeclAsgn (vs, e) -> begin
        vars_typecheck p c vs dup_var_decl bound_var_decl >>= fun vs' ->
        expr_typecheck c e >>= fun e' ->
        match vs', fst e' with
        | _, TupleT ets' ->
          let vts' = List.map ~f:fst vs' in
          Expr.eqs c.subtype p vts' ets' num_decl_vars typ_decl_vars >>= fun () ->
          Ok ((One, DeclAsgn (vs', e')), {c with locals = Context.bind_all_vars c.locals vs'})
        | [v'], _ ->
          Expr.eqs c.subtype p [fst v'] [fst e'] num_decl_vars typ_decl_vars
          >>= fun () -> Ok ((One, DeclAsgn ([v'], e')), {c with locals = Context.bind_all_vars c.locals vs'})
        | _, _ -> err "Invalid declassign"
      end
    | Break -> failwith "TODO"
    | MethodCallStmt (_, _, _) -> failwith "TODO"
  in

  (c, rho) |- s >>| fst

(******************************************************************************)
(* callables                                                                  *)
(******************************************************************************)
let avar_to_expr_t ((_, av): Pos.avar) : Expr.t =
  match av with
  | AId (_, typ) -> Expr.of_typ typ
  | AUnderscore typ -> Expr.of_typ typ

let func_decl_typecheck (c: contexts) ((p, call): Pos.callable_decl) =
  match call with | FuncDecl ((_, id), args, rets) ->
    begin
      if Context.mem c.locals id then
        Error (p, dup_func_decl id)
      else
        match args, rets with
        | [], [ret_typ] ->
          let ret_t = Expr.of_typ ret_typ in
          let c' = {c with locals = Context.add c.locals ~key:id ~data:(Function (UnitT, ret_t))} in
          Ok c'
        | [arg_avar], [ret_typ] ->
          let arg_t = avar_to_expr_t arg_avar in
          let ret_t = Expr.of_typ ret_typ in
          let c' = {c with locals = Context.add c.locals ~key:id ~data:(Function (arg_t, ret_t))} in
          Ok c'
        | _::_, [ret_typ] ->
          let args_t = TupleT (List.map ~f:avar_to_expr_t args) in
          let ret_t = Expr.of_typ ret_typ in
          let c' = {c with locals = Context.add c.locals ~key:id ~data:(Function (args_t, ret_t))} in
          Ok c'
        | [], _::_ ->
          let rets_t = TupleT (List.map ~f:Expr.of_typ rets) in
          let c' = {c with locals = Context.add c.locals ~key:id ~data:(Function (UnitT, rets_t))} in
          Ok c'
        | [arg_avar], _::_ ->
          let arg_t = avar_to_expr_t arg_avar in
          let rets_t = TupleT (List.map ~f:Expr.of_typ rets) in
          let c' = {c with locals = Context.add c.locals ~key:id ~data:(Function (arg_t, rets_t))} in
          Ok c'
        | _::_, _::_ ->
          let args_t = TupleT (List.map ~f:avar_to_expr_t args) in
          let rets_t = TupleT (List.map ~f:Expr.of_typ rets) in
          let c' = {c with locals = Context.add c.locals ~key:id ~data:(Function (args_t, rets_t))} in
          Ok c'
        | _ -> Error (p, "Invalid function type! -- shouldn't hit this case")
    end
    | ProcDecl ((_, id), args) ->
      begin
        if Context.mem c.locals id then
          Error (p, dup_func_decl id)
        else
          match args with
          |[] ->
            let c' = {c with locals = Context.add c.locals ~key:id ~data:(Function (UnitT, UnitT))} in
            Ok c'
          |[arg_avar] ->
            let arg_t = avar_to_expr_t arg_avar in
            let c' = {c with locals = Context.add c.locals ~key:id ~data:(Function (arg_t, UnitT))} in
            Ok c'
          |_::_ ->
            let args_t = TupleT (List.map ~f:avar_to_expr_t args) in
            let c' = {c with locals = Context.add c.locals ~key:id ~data:(Function (args_t, UnitT))} in
            Ok c'
      end

let func_typecheck (c: contexts) ((p, call): Pos.callable) =
  let call' = match call with
    | Func (i, args, rets, _) -> FuncDecl (i, args, rets)
    | Proc (i, args, _) -> ProcDecl (i, args) in
  func_decl_typecheck c (p, call')

let fst_func_pass (prog_funcs : Pos.callable list) (interfaces : Pos.interface list) =
  let empty_contexts = {
    locals        = Context.empty;
    globals       = String.Map.empty;
    delta_m       = String.Map.empty;
    class_context = None;
    delta_i       = String.Map.empty;
    subtype       = (fun _ _ -> false);
  } in
  let interface_map_fold (_, Interface (_, _, l)) =
    let func_decl_fold acc e =
      acc >>= fun g -> func_decl_typecheck g e in
    List.fold_left ~init:(Ok empty_contexts) ~f:func_decl_fold l in
  let inter_contexts =
    List.map ~f:interface_map_fold interfaces in
  let func_fold acc e =
    acc >>= fun g -> func_typecheck g e in
  let prog_context =
    List.fold_left ~init:(Ok empty_contexts) ~f:func_fold prog_funcs in
  prog_context::inter_contexts |> Result.all >>= fun contexts ->
  let context_fold big_context next_context =
    let context_union ~key ~data unified_res =
      unified_res >>= fun unified ->
      match String.Map.find unified.locals key with
      | Some data' ->
        if data' = data then Ok unified
        else
          Error ((-1, -1), sprintf "function %s has inconsistent type declarations" key)
      | None -> Ok {unified with locals = (Context.bind unified.locals key data)} in
    String.Map.fold ~init:big_context ~f:context_union next_context.locals in
  List.fold_left ~init:(Ok empty_contexts) ~f:context_fold contexts

(*
TODO: should the position of the errors be more accurate? i.e. the actual
 position of the arg that was already defined
Ensures parameters do not shadow and body is well-typed
*)
let snd_func_pass c (p, call) =
  match call with
  | Ast.S.Func ((_,id), args, rets, s) ->
    begin
      match args, rets with
      | [], [ret_typ] ->
        let ret_t = Expr.of_typ ret_typ in
        stmt_typecheck c ret_t s >>= fun stmt ->
        begin
          match stmt with
          | Zero, _ -> typ_typecheck c ret_typ >>= fun ret ->
            let call_type = (UnitT, ret_t) in
            Ok (call_type, Ast.S.Func (((), id), [], [ret], stmt))
          | One, _ -> Error (p, no_return)
        end
      | [args'], [ret_typ] ->
        let ret_t = Expr.of_typ ret_typ in
        avars_typecheck p c args dup_var_decl bound_var_decl >>= fun avs ->
        let c' = {c with locals = Context.bind_all_avars c.locals avs} in
        stmt_typecheck c' ret_t s >>= fun stmt ->
        begin
          match fst stmt with
          | Zero -> typ_typecheck c' ret_typ >>= fun ret ->
            let call_type = (typeofavar (snd args'), ret_t) in
            Ok (call_type, Ast.S.Func (((), id), avs, [ret], stmt))
          | One -> Error (p, no_return)
        end
      | _::_, [ret_typ] ->
        let ret_t = Expr.of_typ ret_typ in
        avars_typecheck p c args dup_var_decl bound_var_decl >>= fun avs ->
        let c' = {c with locals = Context.bind_all_avars c.locals avs} in
        stmt_typecheck c' ret_t s >>= fun stmt ->
        begin
          match fst stmt with
          | Zero -> typ_typecheck c' ret_typ >>= fun ret ->
            let args_t = TupleT (List.map ~f:(fun e -> typeofavar (snd e)) args) in
            let call_type = (args_t, ret_t) in
            Ok (call_type, Ast.S.Func (((), id), avs, [ret], stmt))
          | One -> Error (p, no_return)
        end
      | [], _::_ ->
        let rets_t = TupleT (List.map ~f:Expr.of_typ rets) in
        stmt_typecheck c rets_t s >>= fun stmt ->
        begin
          match fst stmt with
          | Zero -> Result.all (List.map ~f:(typ_typecheck c) rets) >>= fun ret_list ->
            let call_type = (UnitT, rets_t) in
            Ok (call_type, Ast.S.Func (((), id), [], ret_list, stmt))
          | One -> Error (p, no_return)
        end
      | [args'], _::_ ->
        let rets_t = TupleT (List.map ~f:Expr.of_typ rets) in
        avars_typecheck p c args dup_var_decl bound_var_decl >>= fun avs ->
        let c' = {c with locals = Context.bind_all_avars c.locals avs} in
        stmt_typecheck c' rets_t s >>= fun stmt ->
        begin
          match fst stmt with
          | Zero -> Result.all (List.map ~f:(typ_typecheck c') rets) >>= fun ret_list ->
            let arg_t = typeofavar (snd args') in
            let call_type = (arg_t, rets_t) in
            Ok (call_type, Ast.S.Func(((), id), avs, ret_list, stmt))
          | One -> Error (p, no_return)
        end
      | _::_, _::_ ->
        let rets_t = TupleT (List.map ~f:Expr.of_typ rets) in
        avars_typecheck p c args dup_var_decl bound_var_decl >>= fun avs ->
        let c' = {c with locals = Context.bind_all_avars c.locals avs} in
        stmt_typecheck c' rets_t s >>= fun stmt ->
        begin
          match fst stmt with
          | Zero -> Result.all (List.map ~f:(typ_typecheck c') rets) >>= fun ret_list ->
            let args_t = TupleT (List.map ~f:(fun e -> typeofavar (snd e)) args) in
            let call_type = (args_t, rets_t) in
            Ok (call_type, Ast.S.Func(((), id), avs, ret_list, stmt))
          | One -> Error (p, no_return)
        end
      | _ -> Error (p, "Invalid function type! -- shouldn't hit this case")
    end
  | Ast.S.Proc ((_,id), args, s) ->
    begin
      match args with
      | [] ->
        stmt_typecheck c UnitT s >>= fun stmt ->
        let call_type = (UnitT, UnitT) in
        Ok (call_type, Ast.S.Proc(((), id), [], stmt))
      | [arg_avar] ->
        avars_typecheck p c args dup_var_decl bound_var_decl >>= fun avs ->
        let c' = {c with locals = Context.bind_all_avars c.locals avs} in
        stmt_typecheck c' UnitT s >>= fun stmt ->
        let arg_t = typeofavar (snd arg_avar) in
        let call_type = (arg_t, UnitT) in
        Ok (call_type, Ast.S.Proc(((), id), avs, stmt))
      | _ ->
        avars_typecheck p c args dup_var_decl bound_var_decl >>= fun avs ->
        let c' = {c with locals = Context.bind_all_avars c.locals avs} in
        stmt_typecheck c' UnitT s >>= fun stmt ->
        let args_t = TupleT (List.map ~f:(fun e -> typeofavar (snd e)) args) in
        let call_type = (args_t, UnitT) in
        Ok (call_type, Ast.S.Proc(((), id), avs, stmt))
    end

let callable_decl_typecheck ((_, c): Pos.callable_decl) : callable_decl Error.result =
  let tuple_or_nah (l: Expr.t list) : Expr.t =
    match l with
    | [hd] -> hd
    | _::_ -> TupleT l
    | [] -> UnitT in
  let empty_contexts = {
    locals        = Context.empty;
    globals       = String.Map.empty;
    delta_m       = String.Map.empty;
    class_context = None;
    delta_i       = String.Map.empty;
    subtype       = (fun _ _ -> false);
  } in
  match c with
  | FuncDecl ((_, id), args, rets) ->
    Result.all (List.map ~f:(avar_typecheck empty_contexts) args) >>= fun args' ->
    Result.all (List.map ~f:(typ_typecheck empty_contexts) rets) >>= fun rets' ->
    let args_t = List.map ~f:avar_to_expr_t args in
    let rets_t = List.map ~f:Expr.of_typ rets in
    let typ = tuple_or_nah args_t, tuple_or_nah rets_t in
    Ok (typ, FuncDecl (((), id), args', rets'))
  | ProcDecl ((_, id), args) ->
    Result.all (List.map ~f:(avar_typecheck empty_contexts) args) >>= fun args' ->
    let args_t = List.map ~f:avar_to_expr_t args in
    Ok ((tuple_or_nah args_t, UnitT), ProcDecl (((), id), args'))

(******************************************************************************)
(* interfaces                                                                 *)
(******************************************************************************)
let interface_typecheck
  ((_, Interface (_, _, callable_decls)): Pos.interface) : interface Error.result =
  Result.all (List.map ~f:callable_decl_typecheck callable_decls) >>= fun decls' ->
  (* TODO MUST CHANGE!!! RIGHT NOW RETURNING EMPTY LIST FOR CLASSES AND USES *)
    Ok ((), Interface ([], [], decls'))

(******************************************************************************)
(* globals                                                                    *)
(******************************************************************************)
let fst_global_pass contexts globals =
  let init = Ok ([], Context.empty) in
  List.fold_left globals ~init ~f:(fun acc (_, g) ->
    acc >>= fun (ids, c) ->
    match g with
    | Gdecl vlist
    | GdeclAsgn (vlist, _) ->
        (Context.bind_all_vars_no_underscore c vlist) >>= fun c ->
        Ok (ids @ (varsofvars vlist), c)
  ) >>= fun (ids, new_vars) ->
  if not (List.contains_dup ids)
    then Ok ({contexts with locals = new_vars})
    else Error ((-1, -1), dup_global_decl)

(******************************************************************************)
(* klasses                                                                    *)
(******************************************************************************)
module KlassVertex = struct
  type t = Pos.klass
  let compare = Pervasives.compare
  let hash    = Hashtbl.hash
  let equal   = (=)
end

module KlassGraph = Graph.Persistent.Digraph.Concrete(KlassVertex)

let class_graph klasses =
  let klass_alist = List.map klasses ~f:(fun k ->
    match k with
    | (_, Klass ((_, name), _, _, _)) -> (name, k)
  ) in
  let klass_index = String.Map.of_alist_exn klass_alist in

  List.fold_left klasses ~init:KlassGraph.empty ~f:(fun g k ->
    let g = KlassGraph.add_vertex g k in

    let super =
      match k with
      | (_, Klass (_, super, _, _)) -> super
    in
    match super with
    | Some (_, s) -> KlassGraph.add_edge g (String.Map.find_exn klass_index s) k
    | None -> g
  )

let fst_klass_pass _contexts _klasses =
  failwith "TODO"
  (*
  let update_klass acc (p, Klass ((_,k), super, fields, methods)) =
    acc >>= fun contexts' ->
    let get_field acc (_, f) =
      acc >>= fun (ids, field_acc) ->
      match f with
        | AUnderscore _ -> Error (p, field_underscore)
        | AId ((_, id), t) -> Ok (id::ids, String.Map.add field_acc ~key:id ~data:t)
    in
    List.fold_left ~f:get_field ~init:(Ok ([], String.Map.empty)) fields >>= fun (ids, fields') ->
    let has_this = not ((List.filter ~f:(fun x -> x = "this") ids) = []) in
    let shadow_global =
      let globals = String.Map.keys contexts'.locals in
      not ((List.filter ~f:(fun x -> List.exists globals ~f:(fun y -> y = x)) ids) = [])
    in
    let has_dup = List.contains_dup ids in
    match has_this, shadow_global, has_dup with
    | true, _, _ -> Error (p, field_this)
    | _, true, _ -> Error (p, field_shadow)
    | _, _, true -> Error (p, dup_field_decl)
    | _ ->
      let method_names = List.map ~f:ids_of_callables methods in
      if List.contains_dup method_names then Error (p, dup_method_decl)
      else
        let klass =
          match super with
          | Some (_, s) -> {name = k; super = Some s; fields = fields'; methods}
          | None  -> {name = k; super = None; fields = fields'; methods}
        in
        let delta_m' = String.Map.add contexts.delta_m ~key:k ~data:klass in
        Ok ({contexts' with delta_m = delta_m'})
  in
  List.fold_left ~f:update_klass ~init:(Ok contexts) klasses
  *)

(******************************************************************************)
(* prog                                                                       *)
(******************************************************************************)
let prog_typecheck (FullProg (name, (_, Prog(uses, globals, klasses, funcs)), interfaces): Pos.full_prog) =
  let empty_contexts = {
    locals        = Context.empty;
    globals       = String.Map.empty;
    delta_m       = String.Map.empty;
    class_context = None;
    delta_i       = String.Map.empty;
    subtype       = (fun _ _ -> false);
  } in
  fst_global_pass empty_contexts globals >>= fun contexts1 ->
  fst_klass_pass contexts1 klasses >>= fun _ ->
  fst_func_pass funcs interfaces >>= fun gamma ->
  Result.all(List.map ~f: (snd_func_pass gamma) funcs) >>= fun func_list ->
  let use_typecheck use =
    match snd use with
    | Use (_, id) -> ((), Use ((), id))
  in
  let use_list = List.map ~f: use_typecheck uses in
  Result.all (List.map ~f:interface_typecheck interfaces) >>= fun interfaces' ->
  (* TODO: MUST CHANGE RIGHT NOW RETURNING EMPTY LIST FOR GLOBALS AND DECLS *)
  Ok (FullProg (name, ((), Prog (use_list, [], [], func_list)), interfaces'))
