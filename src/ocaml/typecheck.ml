module L = List
module U = Util
open Core.Std
open Ast.S
open Graph

(******************************************************************************)
(* Error                                                                      *)
(******************************************************************************)
module Error = struct
  type t = Pos.pos * string
  type 'a result = ('a, t) Result.t
end

let num_f_args         = "Incorrect number of function arguments"
let typ_f_args         = "Ill typed function arguments"
let num_p_args         = "Incorrect number of procedure arguments"
let typ_p_args         = "Ill typed procedure arguments"
let num_ret_args       = "Incorrect number of return expressions"
let typ_ret_args       = "Ill typed return expressions"
let unbound_var x      = sprintf "Unbound variable %s" x
let unbound_call x     = sprintf "Unbound callable %s" x
let dup_global_decl    = "Duplicate global variable declaration"
let dup_var_decl       = "Duplicate variable declaration"
let dup_class_decl     = "Duplicate class declaration"
let inconsist_class c  = sprintf "Class %s declaration is inconsistent with interface files" c
let class_cycle        = "Cyclic class hierarchy"
let dup_field_decl     = "Duplicate field declaration"
let dup_method_decl    = "Duplicate method declaration"
let field_shadow       = "Field declaration shadows global variable"
let method_shadow      = "Method declaration shadows functions"
let field_underscore   = "Field name must be declared"
let bound_var_decl     = "Cannot rebind variable"
let num_decl_vars      = "Incorrect number of variables in declassign"
let typ_decl_vars      = "Ill typed variable declassign"
let dup_func_decl x    = sprintf "Function %s has already been declared" x
let no_return          = "Function is missing return"
let global_und         = "Global variable is underscore"
let non_const_global   = "Global expression is not a constant"
let cyclic_globals     = "Global dependency graph has cycles"
let undeclared_class c = sprintf "class %s not declared" c
let this_var           = "Invalid use of variable \"this\""
let klass_conflict i iname = sprintf "Conflicting declaration for class %s in %s" i iname
let inconsist_method_inter m = sprintf "Method %s declaration is inconsistent with interface files" m
let inconsist_method_super m = sprintf "Method %s declaration is inconsistent with super class" m

(******************************************************************************)
(* Ast                                                                        *)
(******************************************************************************)
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
    | _ -> t1 = t2

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

  let rec subtype t1 t2 =
    if array_subtyping t1 t2 then
      true
    else
      match t1, t2 with
      | _, UnitT -> true
      | TupleT t1s, TupleT t2s -> begin
          match List.zip t1s t2s with
          | Some ts -> List.for_all ts ~f:(fun (t1, t2) -> subtype t1 t2)
          | None -> false
      end
      | _ -> false

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

let is_underscore v =
  match v with
  | AVar (_, AId _) -> false
  | AVar (_, AUnderscore _)
  | Underscore -> true

let varsofvar v =
  match v with
  | AVar (_, AId  ((_, x), _)) -> Some x
  | AVar (_, AUnderscore _)
  | Underscore -> None

let has_underscore vs =
  List.exists (List.map vs ~f:snd) ~f:is_underscore

let varsofvars vs =
  List.map ~f:(fun x -> varsofvar (snd x)) vs
  |> List.filter ~f:is_some
  |> List.map ~f:(function | Some x -> x | None -> failwith "can't happen")

let typeofavar av =
  match av with
  | Ast.S.AId (_, t)
  | Ast.S.AUnderscore t -> Expr.of_typ t

let postypeofavar av =
  match av with
  | Ast.S.AId (_, t)
  | Ast.S.AUnderscore t -> t

let typesofvars vs =
  let f acc (_, v) =
    match v with
    | AVar (_, avar) -> postypeofavar avar :: acc
    | _ -> acc in
  List.fold_left ~f ~init:[] vs

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

let id_of_callable (_, c) =
  match c with
  | Func ((_, i), _, _, _)
  | Proc ((_, i), _, _) -> i

let id_of_callable_decl ((_, c): Pos.callable_decl) =
  match c with
  | FuncDecl ((_, i),_,_)
  | ProcDecl ((_, i),_) -> i

let calldecl_of_call c =
  match c with
  | (p, Func (id, args, rets, _)) -> (p, FuncDecl (id, args, rets))
  | (p, Proc (id, args, _)) -> (p, ProcDecl (id, args))

(******************************************************************************)
(* Contexts                                                                   *)
(******************************************************************************)
module Sigma = struct
  type t =
    | Var of Expr.t
    | Function of Expr.t * Expr.t
    [@@deriving sexp]
end
open Sigma

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

  let bind_all_pos_vars c vs =
    List.fold_left vs ~init:c ~f:(fun c ((_, v): Pos.var) ->
      match v with
      | AVar (_, AId ((_, x), t)) -> bind c x (Var (Expr.of_typ t))
      | AVar (_, AUnderscore _)
      | Underscore -> c
    )

  let bind_all_avars c avs =
    List.fold_left avs ~init:c ~f:(fun c av ->
      match varsofavar (snd av) with
      | Some x -> bind c x (Var (fst av))
      | None -> c
    )

  let bind_all_calls c calls =
    List.fold_left calls ~init:c ~f:(fun c' call ->
      let (args, rets) = typeof_callable (calldecl_of_call call) in
      let id = id_of_callable call in
      bind c' id (Function (args, rets))
    )
end

type contexts = {
  locals           : context;
  delta_m          : KlassM.t String.Map.t;
  delta_i          : KlassM.t String.Map.t;
  class_context    : string option;
  inloop           : bool;
  globals          : String.Set.t;
  typed_globals    : global list;
  subtype          : Expr.t -> Expr.t -> bool;
}

let empty_contexts = {
  locals           = Context.empty;
  delta_m          = String.Map.empty;
  delta_i          = String.Map.empty;
  class_context    = None;
  inloop           = false;
  globals          = String.Set.empty;
  typed_globals    = [];
  subtype          = Expr.subtype;
}

type typecheck_info = {
  prog  : full_prog;
  ctxts : contexts;
}

(******************************************************************************)
(* Helpers                                                                    *)
(******************************************************************************)
(* Ok and Error constructors are defined in Core.Std. If we open Result, we get
 * shadowed constructor warnings. We manually "open" map and bind to avoid the
 * warnings. *)
let (>>=) = Result.(>>=)
let (>>|) = Result.(>>|)


let get_klass_info (c: contexts) (typ, _) pos : KlassM.t Error.result =
  match typ with
  | KlassT classname -> begin
      match String.Map.find c.delta_m classname with
      | None -> Error (pos, undeclared_class classname)
      | Some kl_info -> Ok kl_info
  end
  | _ -> Error (pos, "Not an object expression")

let rec find_callable (ctxs: contexts) (klass_info : KlassM.t) (name : string) =
  let f (_, c) =
    match c with
    | FuncDecl ((_, fname), _, _)
    | ProcDecl ((_, fname), _) -> fname = name
  in
  match List.find ~f klass_info.methods with
  | Some callable -> Some callable
  | None ->
    begin
      match klass_info.super with
      | Some s ->
          let merged_deltas = U.disjoint_merge ctxs.delta_m ctxs.delta_i in
          let super_info = String.Map.find_exn merged_deltas s in
          find_callable ctxs super_info name
      | None -> None
    end

let methods ~delta_m ~delta_i c =
  let delta = U.disjoint_merge delta_m delta_i in
  if not (String.Map.mem delta c) then
    failwith (sprintf "methods: class %s not in delta" c)
  else
    let rec help c =
      let {super; methods; _} = String.Map.find_exn delta c in
      let methods = List.map methods ~f:id_of_callable_decl in
      match super with
      | Some s -> (help s) @ (""::methods)
      | None -> (""::methods)
    in
    help c

let super_methods ~delta_m ~delta_i c =
  let delta = U.disjoint_merge delta_m delta_i in
  let info = String.Map.find_exn delta c in
  let all_methods = methods ~delta_m ~delta_i c in
  List.take all_methods (List.length all_methods - 1 - (List.length info.methods))

let id_of_klass (_, Klass ((_,i), _, _, _)) = i

let _id_of_klassdecl (_, KlassDecl ((_,i),  _, _)) = i

let klassdecl_of_klassm k =
  let dummy = (-1,-1) in
  match k.super with
  | Some s -> (dummy, KlassDecl ((dummy, k.name), Some (dummy, s), (k.methods@k.overrides)))
  | None -> (dummy, KlassDecl ((dummy, k.name), None, (k.methods@k.overrides)))

let klassdecl_of_klass (p, Klass (id, super, _, methods)) =
  (p, KlassDecl (id, super, (List.map ~f:calldecl_of_call methods)))

let super_of_klass (_, Klass (_, s, _, _)) = s

let _super_of_klassdecl (_, KlassDecl (_, s, _)) = s

let flatten_snd_opt_list l =
  let f (e1, e2) acc =
    match e2 with
    | Some x -> (e1, x)::acc
    | None -> acc
  in
  List.fold_right ~f ~init:[] l

let flatten_opt_list (l : ('a option) list) : 'a list =
  let f e acc =
    match e with
    | Some x -> x::acc
    | None -> acc in
  List.fold_right ~f ~init:[] l

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

and expr_call_helper (c : contexts) p argtype rettype args : expr list Error.result =
  match argtype, rettype with
  | _, UnitT -> Error (p, "Using proc call as an expr")
  | TupleT tl, _ -> exprs_typecheck p c tl args num_f_args typ_f_args
  | (IntT|BoolT|ArrayT _|KlassT _), _ ->
    exprs_typecheck p c [argtype] args num_f_args typ_f_args
  | UnitT, _ -> exprs_typecheck p c [] args num_f_args typ_f_args
  | EmptyArray, _ -> failwith "expr_call_helper: EmptyArray as argument type"
  | NullT, _ -> failwith "expr_call_helper: NullT as argument type"

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
      (* array concat *)
      | _, _, PLUS when array_subtyping EmptyArray lt && array_subtyping EmptyArray rt ->
        type_max array_subtyping p lt rt >>= fun max_t -> Ok (max_t, e)
      (* object/array equality *)
      | _, _, (EQEQ|NEQ) when comparable c.subtype lt rt -> Ok (BoolT, e)
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
      expr_call_helper c p argtype rettype args >>= fun args' ->
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
      match List.Assoc.find klass_info.fields fname with
      | None ->
          Error (p, sprintf "Class %s does not have field %s" klass_info.name fname)
      | Some typ ->
          Ok (of_typ typ, FieldAccess (receiver', ((), fname)))
  end
  | MethodCall (receiver, (_, mname), args) -> begin
      expr_typecheck c receiver >>= fun receiver' ->
      get_klass_info c receiver' p >>= fun klass_info ->
      match find_callable c klass_info mname with
      | None ->
          Error (p, sprintf "Class %s does not have method %s" klass_info.name mname)
      | Some callable ->
          let argtype, rettype = typeof_callable callable in
          expr_call_helper c p argtype rettype args >>= fun args' ->
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
  | TKlass (_, name) ->
      if String.Map.mem c.delta_m name || String.Map.mem c.delta_i name then
        Ok (KlassT name, TKlass ((), name))
      else
        Error (p, sprintf "class %s not defined" name)

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

(* lvalue determines expressions that are assignable *)
let lvalue (c : contexts) ((p, e) : Pos.expr) : expr Error.result =
  expr_typecheck c (p, e) >>= fun typed_e ->
  match typed_e with
  | (_, Id (_, id)) when id <> "this" -> Ok typed_e
  | (_, Index _)
  | (_, FieldAccess _) -> Ok typed_e
  | _ -> Error (p, "Trying to assign to invalid expr")

let stmt_call_helper (c : contexts) p argtype rettype args : expr list Error.result =
  match argtype, rettype with
  | TupleT tl, UnitT -> exprs_typecheck p c tl args num_f_args typ_f_args
  | (IntT|BoolT|ArrayT _|KlassT _), UnitT ->
      exprs_typecheck p c [argtype] args num_f_args typ_f_args
  | UnitT, UnitT ->
      exprs_typecheck p c [] args num_f_args typ_f_args
  | EmptyArray, _ -> failwith "stmt_call_helper: EmptyArray as argument type"
  | NullT, _ -> failwith "stmt_call_helper: NullT as argument type"
  | _, _ -> Error (p, "method call stmt returns a value")

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
        if List.for_all (U.init ss) ~f:(fun (t, _) -> t = One)
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
        ({c with inloop = true}, rho) |- s >>= fun (s', _) ->
        match fst b'  with
        | BoolT -> Ok ((One, While (b', s')), c)
        | _ -> err "While conditional not a boolean."
      end
    | ProcCall ((_, f), args) -> begin
        Context.func p c.locals f >>= fun (a, b) ->
        match (a, b), args with
        (* no args *)
        | (UnitT, _), _::_ -> Error (p, "Giving args to a proc with no params")
        | (UnitT, UnitT), [] -> Ok ((One, ProcCall (((), f), [])), c)
        (* multiple args *)
        | (TupleT arg_types, UnitT), _::_::_ ->
          exprs_typecheck p c arg_types args num_p_args typ_p_args >>= fun args' ->
          Ok ((One, ProcCall (((), f), args')), c)
        (* single args *)
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
    | Asgn (l, r) ->
        lvalue c l >>= fun l' ->
        expr_typecheck c r >>= fun r' ->
        if fst r' <= fst l'
        then Ok ((One, Asgn (l', r')), c)
        else
          let ls = Expr.to_string (fst l') in
          let rs = Expr.to_string (fst r') in
          err (sprintf "Cannot assign type %s to type %s" rs ls)
    | Decl vs -> begin
        if List.mem (varsofvars vs) "this" then
          err "cannot declare variable with name 'this'"
        else
          vars_typecheck p c vs dup_var_decl bound_var_decl >>= fun vs' ->
          Ok ((One, Decl vs'), {c with locals = Context.bind_all_vars c.locals vs'})
      end
    | DeclAsgn (vs, e) -> begin
        if List.mem (varsofvars vs) "this" then
          err "cannot declare variable with name 'this'"
        else
          List.map ~f:(typ_typecheck c) (typesofvars vs) |> Result.all >>= fun _ ->
          vars_typecheck p c vs dup_var_decl bound_var_decl >>= fun vs' ->
          expr_typecheck c e >>= fun e' ->
          match vs', fst e' with
          | _, TupleT ets' ->
            let vts' = List.map ~f:fst vs' in
            Expr.eqs c.subtype p vts' ets' num_decl_vars typ_decl_vars >>= fun () ->
            Ok ((One, DeclAsgn (vs', e')), {c with locals = Context.bind_all_vars c.locals vs'})
          | [v'], _ ->
            Expr.eqs c.subtype p [fst v'] [fst e'] num_decl_vars typ_decl_vars >>= fun () ->
            Ok ((One, DeclAsgn ([v'], e')), {c with locals = Context.bind_all_vars c.locals vs'})
          | _, _ -> err "Invalid declassign"
      end
    | MethodCallStmt (receiver, (_, mname), args) -> begin
        expr_typecheck c receiver >>= fun receiver' ->
        get_klass_info c receiver' p >>= fun klass_info ->
        match find_callable c klass_info mname with
        | None ->
            Error (p, sprintf "Class %s does not have method %s" klass_info.name mname)
        | Some callable -> begin
            let argtype, rettype = typeof_callable callable in
            stmt_call_helper c p argtype rettype args >>= fun args' ->
            Ok ((One, MethodCallStmt (receiver', ((), mname), args')), c)
        end
    end
    | Break ->
        if c.inloop then Ok ((Zero, Break), c)
        else err "break not within loop"
  in

  (c, rho) |- s >>| fst

let global_typecheck (contexts: contexts) ((p, g): Pos.global) : global Error.result =
  match g with
  | Gdecl [(_, AVar (_, AId ((_, id), typ)))] ->
      typ_typecheck contexts typ >>= fun typ' ->
      Ok ((), Gdecl [(fst typ', AVar (fst typ', AId (((), id), typ')))])
  | GdeclAsgn ([(_, AVar (_, AId ((_, id), typ)))], e) ->
    typ_typecheck contexts typ >>= fun typ' ->
    expr_typecheck contexts e >>= fun e' ->
    let (<=) = contexts.subtype in
    Expr.eqs (<=) p [fst typ'] [fst e'] num_decl_vars typ_decl_vars >>= fun () ->
    Ok ((), GdeclAsgn ([(fst typ', AVar (fst typ', AId (((), id), typ')))], e'))
  | _ -> failwith "assertion: global_typecheck ill-formed globals"

(******************************************************************************)
(* callables                                                                  *)
(******************************************************************************)
let avar_to_expr_t ((_, av): Pos.avar) : Expr.t =
  match av with
  | AId (_, typ) -> Expr.of_typ typ
  | AUnderscore typ -> Expr.of_typ typ

let shrink_types (ts: Expr.t list) : Expr.t =
  match ts with
  | [] -> UnitT
  | [IntT as t]
  | [BoolT as t]
  | [ArrayT _ as t]
  | [KlassT _ as t] -> t
  | [TupleT _]
  | [UnitT]
  | [EmptyArray]
  | [NullT] -> failwith "shrink_types: invalid function argument"
  | _::_ -> TupleT ts

(* makes*)
let func_decl_typecheck (c: contexts) ((p, call): Pos.callable_decl) =
  let id = id_of_callable_decl (p, call) in
  if Context.mem c.locals id then
    Error (p, dup_func_decl id)
  else
    match call with
    | FuncDecl (_, args, rets) -> begin
      match args, rets with
      | _, [] -> Error (p, "Function declaration must have return type")
      | _, (_::_) ->
        (* args *)
        avars_typecheck p c args dup_var_decl bound_var_decl >>= fun avars ->
        let raw_avars = List.map ~f:snd avars in
        let arg_type = shrink_types (List.map ~f:typeofavar raw_avars) in

        (* rets *)
        Result.all (List.map ~f:(typ_typecheck c) rets) >>= fun rets ->
        let ret_type = shrink_types (List.map ~f:fst rets) in

        Ok (Function (arg_type, ret_type))
    end
    | ProcDecl (_, args) ->
        (* args *)
        avars_typecheck p c args dup_var_decl bound_var_decl >>= fun avars ->
        let raw_avars = List.map ~f:snd avars in
        let arg_type = shrink_types (List.map ~f:typeofavar raw_avars) in

        Ok (Function (arg_type, UnitT))

let func_typecheck c (p, call) =
  match call with
  | Func (i, args, rets, _) -> func_decl_typecheck c (p, FuncDecl (i, args, rets))
  | Proc (i, args, _)       -> func_decl_typecheck c (p, ProcDecl (i, args))

let fst_func_pass contexts prog_funcs interfaces =
  (* Put each decl into its own context. This allows two decls, either in the
   * same interface or across interfaces, to have the same name. *)
  let decls = List.concat_map interfaces ~f:(fun (_, Interface (_,_,_,l)) -> l) in
  let decl_contexts = List.map decls ~f:(fun d ->
    func_decl_typecheck contexts d >>= fun sigma ->
    let id = id_of_callable_decl d in
    Ok {contexts with locals=Context.bind contexts.locals id sigma}
  ) in
  Result.all decl_contexts >>= fun decl_contexts ->

  (* Put funcs into a single context. This disallows functions from being
   * defined twice within a module. *)
  let prog_context = List.fold_left prog_funcs ~init:(Ok contexts) ~f:(fun c f ->
    c >>= fun c ->
    func_typecheck c f >>= fun sigma ->
    let id = id_of_callable f in
    Ok {c with locals=Context.bind c.locals id sigma}
  ) in
  prog_context >>= fun prog_context ->

  (* Merge two gammas toghether, erroring out if two functions with the same
   * name have different types. *)
  let merge (a: context) (b: context) : context Error.result =
    let merged = Context.merge a b ~f:(fun ~key x ->
      ignore key;
      match x with
      | `Left v
      | `Right v -> Some v
      | `Both (v1, v2) -> if v1 = v2 then Some v1 else None
    ) in
    let all_ids = String.Set.of_list ((Context.keys a) @ (Context.keys b)) in
    let merged_ids = String.Set.of_list (Context.keys merged) in
    let dropped_ids = String.Set.diff all_ids merged_ids in
    if String.Set.count ~f:(fun _ -> true) dropped_ids = 0 then
      Ok merged
    else
      let dups = U.string_of_set ~short:true dropped_ids ~f:(fun x -> x) in
      Error ((-1, -1), sprintf "these funcs declared multiple times: %s" dups)
  in

  List.fold_left (prog_context::decl_contexts) ~init:(Ok contexts) ~f:(fun a c ->
    a >>= fun a ->
    merge a.locals c.locals >>= fun locals ->
    Ok {a with locals}
  )

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
type inter_ctx = {
  name        : string;
  uses        : String.Set.t;
  class_decls : Pos.klass_decl String.Map.t;
  func_decls  : Pos.callable_decl String.Map.t;
}

let kdecl_to_klassm (_, KlassDecl ((_, name), super, fdecls)) : KlassM.t =
  let super' =
    match super with
    | None -> None
    | Some (_, id) -> Some id
  in
  { name;
    super = super';
    fields = [];
    methods = fdecls;
    overrides = []; }

let klass_decl_map (klass_decls : Pos.klass_decl list) : KlassM.t String.Map.t =
  let f acc kd =
    let (_, KlassDecl ((_, key), _, _)) = kd in
    String.Map.add acc ~key ~data:(kdecl_to_klassm kd) in
  List.fold_left ~f ~init:String.Map.empty klass_decls

let ctx_of_interface ((p, interface): Pos.interface) : inter_ctx Error.result =
  let Interface (name, uses, cdecls, fdecls) = interface in
  (* misc. helpers *)
  let dup_func fname =
    sprintf "function %s declared multiple times in interface %s" fname name in
  let dup_class cname =
    sprintf "class %s declared multiple times in interface %s" cname name in
  let func_decls =
    let f fdecl =
      match fdecl with
      | (_, FuncDecl ((_, id), _, _))
      | (_, ProcDecl ((_, id), _)) -> (id, fdecl) in
    List.map ~f fdecls |> String.Map.of_alist |> function
      | `Ok map -> Ok map
      | `Duplicate_key k -> Error (p, dup_func k) in
  let class_decls =
    let f cdecl =
      let (_, (KlassDecl ((_, id), _, _))) = cdecl in
      (id, cdecl) in
    List.map ~f cdecls |> String.Map.of_alist |> function
      | `Ok map -> Ok map
      | `Duplicate_key k -> Error (p, dup_class k) in
  func_decls >>= fun func_decls' ->
  class_decls >>= fun class_decls' ->
  Ok {
    name;
    uses = List.map ~f:(fun (_, Use (_, id)) -> id) uses |> String.Set.of_list;
    class_decls = class_decls';
    func_decls = func_decls';
  }

(* see klassdecl_ok *)
let call_decl_typecheck (c : contexts) (p, func_decl) : callable_decl Error.result =
  match func_decl with
  | FuncDecl ((_, id), avars, typs) ->
    avars_typecheck p c avars dup_var_decl bound_var_decl >>= fun avars' ->
    List.map ~f:(typ_typecheck c) typs |> Result.all >>= fun typs' ->
    Ok (typeof_callable (p, func_decl), FuncDecl (((), id), avars', typs'))
  | ProcDecl ((_, id), avars) ->
    avars_typecheck p c avars dup_var_decl bound_var_decl >>= fun avars' ->
    Ok (typeof_callable (p, func_decl), ProcDecl (((), id), avars'))

(* check that all classes used by the decl is in the given set *)
let klassdecl_typecheck (c : contexts) kdecl : klass_decl Error.result =
  let (p, KlassDecl ((_, id), super, fdecls)) = kdecl in
  let super_ok =
    match super with
    | None -> Ok None
    | Some (_, id) ->
        if String.Map.mem c.delta_m id || String.Map.mem c.delta_i id then
          Ok (Some ((), id))
        else Error (p, undeclared_class id) in
  super_ok >>= fun super' ->
  List.map ~f:(call_decl_typecheck c) fdecls |> Result.all >>= fun fdecls' ->
  Ok ((), KlassDecl (((), id), super', fdecls'))

let contexts_of_interface (ctxmap : inter_ctx String.Map.t) (interface : Pos.interface) =
  let (_, Interface (_, uses, kdecls, _)) = interface in
  let neighbors = List.map ~f:(fun (_, Use (_, id)) -> id) uses in
  let neighbor_kdecls =
    let f acc neighbor =
      match String.Map.find ctxmap neighbor with
      | None -> acc
      | Some { class_decls; _ } ->
          String.Map.data class_decls @ acc in
    List.dedup (List.fold_left ~f ~init:[] neighbors) in
  { empty_contexts with
    delta_m = klass_decl_map kdecls;
    delta_i = klass_decl_map neighbor_kdecls; }

(* make sure that the types used by the interface are available *)
let interface_typecheck (c : contexts) (interface : Pos.interface) : interface Error.result =
  let (_, Interface (name, uses, kdecls, fdecls)) = interface in
  List.map ~f:(klassdecl_typecheck c) kdecls |> Result.all >>= fun kdecls' ->
  List.map ~f:(call_decl_typecheck c) fdecls |> Result.all >>= fun fdecls' ->
  let uses' = List.map ~f:(fun (_, Use (_, id)) -> ((), Use ((), id))) uses in
  Ok ((), Interface (name, uses', kdecls', fdecls'))

let interfaces_typecheck (prog_uses : Pos.use list) (interfaces: Pos.interface list) =
  (* set up of contexts *)
  List.map ~f:ctx_of_interface interfaces |> Result.all >>= fun inter_ctxs ->
  let name_ctx_pairs = List.map ~f:(fun ctx -> (ctx.name, ctx)) inter_ctxs in
  let raw_ctx_map =
    match String.Map.of_alist name_ctx_pairs with
    | `Ok map -> Ok map
    | `Duplicate_key k ->
        Error ((-1, -1), sprintf "duplicate interface %s" k) in
  raw_ctx_map >>= fun ctx_map ->

  (* first phase: all types used by an interface exist in neighbors *)
  let f interface =
    let c = contexts_of_interface ctx_map interface in
    interface_typecheck c interface in
  List.map ~f interfaces |> Result.all >>= fun interfaces' ->

  (* second phase: all decl of a class, in any interface, must be the same
   * also build the class_decl_index map on the way *)
  let class_dup_f acc (_, Interface (intname, _, kdecls, _)) =
    acc >>= fun classmap ->
    let f mapacc kdecl =
      mapacc >>= fun mapacc' ->
      let (_, KlassDecl ((_, id), _, _)) = kdecl in
      match String.Map.find mapacc' id with
      | None -> Ok (String.Map.add mapacc' ~key:id ~data:kdecl)
      | Some kdecl2 ->
          if kdecl = kdecl2 then
            Ok (String.Map.add mapacc' ~key:id ~data:kdecl2)
          else Error ((-1, -1), klass_conflict id intname) in
    List.fold_left ~f ~init:(Ok classmap) kdecls in
  List.fold_left ~f:class_dup_f ~init:(Ok String.Map.empty) interfaces' >>= fun _ ->

  (* final pass: build delta_i from given ctx map, prog uses *)
  let kname_km_pairs =
    let f kdecl =
      let (_, KlassDecl ((_, name), _, _)) = kdecl in
      (name, kdecl_to_klassm kdecl) in
    let use_strs = List.map ~f:(fun (_, Use (_, id)) -> id) prog_uses in
    String.Map.filter_keys ~f:(List.mem use_strs) ctx_map |>
    String.Map.map ~f:(fun c -> String.Map.data c.class_decls) |>
    String.Map.data |>
    List.concat |>
    List.map ~f in

  Ok (interfaces', {
    empty_contexts with
    delta_i = String.Map.of_alist_exn kname_km_pairs;
  })

(******************************************************************************)
(* globals                                                                    *)
(******************************************************************************)
module GlobalVertex = struct
  type t = Pos.global
  let compare = Pervasives.compare
  let hash    = Hashtbl.hash
  let equal   = (=)
end
module GlobalGraph = Graph.Persistent.Digraph.Concrete(GlobalVertex)

let global_graph globals =

  let rec vars_of_expr (_, expr) vars =
    match expr with
    | Null | Int _ | Bool _ | String _ | Char _
    | FuncCall _ | New _ | FieldAccess _ | MethodCall _ -> vars
    | Array es -> List.fold_left ~init:vars es
        ~f:(fun acc e -> String.Set.union (vars_of_expr e String.Set.empty) acc)
    | Id (_, x) -> String.Set.add vars x
    | BinOp (lhs, _, rhs) -> String.Set.union (vars_of_expr lhs vars) (vars_of_expr rhs vars)
    | UnOp (_, e) -> vars_of_expr e vars
    | Index (a, i) -> String.Set.union (vars_of_expr a vars) (vars_of_expr i vars)
    | Length e -> vars_of_expr e vars
  in

  let rec vars_of_type (_, t) vars =
    match t with
    | TInt
    | TBool
    | TKlass _
    | TArray (_, None) -> vars
    | TArray (t', Some e) -> vars_of_type t' (vars_of_expr e vars)
  in

  let global_alist = List.map globals ~f:(fun g ->
    match g with
    | (_, Gdecl [(_, AVar (_, AId ((_, id), _)))])
    | (_, GdeclAsgn ([(_, AVar (_, AId ((_, id), _)))], _)) -> (id, g)
    | _ -> failwith "assertion: global_graph got ill-formed globals"
  ) in
  let global_index = String.Map.of_alist_exn global_alist in

  List.fold_left globals ~init:GlobalGraph.empty ~f:(fun g global ->
    let g = GlobalGraph.add_vertex g global in

    match global with
    | (_, GdeclAsgn ([(_, AVar (_, AId ((_, _), t)))], e)) ->
        let e_vars = vars_of_expr e String.Set.empty in
        let t_vars = vars_of_type t String.Set.empty in
        let g' = String.Set.fold e_vars ~init:g ~f:(fun g' var ->
          GlobalGraph.add_edge g' (String.Map.find_exn global_index var) global)
        in
        String.Set.fold t_vars ~init:g' ~f:(fun g'' var ->
          GlobalGraph.add_edge g'' (String.Map.find_exn global_index var) global)
    | (_, Gdecl [(_, AVar (_, AId ((_, _), t)))]) ->
        let t_vars = vars_of_type t String.Set.empty in
        String.Set.fold t_vars ~init:g ~f:(fun g' var ->
          GlobalGraph.add_edge g' (String.Map.find_exn global_index var) global)
    | _ -> failwith "impossible: global_graph"
  )

let global_pass contexts globals =
  let check b s g =
    if b
      then Ok g
      else Error ((-1, -1), s)
  in

  (* (1) *)
  let no_underscores = List.for_all globals ~f:(fun (_, g) ->
    match g with
    | Gdecl vs
    | GdeclAsgn (vs, _) -> not (has_underscore vs)
  ) in
  check no_underscores global_und globals >>= fun globals ->

  (* (2) *)
  let ids = List.concat_map globals ~f:(fun (_, g) ->
    match g with
    | Gdecl vs
    | GdeclAsgn (vs, _) -> varsofvars vs
  ) in
  let no_duplicates = not (List.contains_dup ids) in
  let global_ids = String.Set.of_list ids in
  check no_duplicates dup_global_decl globals >>= fun globals ->

  (* (3) *)
  let no_this = not (String.Set.mem global_ids "this") in
  check no_this this_var globals >>= fun globals ->

  (* (4) *)
  let rec expr_ok (_, e) =
    let cog = expr_ok in
    match e with
    | Null | Int _ | Bool _ | String _ | Char _ -> true
    | FuncCall _ | New _ | FieldAccess _ | MethodCall _ -> false
    | Array es -> List.for_all es ~f:cog
    | Id (_, x) -> String.Set.mem global_ids x
    | BinOp (lhs, _, rhs) -> cog lhs && cog rhs
    | UnOp (_, e) -> cog e
    | Index (a, i) -> cog a && cog i
    | Length e -> cog e
  in

  let rec type_ok (_, t) =
    match t with
    | TInt
    | TBool
    | TKlass _ -> true
    | TArray (t, None) -> type_ok t
    | TArray (t, Some e) -> type_ok t && expr_ok e
  in

  let var_ok ((_, v): Pos.var) =
    match v with
    | AVar (_, AId (_, t)) -> type_ok t
    | AVar (_, AUnderscore _) -> failwith "assertion: var_ok AUnderscore"
    | Underscore  -> failwith "assertion: var_ok Underscore"
  in

  let globals_ok = List.for_all globals ~f:(fun (_, g) ->
    match g with
    | Gdecl vs -> List.for_all vs ~f:var_ok
    | GdeclAsgn (vs, e) -> List.for_all vs ~f:var_ok && expr_ok e
  ) in
  check globals_ok non_const_global globals >>= fun _ ->

  (* flatten globals *)
  let globals = List.concat_map globals ~f:(fun (p, g) ->
    match g with
    | Gdecl vs -> List.map vs ~f:(fun v -> (p, Gdecl [v]))
    | GdeclAsgn (vs, e) -> List.map vs ~f:(fun v -> (p, GdeclAsgn ([v], e)))
  ) in

  (* (5) *)
  let ggraph = global_graph globals in
  let module GlobalTraverse = Graph.Traverse.Dfs(GlobalGraph) in
  let no_cycles = not (GlobalTraverse.has_cycle ggraph) in
  check no_cycles cyclic_globals globals >>= fun _ ->

  (* topo sort globals *)
  let module GlobalTopo = Graph.Topological.Make(GlobalGraph) in
  let globals = List.rev (GlobalTopo.fold (fun g a -> g::a) ggraph []) in

  (* update gamma *)
  let gamma' = List.fold_left globals ~init:contexts.locals ~f:(fun c (_, g) ->
    match g with
    | Gdecl vs
    | GdeclAsgn (vs, _) -> (Context.bind_all_pos_vars c vs)
  ) in
  let contexts = {contexts with locals=gamma'} in

  (* (6) *)
  let typed_globals = List.map globals ~f:(fun g -> global_typecheck contexts g) in
  Result.all typed_globals >>= fun typed_globals ->

  Ok {contexts with globals=global_ids; typed_globals}

(******************************************************************************)
(* klasses                                                                    *)
(******************************************************************************)
module KlassVertex = struct
  type t = Pos.klass_decl
  let compare = Pervasives.compare
  let hash    = Hashtbl.hash
  let equal   = (=)
end

module KlassGraph = Graph.Persistent.Digraph.Concrete(KlassVertex)

let class_graph klasses =
  let klass_alist = List.map klasses ~f:(fun k ->
    match k with
    | (_, KlassDecl ((_, name), _, _)) -> (name, k)
  ) in
  let klass_index = String.Map.of_alist_exn klass_alist in

  List.fold_left klasses ~init:KlassGraph.empty ~f:(fun g k ->
    let g = KlassGraph.add_vertex g k in

    let super =
      match k with
      | (_, KlassDecl (_, super, _)) -> super
    in
    match super with
    | Some (_, s) -> KlassGraph.add_edge g (String.Map.find_exn klass_index s) k
    | None -> g
  )

let is_alist_ok as1 as2 =
  let f acc (_, e1) (_, e2) =
    match e1, e2 with
    | AUnderscore t1, AUnderscore t2
    | AId (_, t1), AId (_, t2)
    | AUnderscore t1, AId (_, t2)
    | AId (_, t1), AUnderscore t2 -> acc && t1 = t2
  in
  List.fold2_exn ~f ~init:true as1 as2

let is_typlist_ok ts1 ts2 =
  let f acc t1 t2 = acc && t1 = t2 in
  List.fold2_exn ~f ~init:true ts1 ts2

let call_decls_type_ok p1 p2 =
  match p1, p2 with
  | ProcDecl (_, alist1), ProcDecl (_, alist2) ->
    begin
      try
        is_alist_ok alist1 alist2
      with _ -> false
    end
  | FuncDecl (_, alist1, typlist1), FuncDecl (_, alist2, typlist2) ->
    begin
      try
        (is_alist_ok alist1 alist2) && (is_typlist_ok typlist1 typlist2)
      with _ -> false
    end
  | _ -> false

let _calls_type_ok p1 p2 =
  match p1, p2 with
  | Proc (_, alist1, _), Proc (_, alist2, _) ->
    begin
      try
        is_alist_ok alist1 alist2
      with _ -> false
    end
  | Func (_, alist1, typlist1, _), Func (_, alist2, typlist2, _) ->
    begin
      try
        (is_alist_ok alist1 alist2) && (is_typlist_ok typlist1 typlist2)
      with _ -> false
    end
  | _ -> false

let method_def_ok ((_, c1) as c1') ((_, c2) as c2') =
  let id1 = id_of_callable_decl c1' in
  let id2 = id_of_callable_decl c2' in
  (id1 = id2) && call_decls_type_ok c1 c2

let fst_klass_pass contexts klasses =
  let module SortedGraph = Topological.Make(KlassGraph) in
  let module DfsGraph = Traverse.Dfs(KlassGraph) in
  let open Core_kernel in

  let klass_names = List.map ~f:id_of_klass klasses in
  let klass_to_field = List.map ~f:(fun (_, Klass ((_,i), _, f,_)) -> (i,f)) klasses in
  let klass_to_super = List.map ~f:(fun x -> (id_of_klass x, super_of_klass x)) klasses
                       |> flatten_snd_opt_list
                       |> List.map ~f: (fun (x1, (_, x2)) -> (x1, x2))
  in
  let klass_supers = List.map ~f:snd klass_to_super in
  let kdecls_of_klasses = List.map ~f:klassdecl_of_klass klasses in

  let klassm_of_inters = String.Map.data contexts.delta_i in
  let kdecls_of_inters = List.map ~f:klassdecl_of_klassm klassm_of_inters in
  let f acc (k_decl: KlassM.t) =
    (k_decl.name, k_decl.super)::acc
  in
  let k_decl_to_super = List.fold_left ~f ~init:[] klassm_of_inters
                        |> flatten_snd_opt_list
  in
  let (k_decl_names, k_decl_supers) = L.split k_decl_to_super in

  let names = klass_names @ k_decl_names in
  let supers = klass_supers @ k_decl_supers in
  let names_to_supers = klass_to_super @ k_decl_to_super in

  let invalid_super = List.findi ~f:(fun _ e -> not (L.exists (fun e' -> e' = e) names)) supers in

  if is_some invalid_super then Error ((-1, -1), undeclared_class (snd (Option.value_exn invalid_super)))
  else if List.contains_dup klass_names then Error ((-1, -1), dup_class_decl)
  else
    let entire_klass_graph = class_graph (kdecls_of_klasses @ kdecls_of_inters) in
    if DfsGraph.has_cycle entire_klass_graph then Error ((-1, -1), class_cycle)
    else
      let klass_graph = class_graph kdecls_of_klasses in
      let klass_fold (p, KlassDecl ((_,k), super, methods)) acc =
        acc >>= fun contexts' ->
        let get_field (p', f) acc =
          acc >>= fun field_acc ->
          match f with
            | AUnderscore _ -> Error (p', field_underscore)
            | AId ((_, id), t) ->
                if id = "this" then Error (p', this_var)
                else if List.exists ~f:(fun (x, _) -> x = id) field_acc then Error (p', dup_field_decl)
                else
                  Ok ((id, t)::field_acc)
        in
        let fields = L.assoc k klass_to_field in
        List.fold_right ~f:get_field ~init:(Ok []) fields >>= fun fields' ->
        let method_names = List.map ~f:id_of_callable_decl methods in
        if List.contains_dup method_names then Error (p, dup_method_decl)
        else
          let check_inter () =
            let method_decls_match methods1 methods2 =
              let f ((p, _) as e1) e2 acc =
                acc >>= fun acc' ->
                if method_def_ok e1 e2 then
                  Ok (e1::acc')
                else
                  Error (p, inconsist_method_inter (id_of_callable_decl e1))
              in
              begin
                try
                  L.fold_right2 f methods1 methods2 (Ok [])
                with _ -> Error (p, inconsist_class k)
              end
            in
            match String.Map.find contexts.delta_i k with
            | Some def ->
              begin
                match super, def.super with
                | None, None -> method_decls_match methods def.methods
                | Some (_, s), Some s' when s = s' -> method_decls_match methods def.methods
                | _ ->  Error (p, inconsist_class k)
              end
            | None -> Ok methods
          in
          let check_super methods1 =
            match super with
            | None -> Ok ({name = k;
                           super = None;
                           fields = fields';
                           methods=methods1;
                           overrides = []})
            | Some (_, s) ->
              let find_method_def m_name methods =
                let f _ e =
                  match e with
                  | (_, FuncDecl ((_, i'), _, _))
                  | (_, ProcDecl ((_, i'), _)) -> m_name = i'
                in
                List.findi ~f methods
              in
              let rec check_override super ((p, _) as m1) acc =
                acc >>= fun (m, o) ->
                match super with
                | None -> Ok (m1::m, o)
                | Some s ->
                  begin
                    let super_ctx = String.Map.find_exn contexts'.delta_m s in
                    let m_name = id_of_callable_decl m1 in
                    match find_method_def m_name super_ctx.methods with
                    | Some (_, m2) ->
                      if method_def_ok m1 m2 then Ok (m, m1::o)
                      else
                        Error (p, inconsist_method_super m_name)
                    | None -> check_override super_ctx.super m1 acc
                  end
              in
              List.fold_right ~f:(check_override (Some s))
                              ~init:(Ok ([],[]))
                              methods1
              >>= fun (m, o) ->
                Ok ({name = k;
                     super = Some s;
                     fields = fields';
                     methods = m;
                     overrides = o})
          in
          check_inter () >>= fun methods1 ->
          check_super methods1 >>= fun klass' ->
          let delta_m' = String.Map.add contexts'.delta_m ~key:k ~data:klass' in
          Ok ({contexts' with delta_m = delta_m'})
      in
      SortedGraph.fold klass_fold klass_graph (Ok contexts) >>= fun contexts' ->
      let class_super_map = String.Map.of_alist_exn names_to_supers in
      let subtype_rel = make_subtype_rel class_super_map in
      Ok ({contexts' with subtype = subtype_rel})

let snd_klass_pass (c: contexts) (klasses : Pos.klass list) : (klass list) Error.result =
  let klass_map (p, Klass ((_, name), super, fields, methods)) =
    let name' = ((), name) in
    let super' =
      match super with
      | Some (_, id) -> Some ((), id)
      | None -> None
    in
    (* typecheck fields, ensure not shadowed by globals *)
    let fields_ok =
      let field_names =
        List.map ~f:(Fn.compose varsofavar snd) fields |>
        flatten_opt_list
      in
      let field_shadowed =
        List.exists ~f:(fun x -> String.Set.mem c.globals x) field_names
      in
      if field_shadowed then
        Error (p, field_shadow)
      else
        avars_typecheck p c fields dup_var_decl bound_var_decl
    in
    (* typecheck method, ensure not shadowed by globals *)
    let methods_types_ok = Ok (List.map ~f:(fun x -> func_typecheck c x) methods) in

    (* third phase: need to check if methods shadow functions in second pass *)
    let methods_ok fields'=
      let f ~key ~data acc =
        match data with
        | Var _ -> acc
        | Function _ -> key::acc
      in
      let functions = String.Map.fold ~init:[] ~f c.locals in
      let method_shadowed =
        List.exists ~f:(fun x -> List.mem functions (id_of_callable x)) methods
      in
      if method_shadowed then Error (p, method_shadow)
      else
        let locals1 = Context.bind_all_avars c.locals fields' in
        let locals2 = Context.bind_all_calls locals1 methods in
        let locals3 = Context.bind locals2 "this" (Var (KlassT name)) in
        let c' = {c with locals = locals3} in
        Result.all (List.map ~f:(snd_func_pass c') methods)
    in
    fields_ok >>= fun fields' ->
    methods_types_ok >>= fun _ ->
    methods_ok fields' >>= fun methods' ->
    Ok ((), Klass (name', super', fields', methods'))
  in
  Result.all (List.map ~f:klass_map klasses)

(******************************************************************************)
(* prog                                                                       *)
(******************************************************************************)
let prog_typecheck p =
  let FullProg (name, (_, Prog(uses, globals, klasses, funcs)), interfaces) = p in
  let use_typecheck use =
    match snd use with
    | Use (_, id) -> ((), Use ((), id))
  in
  let use_list = List.map ~f: use_typecheck uses in
  interfaces_typecheck uses interfaces >>= fun (interfaces', _) ->
  fst_klass_pass empty_contexts klasses >>= fun contexts1 ->
  global_pass contexts1 globals >>= fun contexts2 ->
  fst_func_pass contexts2 funcs interfaces >>= fun contexts3 ->
  Result.all (List.map ~f: (snd_func_pass contexts3) funcs) >>= fun func_list ->
  snd_klass_pass contexts3 klasses >>= fun klass_list ->
  Ok ({
    prog  = FullProg (name, ((), Prog (use_list, contexts3.typed_globals, klass_list, func_list)), interfaces');
    ctxts = contexts3;
  })

