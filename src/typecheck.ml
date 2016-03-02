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
let dummy () = ()

let (>>=) = Result.bind
let (>>|) = Result.map

(* `all_unique xs` returns whether every element in `xs` is unique. *)
let all_unique (xs: string list) : bool =
  List.length xs = List.length (List.dedup xs)

(******************************************************************************)
(* expr                                                                       *)
(******************************************************************************)
let rec expr_typecheck c (p, expr) =
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
  | Id s -> begin
    match String.Map.find c s with
    | Some (Var typ) -> Ok (typ, Id s)
    | _ -> Error (p, Printf.sprintf "Variable %s unbound" s)
  end
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
  | FuncCall ((_, name), args) -> begin
    match String.Map.find c name, args with
    | Some (Function (UnitT, t)), [] when t <> UnitT ->
        Ok (t, FuncCall (((), name), []))
    | Some (Function (TupleT argst, t)), _::_::_ when t <> UnitT -> begin
      Result.all (List.map ~f:(expr_typecheck c) args) >>= fun args ->
      match List.zip argst args with
      | Some zipped ->
          if List.for_all ~f:(fun (t1, (t2, _)) -> t1 = t2) zipped
            then Ok (t, FuncCall (((), name), args))
            else Error (p, "Function args do not match parameter type")
      | None -> Error (p, "Incorrect number of arguments")
    end
    | Some (Function (t1, t2)), [arg] when t2 <> UnitT -> begin
      expr_typecheck c arg >>= function
      | (argt, arg) when argt = t1 -> Ok (t2, FuncCall (((), name), [(argt, arg)]))
      | _ -> Error (p, "Function arg does not match parameter type")
    end
    | None, _ -> Error (p, Printf.sprintf "Variable %s not in scope" name)
    | _ -> Error (p, "Function call type error")
  end

(******************************************************************************)
(* stmt                                                                       *)
(******************************************************************************)
let lub a b =
  match a, b with
  | One, _
  | _, One -> One
  | Zero, Zero -> Zero

let stmt_typecheck c rho s =
  let rec (|-) (c, rho) (p, s) : (stmt * context, error_msg) Result.t =
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
          else Error (p, "Unreachable code")
    end
    | If (b, t) -> begin
        expr_typecheck c b >>= fun b' ->
        (c, rho) |- t >>= fun (t', _) ->
        match fst b'  with
        | BoolT -> Ok ((One, If (b', t')), c)
        | _ -> Error (p, "If conditional not a boolean.")
    end
    | IfElse (b, t, f) -> begin
      expr_typecheck c b >>= fun b' ->
      (c, rho) |- t >>= fun (t', _) ->
      (c, rho) |- f >>= fun (f', _) ->
      match fst b'  with
      | BoolT -> Ok ((lub (fst t') (fst f'), IfElse (b', t', f')), c)
      | _ -> Error (p, "If conditional not a boolean.")
    end
    | While (b, s) ->
        expr_typecheck c b >>= fun b' ->
        (c, rho) |- s >>= fun (s', _) ->
        Ok ((One, While (b', s')), c)
    | ProcCall ((_, f), args) -> begin
      match String.Map.find c f, args with
      (* f() *)
      | Some (Function (UnitT, UnitT)), [] ->
          Ok ((One, ProcCall (((), f), [])), c)
      (* f(e1, ..., en) *)
      | Some (Function (TupleT arg_types, UnitT)), _::_::_ -> begin
          Result.all (List.map ~f:(expr_typecheck c) args) >>= fun args' ->
          match List.zip arg_types args' with
          | Some zipped ->
            if List.for_all ~f:(fun (t1, (t2, _)) -> t1 = t2) zipped
              then Ok ((One, ProcCall (((), f), args')), c)
              else Error (p, "Function args do not match parameter type")
          | None -> Error (p, "Incorrect number of arguments")
      end
      (* f(e) *)
      | Some (Function (arg_t, UnitT)), [arg] ->
          expr_typecheck c arg >>= fun arg' ->
          if arg_t = fst arg'
            then Ok ((One, ProcCall (((), f), [arg'])), c)
            else Error (p, "Procedure arg does not match parameter type")
      | None, _ -> Error (p, Printf.sprintf "Function %s not defined" f)
      | _, _ -> Error (p, "Procedure call type error")
    end
    | Return es -> begin
        match rho, es with
        | UnitT, [] -> Ok ((Zero, Return []), c)
        | UnitT, _ -> Error (p, "Non-empty return inside procedure call")
        | TupleT ts, es ->
            Result.all (List.map ~f:(expr_typecheck c) es) >>= fun es' ->
            Ok ((Zero, Return es'), c)
        | t, [e] ->
            expr_typecheck c e >>= fun e' ->
            if fst e' = t
              then Ok ((Zero, Return [e']), c)
              else Error (p, "Invalid return type")
        | t, _ -> Error (p, "Too many return expressions")
    end
    | Decl vs -> failwith "a"
    | DeclAsgn (vs, e) ->failwith "a"
    | Asgn     (l, r) ->failwith "a"
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
