module Long = Int64
open Core.Std
open Ast
open Ast.S
open Typecheck

type id = string
type value =
  | Int of int64
  | Array of (value list) ref
  | Tuple of value list
type store = Value of value | Function of (id option) list * stmt
type context = (store option) String.Map.t

let of_bool (b: bool) : value =
  if b
  then Int 1L
  else Int 0L

let to_bool (i: int64) : bool =
  match i with
  | 0L -> false
  | 1L -> true
  | _  -> failwith "invalid to_bool argument"

let rec string_of_value (v: value) =
  let sov = string_of_value in
  match v with
  | Int i -> sprintf "%s" (Int64.to_string i)
  | Array vs -> sprintf "{%s}" (Util.commas (List.map ~f:sov !vs))
  | Tuple vs -> sprintf "(%s)" (Util.commas (List.map ~f:sov vs))

let string_of_values (vs: value list) : string =
  List.map vs ~f:(function
      | Int i -> Char.of_int_exn (Int64.to_int_exn i)
      | Array _
      | Tuple _ -> failwith "invalid parseInt argument"
    )
  |> String.of_char_list

(* conv, io interface file functions *)
(* NOTE: eof() function in io not included idk how to do it *)

let unparseInt (n: int64) : string =
  let open Long in
  to_string n

let parseInt (s: string) : int64 =
  let open Long in
  of_string s

let print =
  print_string

let println =
  print_endline

let readln =
  read_line

let getchar () : char =
  let i = read_int () in
  char_of_int i

(* helpful decl functions *)
let id_of_avar ((_, av): avar) =
  match av with
  | AId ((_,id), (_, t)) -> (Some id, Some t)
  | AUnderscore (_, t)-> (None, Some t)

let id_of_var ((_, v): var) =
  match v with
  | AVar av -> id_of_avar av
  | Underscore -> (None, None)

let ids_of_vars vlist = List.map ~f: id_of_var vlist

let ids_of_avars avlist = List.map ~f: id_of_avar avlist

let rec populate_list store t size =
  let filler = 
    match t with
    | TInt | TBool -> Int 0L
    | TArray (_, None) -> Array (ref [])
    | TArray ((_, t'), Some e) -> 
        let size =
          match eval_expr store e with
          | Int i -> i
          | _ -> failwith "bind_ids array size is not an int!!!"
        in
        Array (ref (populate_list store t' size))
  in
  let rec helper t count acc =
    if count > 0L then 
      helper t (Int64.pred count) (filler::acc)
    else
      acc
  in
  helper t size []

and bind_ids store ids =
  let helper s (i, t) =
    match i, t with
    | Some i', Some (TArray (_, None)) -> 
        String.Map.add s ~key:i' ~data: (Some (Value (Array (ref []))))
    | Some i', Some (TArray ((_, t), Some e)) -> 
        let size =
          match eval_expr store e with
          | Int i -> i
          | _ -> failwith "bind_ids array size is not an int!!!"
        in
        let dummy_list = populate_list store t size in 
        String.Map.add s ~key:i' ~data: (Some (Value (Array (ref dummy_list))))
    | Some i', _-> String.Map.add s ~key:i' ~data: None
    | None, _ -> s
  in
  List.fold_left ~f:helper ~init:store ids

and bind_ids_vals store ids_vals =
  let helper s (i, v) =
    match i with
    | Some i' -> String.Map.add s ~key:i' ~data: (Some (Value v))
    | None -> s
  in
  List.fold_left ~f:helper ~init:store ids_vals

(* interpreter *)

and eval_full_prog (store: context) (FullProg (prog, _): Typecheck.full_prog) : value option =
  let updated_store = eval_prog store prog in
  get_main_val updated_store

and eval_prog (store: context) ((_, Prog (_, calls)): Typecheck.prog) : context =
  List.fold_left ~f: (fun store' call -> eval_callable store' call) ~init: store calls

and get_main_val context : value option =
  match String.Map.find context "main" with
  | Some (Some (Function (_, stmt))) -> snd (eval_stmt context stmt)
  | Some _ -> failwith "main is a variable? lol"
  | None -> failwith "no main function? lol"

and eval_callable (store: context) ((_, c): Typecheck.callable) : context =
  match c with
  | Func ((_, id), avars, _, stmt)
  | Proc ((_, id), avars, stmt) ->
    let args = List.map ~f:fst (ids_of_avars avars) in
    String.Map.add store ~key: id ~data: (Some (Function (args, stmt)))

and eval_stmts store ss =
  List.fold_left ~f:(fun (store', res) s ->
      match res, (eval_stmt store' s) with
      | Some _, (s', _) -> (s', res)
      | None, evaled -> evaled)
    ~init:(store, None)
    ss

and eval_stmt (store: context) ((_,s): Typecheck.stmt) : context * value option =
  match s with
  | Decl vlist ->
    let ids = ids_of_vars vlist in
    let store' = bind_ids store ids in
    (store', None)
  | DeclAsgn (vlist, e) ->
    begin
      match vlist, (eval_expr store e) with
      | _::_, Tuple elist ->
        let ids = List.map ~f:fst (ids_of_vars vlist) in
        begin
          match List.zip ids elist with
          | Some l ->
            let store' = bind_ids_vals store l in
            (store', None)
          | None -> failwith "shouldn't happen -- declasgn var list and e list do not match"
        end
      | [v], e' ->
        let id = fst (id_of_var v) in
        let store' = bind_ids_vals store [(id, e')] in
        (store', None)
      | _ -> failwith "shouldn't happen -- declasgn no var declared"
    end
  | Asgn (e1, e2) ->
    begin
      match e1, (eval_expr store e2) with
      | (_, Id (_,i)), e2' ->
        let store' = bind_ids_vals store [(Some i, e2')] in
        (store', None)
      | (_, Index(e1, e2)), e2' ->
        begin
          match (eval_expr store e1), (eval_expr store e2) with
          | Array l, Int i ->
            let i' = Long.to_int i in
            let new_l = List.mapi ~f: (fun idx e -> if idx = i' then e2' else e) (!l) in
            l := new_l;
            (store, None)
          | _ -> failwith "shouldn't happen in asgn"
        end
      | _ -> failwith "shouldn't happen - asgn left is not a var or array indexing"
    end
  | Block slist ->
    let (store', v) = eval_stmts store slist in
    let store'' = String.Map.(filteri store' ~f:(fun ~key ~data:_ -> mem store key)) in
    (store'', v)
  | Return elist ->
    begin
      match elist with
      |_::_::_ ->
        let res = List.map ~f:(eval_expr store) elist in
        (store, Some (Tuple res))
      | [e] ->
        let e' = eval_expr store e in
        (store, Some e')
      |[] -> (store, None)
    end
  | If (e1, slist) ->
    begin
      match eval_expr store e1 with
      | Int 1L -> eval_stmt store slist
      | Int 0L -> (store, None)
      | _ -> failwith "shouldn't happen -- if"
    end
  | IfElse (e1, slist1, slist2) ->
    begin
      match eval_expr store e1 with
      | Int 1L -> eval_stmt store slist1
      | Int 0L -> eval_stmt store slist2
      | _ -> failwith "shouldn't happen -- ifelse"
    end
  | While (e1, slist) ->
    let rec helper b (store', ret) =
      match ret, (eval_expr store' b) with
      | Some _, _ -> (store', ret)
      | None, Int 1L ->
        let updated = eval_stmt store' slist in
        helper b updated
      | None, Int 0L -> (store', ret)
      | _ -> failwith "shouldn't happen -- while not a boolean"
    in
    helper e1 (store, None)
  | ProcCall ((_,id), elist) ->
    begin
      match (String.Map.find store id) with
      | Some (Some (Function (params, body))) ->
        let vals = List.map ~f:(eval_expr store) elist in
        begin
          match (List.zip params vals) with
          | Some l ->
            let store' = bind_ids_vals store l in
            let (_, v) = eval_stmt store' body in
            assert (v = None);
            (store, None)
          | None -> failwith "shouldn't happen -- proccall params and args don't match"
        end
      | Some _ -> failwith "shouldn't happen -- proccall not a function"
      | None ->
        let vals = List.map ~f:(eval_expr store) elist in
        match id, vals with
        | "print", [Array vs] -> print (string_of_values !vs); (store, None)
        | "println", [Array vs] -> println (string_of_values !vs); (store, None)
        | _ -> failwith "shouldn't happen -- proccall function not delcared"
    end

and eval_expr (c: context) ((t, e): Typecheck.expr) : value =
  let open Long in
  match e with
  | Int i -> Int i
  | Bool false -> Int 0L
  | Bool true -> Int 1L
  | String s ->
    let chars = String.to_list_rev (String.rev s) in
    Array (ref (List.map ~f:(fun ch -> eval_expr c (t, Char ch)) chars))
  | Char c -> Int (Int64.of_int (Char.to_int c))
  | Array l ->
    let new_l = ref (List.map ~f:(eval_expr c) l) in
    Array new_l
  | Id (_, i) ->
    begin
      match String.Map.find c i with
      | Some (Some (Value v)) -> v
      | Some _ -> failwith "shouldn't happen -- id"
      | None -> failwith "variable has been declared but not assigned to a value"
    end
  | BinOp (e1, op, e2) -> eval_binop (eval_expr c e1) op (eval_expr c e2)
  | UnOp (op, e1) -> eval_unop op (eval_expr c e1)
  | Index (e1, e2) ->
    begin
      match (eval_expr c e1), (eval_expr c e2) with
      | Array l, Int i ->
        let i' = to_int i in
        begin
          match List.nth (!l) i' with
          | Some x -> x
          | None -> failwith "invalid index"
        end
      | _ -> failwith "shouldn't happen -- index"
    end
  | Length e1 ->
    begin
      match eval_expr c e1 with
      | Array l ->
        let len = List.length (!l) in
        Int (of_int len)
      | _ -> failwith "shouldn't happen -- length"
    end
  | FuncCall ((_, id), elist) ->
    match String.Map.find c id with
    | Some (Some (Function (params, body))) ->
      let vals = List.map ~f:(eval_expr c) elist in
      begin
        match List.zip params vals with
        | Some l ->
          let c' = bind_ids_vals c l in
          begin
            match eval_stmt c' body with
            | (_, Some ret) -> ret
            | _ -> failwith "function has no return"
          end
        | None -> failwith "shouldn't happen -- funccall params and args don't match"
      end
    | Some _ -> failwith "shouldn't happen -- funccall not a function"
    | None ->
      let vals = List.map ~f:(eval_expr c) elist in
      match id, vals with
      | "unparseInt", [Int i] -> eval_expr c (t, String (unparseInt i))
      | "parseInt", [Array vs] -> Int (parseInt (string_of_values (!vs)))
      | "readln", [] -> eval_expr c (t, String (readln ()))
      | "getchar", [] -> eval_expr c (t, Char (getchar ()))
      | _ -> failwith "shouldn't happen -- funccall function not delcared"

and eval_binop e1 op e2 =
  let open Long in
  let open Big_int in
  match e1, op, e2 with
  | Int i1, MINUS, Int i2 -> Int (sub i1 i2)
  | Int i1, STAR, Int i2 -> Int (mul i1 i2)
  | Int i1, HIGHMULT, Int i2 ->
    let i1' = big_int_of_int64 i1 in
    let i2' = big_int_of_int64 i2 in
    let mult = mult_big_int i1' i2' in
    let max_long = big_int_of_int64 max_int in
    let divided = div_big_int mult max_long in
    let result = int64_of_big_int divided in
    Int result
  | Int i1, DIV, Int i2 -> Int (div i1 i2)
  | Int i1, MOD, Int i2 -> Int (rem i1 i2)
  | Int i1, PLUS, Int i2 -> Int (add i1 i2)
  | Int i1, LT, Int i2 -> of_bool ((compare i1 i2) < 0)
  | Int i1, LTE, Int i2 -> of_bool ((compare i1 i2) <= 0)
  | Int i1, GTE, Int i2 -> of_bool ((compare i1 i2) >= 0)
  | Int i1, GT, Int i2 -> of_bool ((compare i1 i2) > 0)
  | Int i1, EQEQ, Int i2 -> of_bool ((compare i1 i2) = 0)
  | Int i1, NEQ, Int i2 -> of_bool ((compare i1 i2) <> 0)
  | Int i1, AMP, Int i2 -> of_bool (to_bool i1 && to_bool i2)
  | Int i1, BAR, Int i2 -> of_bool (to_bool i1 || to_bool i2)
  | Array vs1, PLUS, Array vs2 -> Array (ref ((!vs1) @ (!vs2)))
  | Array vs1, EQEQ, Array vs2 -> of_bool (phys_equal vs1 vs2)
  | Array vs1, NEQ, Array vs2 -> of_bool (not (phys_equal vs1 vs2))
  | _ -> failwith "shouldn't happen -- binop"

and eval_unop op e1 =
  let open Long in
  match op, e1 with
  | UMINUS, Int i -> Int (neg i)
  | BANG, Int i -> Int (Int64.(1L - i))
  | _ -> failwith "shouldn't happen -- unop"

let eval (p: Typecheck.prog) : unit =
  ignore (get_main_val (eval_prog String.Map.empty p))
