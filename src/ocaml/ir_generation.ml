module Long = Int64
open Core.Std
open Async.Std
open Ir
open Ir_util
open Ast
open Typecheck

type node = Node of string * string list
type graph = node list
type block = Block of string * Ir.stmt list

let string_of_node (Node (l, ls)) =
  sprintf "%s -> %s" l (Util.commas ls)

let indent (ss: Ir.stmt list) : string =
  List.map ~f:Ir.string_of_stmt ss
  |> List.map ~f:(fun s -> "  " ^ s)
  |> String.concat ~sep:"\n"

let string_of_graph g =
  Util.join (List.map ~f:string_of_node g)

let string_of_block (Block (l, ss)) =
  match ss with
  | [] -> sprintf "%s:" l
  | _  -> sprintf "%s:\n%s" l (indent ss)

let string_of_blocks bs =
  "\n" ^ (String.concat ~sep:"\n" (List.map ~f:string_of_block bs)) ^ "\n"

(******************************************************************************)
(* Naming Helpers                                                             *)
(******************************************************************************)
module FreshTemp  = Fresh.Make(struct let name = "__temp" end)
module FreshLabel = Fresh.Make(struct let name = "__label" end)
module FreshArgReg = Fresh.Make(struct let name = "_ARG" end)
module FreshRetReg = Fresh.Make(struct let name = "_RET" end)


let temp             = FreshTemp.gen
let fresh_temp       = FreshTemp.fresh
let reset_fresh_temp = FreshTemp.reset

let label             = FreshLabel.gen
let fresh_label       = FreshLabel.fresh
let reset_fresh_label = FreshLabel.reset

(* name for ith return register; use for returning
 * values from func calls *)
let retreg = FreshRetReg.gen

(* name for ith arg register; use for passing
 * argument values into func calls *)
let argreg = FreshArgReg.gen

(******************************************************************************)
(* IR Generation                                                              *)
(******************************************************************************)
let not_expr e = BinOp (BinOp (e, ADD, Const 1L), MOD, Const 2L)

let out_of_bounds_proc = "_I_outOfBounds_p"

let const (n: int) =
  Const (Int64.of_int n)

(* Number of bytes in a word in memory *)
let word_size = 8

let word = const word_size

(* returns a function call node that allocates n bytes of memory *)
let malloc (n: int) : Ir.expr =
  Call (Name ("_I_alloc_i"), [Const (Int64.of_int n)])

(* mallocs n words instead of bytes *)
let malloc_word (n: int) : Ir.expr =
  malloc (n * word_size)

(* malloc using ir expr *)
let malloc_ir (e: Ir.expr) : Ir.expr =
  Call (Name ("_I_alloc_i"), [e])

(* malloc_word using ir expr *)
let malloc_word_ir (e: Ir.expr) : Ir.expr =
  malloc_ir (BinOp (e, MUL, const word_size))

(* x $ y == y words offset from x *)
let ( $ ) (x: Ir.expr) (y: int) =
  BinOp(x, ADD, const (y * word_size))

(* x $$ y == y words offset from x *)
let ( $$ ) (x: Ir.expr) (y: Ir.expr) =
  BinOp(x, ADD, BinOp(y, MUL, const word_size))

let ir_of_ast_binop (b_code : Ast.S.binop_code) : binop_code =
  match b_code with
  | MINUS    -> SUB
  | STAR     -> MUL
  | HIGHMULT -> HMUL
  | DIV      -> DIV
  | MOD      -> MOD
  | PLUS     -> ADD
  | LT       -> LT
  | LTE      -> LEQ
  | GTE      -> GEQ
  | GT       -> GT
  | EQEQ     -> EQ
  | NEQ      -> NEQ
  | AMP      -> AND
  | BAR      -> OR

let concat_name = "__concat"
let concat_ir =
  let open Ir.Abbreviations in
  let open Ir.Infix in
  let xs = temp "xs" in
  let ys = temp "ys" in
  let zs = temp "zs" in
  let zs2 = temp "zs2" in
  let len_xs = temp "len_xs" in
  let len_ys = temp "len_ys" in
  let len_zs = temp "len_zs" in
  let i = temp "i" in

  let loop_one_start = "loop_one_start" in
  let loop_one_true = "loop_one_true" in
  let loop_one_false = "loop_one_false" in

  let loop_two_start = "loop_two_start" in
  let loop_two_true = "loop_two_true" in
  let loop_two_false = "loop_two_false" in

  seq [
    (* setup *)
    move xs (temp (argreg 0));
    move ys (temp (argreg 1));
    move len_xs (mem (xs + (const (-8L))));
    move len_ys (mem (ys + (const (-8L))));
    move len_zs (len_xs + len_ys);
    move zs (call (name "_I_alloc_i") [(one + len_zs) * eight]);
    move (mem zs) (len_zs);
    move zs (zs + eight);

    (* loop 1 *)
    move i zero;

    label loop_one_start;
    cjump (i < len_xs) loop_one_true loop_one_false;
    label loop_one_true;
      move (mem (zs + (i * eight))) (mem (xs + (i * eight)));
      move i (i + one);
      jump (name loop_one_start);
    label loop_one_false;

    (* loop 2 *)
    move zs2 (zs + (len_xs * eight));
    move i zero;

    label loop_two_start;
    cjump (i < len_ys) loop_two_true loop_two_false;
    label loop_two_true;
      move (mem (zs2 + (i * eight))) (mem (ys + (i * eight)));
      move i (i + one);
      jump (name loop_two_start);
    label loop_two_false;

    (* return *)
    move (temp (retreg 0)) (zs);
    return;
  ]

let concat_func_decl =
  (concat_name, concat_ir, (Typecheck.Expr.EmptyArray, Typecheck.Expr.EmptyArray))

type callnames = string String.Map.t

let rec gen_expr (callnames: string String.Map.t) ((t, e): Typecheck.expr) ctxt =
  match e with
  | Int i -> Const i
  | Bool b -> if b then Const (1L) else Const (0L)
  | String s ->
    let elms = String.fold s ~init:[] ~f:(fun acc c -> (t, Ast.S.Char c)::acc) in
    gen_expr callnames (ArrayT IntT, Array (List.rev elms)) ctxt
  | Char c -> Const (Int64.of_int (Char.to_int c))
  | Array elts ->
    let arr_len = List.length elts in
    let mem_loc = malloc_word (arr_len + 1) in
    let loc_tmp = Temp (fresh_temp ()) in
    let mov_elt_seq (i, seq) elt =
      let mov_elt = Move (Mem (loc_tmp$(i), NORMAL), gen_expr callnames elt ctxt) in
      (i + 1, mov_elt :: seq) in
    ESeq (
      Seq (
        Move (loc_tmp, mem_loc) ::
        Move (Mem (loc_tmp, NORMAL), const arr_len) ::
        (List.fold_left ~f:mov_elt_seq ~init:(1, []) elts |> snd)
      ),
      loc_tmp$(1)
    )
  | Id (_, id) -> begin
    if String.Set.mem ctxt.globals id then
      failwith "TODO"
    else Temp id
  end
  | BinOp ((t1, e1), op, (t2, e2)) -> begin
      match t1, op, t2 with
      (* Array concatenation *)
      | (ArrayT _ | EmptyArray), PLUS, (ArrayT _ | EmptyArray) ->
          Call (Name concat_name, [gen_expr callnames (t1, e1) ctxt;
                                   gen_expr callnames (t2, e2) ctxt])
      | _ -> BinOp (gen_expr callnames (t1, e1) ctxt, ir_of_ast_binop op, gen_expr callnames (t2, e2) ctxt)
    end
  | UnOp (UMINUS, e1) -> BinOp (Const (0L), SUB, gen_expr callnames e1 ctxt)
  | UnOp (BANG, e1) -> not_expr (gen_expr callnames e1 ctxt)
  | Index (a, i) ->
    let index = gen_expr callnames i ctxt in
    let addr = gen_expr callnames a ctxt in
    let index_tmp = Temp (fresh_temp ()) in
    let addr_tmp = Temp (fresh_temp ()) in
    let len = Mem (BinOp (addr_tmp, SUB, word), NORMAL) in
    let in_bounds = BinOp (BinOp (index_tmp, LT, len), AND, BinOp (index_tmp, GEQ, Const(0L))) in
    let t_label = fresh_label () in
    let f_label = fresh_label () in
    ESeq (Seq ([
        Move (index_tmp, index);
        Move (addr_tmp, addr);
        CJump (in_bounds, t_label, f_label);
        Label f_label;
        Exp (Call (Name out_of_bounds_proc, []));
        Label t_label;
      ]),
      Mem (BinOp (addr_tmp, ADD, BinOp (word, MUL, index_tmp)), NORMAL))
  | Length a -> Mem (BinOp(gen_expr callnames a ctxt, SUB, word), NORMAL)
  | FuncCall ((_, id), args) ->
    let name = match String.Map.find callnames id with
      | Some s -> s
      | None -> failwith "impossible: calling an unknown function" in
    let args_ir =
      List.fold_right args ~f:(fun elm acc -> (gen_expr callnames elm ctxt)::acc) ~init:[] in
    Call (Name name, args_ir)
  | Null -> Const 0L
  | _ -> failwith "TODO"

and gen_control (callnames: string String.Map.t) ((t, e): Typecheck.expr) t_label f_label ctxt =
  match e with
  | Bool true -> Jump (Name t_label)
  | Bool false -> Jump (Name f_label)
  | BinOp (e1, AMP, e2) ->
    let inter_label = fresh_label () in
    Seq ([
        CJump (gen_expr callnames e1 ctxt, inter_label, f_label);
        Label inter_label;
        CJump (gen_expr callnames e2 ctxt, t_label, f_label)
      ])
  | BinOp (e1, BAR, e2) ->
    let inter_label = fresh_label () in
    Seq ([
        CJump (gen_expr callnames e1 ctxt, t_label, inter_label);
        Label inter_label;
        CJump (gen_expr callnames e2 ctxt, t_label, f_label)
      ])
  | UnOp (BANG, e1) -> gen_control callnames e1 f_label t_label ctxt
  | _ -> CJump (gen_expr callnames (t, e) ctxt, t_label, f_label)

and gen_decl_help (callnames: string String.Map.t) ((_, t): typ) ctxt : Ir.expr =
  let incr_ir e = (BinOp (e, ADD, const 1)) in
  match t with
  | TBool | TInt -> Temp (fresh_temp ())
  | TArray ((at', t'), index) ->
    let fill () =
      match t' with
      | TInt | TBool -> const 0
      | TArray _ -> gen_decl_help callnames (at', t') ctxt
      | _ -> failwith "TODO"
    in
    let array_size =
      match index with
      | Some index_expr -> gen_expr callnames index_expr ctxt
      | None -> const 0
    in

    (* helpful temps *)
    let size_tmp = Temp (fresh_temp ()) in
    let loc_tmp = Temp (fresh_temp ()) in
    let i = Temp (fresh_temp ()) in

    (* helpful labels *)
    let cont_lbl = fresh_label () in
    let bad_size_lbl = fresh_label () in
    let while_lbl = fresh_label () in
    let t_lbl = fresh_label () in
    let f_lbl = fresh_label () in

    (* helpful predicates *)
    let pred = BinOp(i, LT, incr_ir array_size) in

    ESeq (
      Seq ([
          (* size_tmp = array_size
           * if size_tmp < 0: outOfBounds() *)
          Move (size_tmp, array_size);
          CJump (BinOp(size_tmp, GEQ, const 0), cont_lbl, bad_size_lbl);
          Label (bad_size_lbl);
          Exp (Call (Name out_of_bounds_proc, []));

          (* loc_tmp = malloc(word_size * (array_size + 1))
           * loc_tmp[0] = array_size
           * i = 1
           * while (i < array_size + 1):
           *   loc_tmp[i] = fill()
           *   i++
           * return &loc_tmp[1] *)
          Label (cont_lbl);
          Move (loc_tmp, array_size |> incr_ir |> malloc_word_ir);
          Move (Mem (loc_tmp, NORMAL), array_size);
          Move (i, const 1);
          Label while_lbl;
          CJump (pred, t_lbl, f_lbl);
          Label t_lbl;
          Move (Mem (loc_tmp$$(i), NORMAL), fill ());
          Move (i, incr_ir i);
          Jump (Name while_lbl);
          Label f_lbl;
        ]),
      loc_tmp$(1)
    )
  | _ -> failwith "TODO"

and gen_stmt callnames s ctxt =
  let rec help callnames ((_, s): Typecheck.stmt) (break_label: string option) =
    match s with
    | Decl varlist ->
      begin
        let gen_var_decls ((_, x): Typecheck.var) seq =
          match x with
          | AVar (_, AId ((_, idstr), (at, TArray (t, i)))) ->
            Move (Temp idstr, gen_decl_help callnames (at, TArray (t, i)) ctxt) :: seq
          | AVar (_, AId ((_, idstr), (_, (TInt | TBool | TKlass _)))) ->
            (Move (Temp idstr, Const 0L))::seq
          | AVar (_, AUnderscore _)
          | Underscore -> seq
        in
        Seq (List.fold_right ~f:gen_var_decls ~init:[] varlist)
      end
    | DeclAsgn ([(_,v)], exp) ->
      begin
        match v with
        | AVar (_, AId (var_id, _)) ->
          let (_, var_id') = var_id in
          Move (Temp var_id', gen_expr callnames exp ctxt)
        | AVar _ | Underscore ->
          Exp (gen_expr callnames exp ctxt)
      end
    | DeclAsgn (_::_ as vlist, (TupleT tlist, rawexp)) ->
      (* TODO: assumptions:
       * - rawexp is necessarily a FuncCall
       * - tuple return values are placed in registers _RET1, etc;
       * see design.txt *)
      let gen_var_decls (i, seq) ((_, x): Typecheck.var) =
        match x with
        | AVar (_, AId ((_, idstr), _)) ->
          let retval =
            if i = 0 then gen_expr callnames (TupleT tlist, rawexp) ctxt
            else Temp (retreg i) in
          (i + 1, Move (Temp idstr, retval) :: seq)
        | AVar _ | Underscore ->
            if i = 0 then
              (i + 1, Exp (gen_expr callnames (TupleT tlist, rawexp) ctxt) :: seq)
            else (i + 1 , seq) in
      let (_, ret_seq_) = List.fold_left ~f:gen_var_decls ~init:(0,[]) vlist in
      let ret_seq = List.rev ret_seq_ in
      Seq (ret_seq)
    | DeclAsgn (_::_, _) -> failwith "impossible"
    | DeclAsgn ([], _) -> failwith "impossible"
    | Asgn ((_, lhs), fullrhs) ->
      begin
        match lhs with
        | Id (_, idstr) -> Move (Temp idstr, gen_expr callnames fullrhs ctxt)
        | Index (arr, i) ->
          let index = gen_expr callnames i ctxt in
          let addr = gen_expr callnames arr ctxt in
          let index_tmp = Temp (fresh_temp ()) in
          let addr_tmp = Temp (fresh_temp ()) in
          let len = Mem (BinOp (addr_tmp, SUB, word), NORMAL) in
          let in_bounds = BinOp (BinOp (index_tmp, LT, len), AND, BinOp (index_tmp, GEQ, Const(0L))) in
          let t_label = fresh_label () in
          let f_label = fresh_label () in
          Seq [
            Move (index_tmp, index);
            Move (addr_tmp, addr);
            CJump (in_bounds, t_label, f_label);
            Label f_label;
            Exp (Call (Name out_of_bounds_proc, []));
            Label t_label;
            Move (Mem (addr_tmp$$index_tmp, NORMAL), gen_expr callnames fullrhs ctxt)
          ]
        | _ -> failwith "impossible"
      end
    | Block stmts -> Seq (List.map ~f:(fun s -> help callnames s break_label) stmts)
    | Return exprlist ->
      let mov_ret (i, seq) expr  =
        let mov = Move (Temp (retreg i), gen_expr callnames expr ctxt) in
        (i + 1, mov :: seq) in
      let (_, moves) = List.fold_left ~f:mov_ret ~init:(0, []) exprlist in
      Seq (moves @ [Ir.Return])
    | If (pred, t) ->
      let t_label = fresh_label () in
      let f_label = fresh_label () in
      Seq ([
          gen_control callnames pred t_label f_label ctxt;
          Label t_label;
          help callnames t break_label;
          Label f_label;
        ])
    | IfElse (pred, t, f) ->
      let t_label = fresh_label () in
      let f_label = fresh_label () in
      let rest_label = fresh_label () in
      Seq ([
          gen_control callnames pred t_label f_label ctxt;
          Label t_label;
          help callnames t break_label;
          Jump (Name rest_label);
          Label f_label;
          help callnames f break_label;
          Label rest_label;
        ])
    | While (pred, s) ->
      let while_label = fresh_label () in
      let t_label = fresh_label () in
      let f_label = fresh_label () in
      Seq ([
          Label while_label;
          gen_control callnames pred t_label f_label ctxt;
          Label t_label;
          help callnames s (Some f_label);
          Jump (Name while_label);
          Label f_label;
        ])
    | ProcCall ((_, id), args) ->
      let name = match String.Map.find callnames id with
        | Some s -> s
        | None -> failwith "impossible: calling unknown function" in
      let f = fun arg -> gen_expr callnames arg ctxt in
      Exp (Call (Name name, List.map ~f args))
    | Break -> begin
      match break_label with
      | None -> failwith "impossible: break has nowhere to jump"
      | Some s -> Jump (Name s)
    end
    | _ -> failwith "TODO"
  in
  help callnames s None

and gen_func_decl (callnames: string String.Map.t) (c: Typecheck.callable) ctxt =
  let (_, args, body) =
    match c with
    | (_, Func ((_, name), args, _, body)) ->
      (name, args, body)
    | (_, Proc ((_, name), args, (s, Block stmts))) ->
      (name, args, (s, Block (stmts @ [(s, Return [])])))
    | (_, Proc ((_, name), args, ((s, _) as body))) ->
      let body' = (s, Ast.S.Block [body; (s, Return [])]) in
      (name, args, body')
  in
  let arg_mov (i, seq) (av: Typecheck.avar)  =
    let seq' =
      match av with
      | (_, AId ((_, idstr), _)) ->
        Move (Temp idstr, Temp (argreg i)) :: seq
      | _ -> seq
    in
    (i + 1, seq')
  in
  let (_, moves) = List.fold_left ~f:arg_mov ~init:(0, []) args in
  let (typ, _) = c in
  (abi_callable_name c, Seq(moves @ [gen_stmt callnames body ctxt]), typ)

and gen_comp_unit (FullProg(name, (_, program), interfaces): Typecheck.full_prog) ctxt =
  let Ast.S.Prog (_, _, _, callables) = program in
  let int_callables =
   List.fold_left
     ~f:(fun acc ((_, Interface (_, _, clist)): Typecheck.interface) -> clist @ acc)
     ~init:[] interfaces in
  let int_callnames = abi_callable_decl_names int_callables in
  let prog_callnames = abi_callable_names callables in
  let callnames =
    String.Map.fold
      ~f:(fun ~key ~data acc -> String.Map.add ~key ~data acc)
      ~init:int_callnames
      prog_callnames in
  let f = fun callable -> gen_func_decl callnames callable ctxt in
  let gen_callables = List.map ~f callables in
  let f map (cname, block, typ) =
    String.Map.add map ~key:cname ~data:(cname, block, typ) in
  let callable_map = List.fold_left ~f ~init:String.Map.empty gen_callables in
  let callable_map =
    String.Map.add callable_map ~key:concat_name ~data:concat_func_decl in
  let open Filename in
  let program_name = name |> chop_extension |> basename in
  (program_name, callable_map)

type filename = string
type globals = Typecheck.global list

let global_name filename =
  sprintf "_I_globalinit_%s" filename

let global_ir (globals: Typecheck.global list) ctxt =
  let initializers = List.map globals ~f:(fun (_, g) ->
    let open Ast.S in
    match g with
    | S.Gdecl vs -> (Stmt.One, S.Decl vs)
    | S.GdeclAsgn (vs, e) -> (Stmt.One, S.DeclAsgn (vs, e))
  ) in
  gen_stmt String.Map.empty (Stmt.One, S.Block initializers) ctxt

let global_func_decl filename globals ctxt =
  (
    global_name filename,
    global_ir globals ctxt,
    (Typecheck.Expr.UnitT, Typecheck.Expr.UnitT)
  )

(******************************************************************************)
(* Lowering IR                                                                *)
(******************************************************************************)

let rec lower_expr e =
  match e with
  | BinOp (e1, binop, e2) ->
    let (s1, e1') = lower_expr e1 in
    let (s2, e2') = lower_expr e2 in
    begin
      match s2 with
      | [] -> (s1 @ s2, BinOp(e1', binop, e2'))
      | _ ->
        let temp = Temp (fresh_temp ()) in
        let temp_move = Move (temp, e1') in
        (s1 @ [temp_move] @ s2, BinOp (temp, binop, e2'))
    end
  | Call (e', es) ->
    let call_fold (acc, temps) elm =
      let (s1, e1) = lower_expr elm in
      let temp = fresh_temp () in
      let temp_move = Move (Temp temp, e1) in
      (temp_move::(List.rev_append s1 acc), (Temp temp)::temps)
    in
    let (name_s, name_e) = lower_expr e' in
    let (arg_stmts, arg_temps) = List.fold_left ~f: call_fold ~init: ([], []) es in
    let fn_stmts = name_s @ (List.rev arg_stmts) in
    let fn_args = List.rev arg_temps in
    let temp_fn = fresh_temp () in
    let temp_move_fn = Move (Temp temp_fn, Call(name_e, fn_args)) in
    (fn_stmts @ [temp_move_fn], Temp temp_fn)
  | ESeq (s, e') ->
    let s1 = lower_stmt s in
    let (s2, e2) = lower_expr e' in
    (s1 @ s2, e2)
  | Mem (e', t) ->
    let (s', e') = lower_expr e' in
    (s', Mem (e', t))
  | Name _
  | Temp _
  | Const _ -> ([], e)

and lower_stmt s =
  match s with
  | CJump (e, l1, l2) ->
    let (s', e') = lower_expr e in
    s' @ [CJump (e', l1, l2)]
  | Jump e ->
    let (s', e') = lower_expr e in
    s' @ [Jump e']
  | Exp e -> fst (lower_expr e)
  | Move (dest, e') ->
    let (dest_s, dest') = lower_expr dest in
    let (e_s, e'') = lower_expr e' in
    dest_s @ e_s @ [Move(dest', e'')]
  | Seq ss -> List.concat_map ~f:lower_stmt ss
  | Label _
  | Return -> [s]
  | CJumpOne _ -> failwith "this node shouldn't exist"

let lower_func_decl (i, s, t) =
  (i, Seq (lower_stmt s), t)

let lower_comp_unit (id, funcs) =
  (id, String.Map.map ~f:(fun data -> lower_func_decl data) funcs)

(******************************************************************************)
(* Basic Block Reordering                                                     *)
(******************************************************************************)
let gen_block stmts =
  let f (blocks, acc, label) s =
    match s, label, acc with
    | Label s, Some l, _  -> (Block (l, acc)::blocks, [], Some s)
    | Label s, None,   [] -> (blocks, [], Some s)
    | Label s, None,   _  -> (Block (fresh_label (), acc)::blocks, [], Some s)
    | (CJump _|Jump _|Return), Some l, _ -> (Block (l, s::acc)::blocks, [], None)
    | (CJump _|Jump _|Return), None,   _ -> (Block (fresh_label (), s::acc)::blocks, [], None)
    | _ -> (blocks, s::acc, label)
  in
  let (b, a, l) = List.fold_left ~f ~init:([], [], None) stmts in
  match l, a with
  | None, [] -> b |> List.rev
  | None, _ -> (Block (fresh_label (), a)) :: b |> List.rev
  | Some l', _ -> (Block (l', a)) :: b |> List.rev

let connect_blocks blocks =
  let rec help blocks acc =
    match blocks with
    | (Block (_, (CJump _ | Jump _ | Return)::_) as h1)::h2::tl ->
      help (h2::tl) (h1::acc)
    | Block (l1, stmts1)::(Block (l2, _) as h2)::tl ->
      let jump_nextblock = Jump (Name l2) in
      let new_block = Block (l1, jump_nextblock::stmts1) in
      help (h2::tl) (new_block::acc)
    | [Block (_, (CJump _ | Jump _ | Return)::_) as h1] -> help [] (h1::acc)
    | [Block (l, stmts)] ->
      let new_block = Block (l, Return::stmts) in
      help [] (new_block::acc)
    | [] -> List.rev acc
  in
  help blocks []

let create_graph blocks =
  let rec help blocks graph =
    match blocks with
    | Block (l1, ss1)::(Block (l2, _) as b2)::tl -> begin
        match ss1 with
        | CJump (_, tru, fls)::_ -> help (b2::tl) (Node (l1, [tru; fls])::graph)
        | Jump (Name l')::_ -> help (b2::tl) (Node (l1, [l'])::graph)
        | Jump _::_ -> failwith "error -- invalid jump"
        | Return::_ -> help (b2::tl) (Node (l1, [])::graph)
        | _ -> help (b2::tl) (Node (l1, [l2])::graph)
      end
    | [Block(l, ss)] -> begin
        match ss with
        | CJump (_, tru, fls)::_ -> help [] (Node (l, [tru; fls])::graph)
        | Jump (Name l')::_ -> help [] (Node (l, [l'])::graph)
        | Jump _::_ -> failwith "error -- invalid jump"
        | Return::_ -> help [] (Node (l, [])::graph)
        | _ -> help [] (Node (l, [])::graph)
      end
    | [] -> List.rev graph
  in
  help blocks []

let (===) (Node (l, _)) l' =
  l = l'

let node_label (Node (l, _)) =
  l

let in_graph graph l =
  List.exists graph ~f:(fun n -> n === l)

let get_node graph l =
  List.find_exn graph ~f:(fun n -> n === l)

let valid_trace graph trace =
  List.length trace > 0 &&
  not (List.contains_dup trace) &&
  List.for_all trace ~f:(in_graph graph) &&
  List.for_all (Util.pairs trace) ~f:(fun (l1, l2) ->
      let (Node (_, ls)) = get_node graph l1 in
      List.mem ls l2
    )

let find_trace graph root =
  let rec help graph (Node (l, adj)) acc =
    let ok l' = l' <> l && in_graph graph l' && not (List.mem acc l') in
    (* we rev to be compatible with an old version and not break tests. *)
    match List.rev (List.filter adj ~f:ok) with
    | [] -> List.rev (l::acc)
    | l'::_ -> help graph (get_node graph l') (l::acc)
  in
  help graph root []

let valid_seq graph seq =
  match graph, seq with
  | [], [] -> true
  | [], _::_ | _::_, [] -> false
  | _::_, []::_ -> false
  | n::_, (l::_)::_ ->
    let graph_labels = List.map graph ~f:node_label in
    let seq_labels = List.concat seq in
    node_label n = l &&
    Util.all_eq graph_labels seq_labels &&
    not (List.contains_dup seq_labels) &&
    List.for_all seq ~f:(valid_trace graph)

let find_seq graph =
  let rec help graph acc =
    match graph with
    | [] -> List.rev acc
    | n::ns ->
      let trace = find_trace ns n in
      let not_in_trace l = not (List.mem trace l) in
      let rest = List.filter graph ~f:(fun (Node(l,_))-> not_in_trace l) in
      help rest (trace::acc)
  in
  help graph []

let tidy blocks =
  let rec help blocks acc =
    match blocks with
    | (Block(l1,ss1) as b1)::(Block(l2,_) as b2)::btl -> begin
        match ss1 with
        | CJump (e, lt, lf)::sstl ->
          if lf = l2 then
            help (b2::btl) (Block(l1, CJumpOne(e, lt)::sstl)::acc)
          else if lt = l2 then
            help (b2::btl) (Block (l1, CJumpOne(not_expr e, lf)::sstl)::acc)
          else
            let new_cjump = CJumpOne (e, lt) in
            let new_jump = Jump (Name lf) in
            help (b2::btl) (Block (l1, new_jump::new_cjump::sstl)::acc)
        | Jump (Name l')::sstl ->
          if l' = l2
          then help (b2::btl) (Block (l1, sstl)::acc)
          else help (b2::btl) (b1::acc)
        | Jump _::_ -> failwith "error -- invalid jump"
        | _ -> help (b2::btl) (b1::acc)
      end
    | [Block(l,ss) as b] -> begin
        match ss with
        | CJump (e, lt, lf)::sstl ->
          let new_cjump = CJumpOne (e, lt) in
          let new_jump = Jump (Name lf) in
          help [] ((Block (l, new_jump::new_cjump::sstl))::acc)
        | _ -> help [] (b::acc)
      end
    | [] -> List.rev acc
  in
  help blocks []

let block_reorder (stmts: Ir.stmt list) =
  let blocks = connect_blocks (gen_block stmts) in
  let graph = create_graph blocks in
  let seq = List.concat (find_seq graph) in
  let get_block l = List.find_exn blocks ~f:(fun (Block(l', _)) -> l' = l) in
  let blocks = List.map seq ~f:get_block in
  let tidied = tidy blocks in
  List.map ~f: (fun (Block (l, s)) -> Block (l, List.rev s)) tidied

let block_to_stmt blist =
  let stmt_list = List.fold_right ~f:(fun (Block (l, stmts)) acc -> (Label l)::stmts@acc) ~init:[] blist in
  Seq (stmt_list)

let block_reorder_func_decl fd =
  match fd with
  | id, Seq stmts, t -> (id, block_reorder stmts |> block_to_stmt, t)
  | _ -> failwith "can't happen"

let block_reorder_comp_unit (id, funcs) =
  (id, String.Map.map ~f:(fun data -> block_reorder_func_decl data) funcs)

(******************************************************************************)
(* IR-Level Constant Folding                                                  *)
(******************************************************************************)

let ir_constant_folding (comp_unit: Ir.comp_unit) : Ir.comp_unit =
  let rec fold_expr e =
    let open Long in
    let open Big_int in
    match e with
    | BinOp (Const _, MOD, Const 0L)
    | BinOp (Const _, DIV, Const 0L) -> e
    | BinOp (Const i1, ADD, Const i2) -> Const (add i1 i2)
    | BinOp (Const i1, SUB, Const i2) -> Const (sub i1 i2)
    | BinOp (Const i1, MUL, Const i2) -> Const (mul i1 i2)
    | BinOp (Const i1, HMUL, Const i2) ->
      let i1' = big_int_of_int64 i1 in
      let i2' = big_int_of_int64 i2 in
      let mult = mult_big_int i1' i2' in
      let shifted = shift_right_big_int mult 64 in
      let result = int64_of_big_int shifted in
      Const result
    | BinOp (Const i1, DIV, Const i2) -> Const (div i1 i2)
    | BinOp (Const i1, MOD, Const i2) -> Const (rem i1 i2)
    | BinOp (Const i1, AND, Const i2) -> Const (logand i1 i2)
    | BinOp (Const i1, OR, Const i2) -> Const (logor i1 i2)
    | BinOp (Const i1, XOR, Const i2) -> Const (logxor i1 i2)
    | BinOp (Const i1, LSHIFT, Const i2) ->
      let i2' = to_int i2 in
      Const (shift_left i1 i2')
    | BinOp (Const i1, RSHIFT, Const i2) ->
      let i2' = to_int i2 in
      Const (shift_right_logical i1 i2')
    | BinOp (Const i1, ARSHIFT, Const i2) ->
      let i2' = to_int i2 in
      Const (shift_right i1 i2')
    | BinOp (Const i1, EQ, Const i2) -> if (compare i1 i2) = 0 then Const (1L) else Const (0L)
    | BinOp (Const i1, NEQ, Const i2) -> if (compare i1 i2) <> 0 then Const (1L) else Const (0L)
    | BinOp (Const i1, LT, Const i2) -> if (compare i1 i2) < 0 then Const (1L) else Const (0L)
    | BinOp (Const i1, GT, Const i2) -> if (compare i1 i2) > 0 then Const (1L) else Const (0L)
    | BinOp (Const i1, LEQ, Const i2) -> if (compare i1 i2) <= 0 then Const (1L) else Const (0L)
    | BinOp (Const i1, GEQ, Const i2) -> if (compare i1 i2) >= 0 then Const (1L) else Const (0L)
    | BinOp (e1, op, e2) ->
      begin
        match (fold_expr e1), (fold_expr e2) with
        | (Const _ as c1), (Const _ as c2)-> fold_expr (BinOp (c1, op, c2))
        | e1', e2' -> BinOp (e1', op, e2')
      end
    | Call (e', elist) ->
      let folded_list = List.map ~f: fold_expr elist in
      let folded_e = fold_expr e' in
      Call (folded_e, folded_list)
    | ESeq (s, e') -> ESeq (s, fold_expr e')
    | Mem (e', t) -> Mem (fold_expr e', t)
    | Const _
    | Name _
    | Temp _ -> e in
  let rec fold_stmt = function
    | CJump (e, s1, s2) -> CJump (fold_expr e, s1, s2)
    | CJumpOne (e, s) -> CJumpOne (fold_expr e, s)
    | Jump e -> Jump (fold_expr e)
    | Exp e -> Exp (fold_expr e)
    | Label _ as l -> l
    | Move (e1, e2) -> Move (fold_expr e1, fold_expr e2)
    | Seq (stmtlist) -> Seq (List.map ~f:fold_stmt stmtlist)
    | Return -> Return in
  let fold_func_decl (fname, stmt, typ) = (fname, fold_stmt stmt, typ) in
  let (id, funcs) = comp_unit in
  (id, String.Map.map ~f:fold_func_decl funcs)

