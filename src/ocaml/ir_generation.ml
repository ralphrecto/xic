module Long = Int64
open Core.Std
open Async.Std
open Ir
open Ast
open Typecheck

type node = Node of string * string list
type graph = node list
type block = Block of string * Ir.stmt list

let string_of_node (Node (l, ls)) =
  sprintf "%s -> %s" l (Util.commas ls)

let string_of_graph g =
  Util.join (List.map ~f:string_of_node g)

(******************************************************************************)
(* Naming Helpers                                                             *)
(******************************************************************************)
(* Convert an id string to a temp string. The temp string
 * should not be a possible identifier. Identifiers begin
 * with alphabetic characters. *)
let id_to_temp (idstr: string) : string = "%TEMP%" ^ idstr

(* Increment and return the *old* value of r. *)
let get_and_incr (r: int ref) : int =
  let x = !r in
  incr r;
  x

let num_temp  = ref 0
let num_label = ref 0

let reset_fresh_temp () =
  num_temp := 0

let reset_fresh_label () =
  num_label := 0

let temp n =
  sprintf "__temp%d" n

let label n =
  sprintf "__label%d" n

let fresh_temp () =
  temp (get_and_incr num_temp)

let fresh_label () =
  label (get_and_incr num_label)

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

(* name for ith return register; use for returning
 * values from func calls *)
let retreg i = "_RET" ^ (string_of_int i)

(* name for ith arg register; use for passing
 * argument values into func calls *)
let argreg i = "_ARG" ^ (string_of_int i)

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

(* Format callable names according to Xi ABI *)
let format_callable_name (c: Typecheck.callable) : string =
  let rec type_name (e: Typecheck.Expr.t) = match e with
    | IntT -> "i"
    | BoolT -> "b"
    | UnitT -> "p" (* p for procedure *)
    | ArrayT t' -> "a" ^ (type_name t')
    | TupleT tlist ->
        let open List in
        let tnames = fold_right ~f:( ^ ) ~init:"" (map ~f:type_name tlist) in
        "t" ^ (string_of_int (length tlist)) ^ tnames
    | EmptyArray -> failwith "impossible" in
  let function_name =
    let f c = if c = '_' then "__" else String.of_char c in
    String.concat_map ~f in
  let (fname, argnames, retnames) =
    match c with
    | (argt, rett), Func ((_, idstr), _, _, _)
    | (argt, rett), Proc ((_, idstr), _, _) ->
        function_name idstr, type_name argt, type_name rett in
  Printf.sprintf "_I%s_%s%s" fname retnames argnames

let rec gen_expr ((t, e): Typecheck.expr) =
  match e with
  | Int       i              -> Const i
  | Bool      b              -> if b then Const (1L) else Const (0L)
  (* TODO: supporting more than ASCII chars? *)
  | String    s              ->
      let elms = String.fold s ~init:[] ~f:(fun acc c -> (t, Ast.S.Char c)::acc) in
      gen_expr (ArrayT IntT, Array (List.rev elms))
  | Char      c              -> Const (Int64.of_int (Char.to_int c))
  | Array elts               ->
    let arr_len = List.length elts in
    let mem_loc = malloc_word (arr_len + 1) in
    let loc_tmp = Temp (fresh_temp ()) in
    let mov_elt_seq (i, seq) elt =
      let mov_elt = Move (Mem (loc_tmp$(i), NORMAL), gen_expr elt) in
      (i + 1, mov_elt :: seq) in
    ESeq (
      Seq (
        Move (loc_tmp, mem_loc) ::
        Move (Mem (loc_tmp, NORMAL), const arr_len) ::
        (List.fold_left ~f:mov_elt_seq ~init:(1, []) elts |> snd)
      ),
      loc_tmp$(1)
    )
  | Id       (_, id)         -> Temp (id_to_temp id)
  | BinOp ((t1, e1), op, (t2, e2)) -> begin
      match t1, op, t2 with
      (* Array concatenation *)
      | ArrayT _, PLUS, ArrayT _ ->
        let incr_ir e = (BinOp (e, ADD, const 1)) in

        let arr1, arr2 = gen_expr (t1, e1), gen_expr (t2, e2) in
        let arrtmp1, arrtmp2  = Temp (fresh_temp ()), Temp (fresh_temp ()) in
        let lenarr1, lenarr2 = Temp (fresh_temp ()), Temp (fresh_temp ()) in
        let newarr1, newarr2 = Temp (fresh_label ()), Temp (fresh_label ()) in
        let while_lbl1, while_lbl2 = fresh_label (), fresh_label () in
        let t1_lbl, f1_lbl = fresh_label (), fresh_label () in
        let t2_lbl, f2_lbl = fresh_label (), fresh_label () in
        let i, j = Temp (fresh_temp ()), Temp (fresh_temp ()) in
        ESeq (
          Seq ([
            Move (arrtmp1, arr1);
            Move (arrtmp2, arr2);
            Move (lenarr1, Mem(arrtmp1$(-1), NORMAL));
            Move (lenarr2, Mem(arrtmp2$(-1), NORMAL));
            Move (newarr1, (BinOp (lenarr1, ADD, lenarr2)) |> incr_ir |> malloc_word_ir );
            Move (Mem (newarr1, NORMAL), BinOp (lenarr1, ADD, lenarr2));
            Move (newarr1, BinOp (newarr1, ADD, word));

            Move (i, const 0);  
            Label while_lbl1;
            CJump (BinOp(i, LT, lenarr1), t1_lbl, f1_lbl);
            Label t1_lbl;
            Move (Mem (newarr1$$(i), NORMAL), Mem (arrtmp1$$(i), NORMAL));
            Move (i, incr_ir i);
            Jump (Name while_lbl1);
            Label f1_lbl;

            Move (newarr2, BinOp (newarr1, ADD, lenarr1) |> incr_ir);
            Move (j, const 0);
            Label while_lbl2;
            CJump (BinOp(j, LT, lenarr2), t2_lbl, f2_lbl);
            Label t2_lbl;
            Move (Mem (newarr2$$(j), NORMAL), Mem (arrtmp2$$(j), NORMAL));
            Move (j, incr_ir j);
            Jump (Name while_lbl2);
            Label f2_lbl;
          ]),
          newarr1
        )
      | _ -> BinOp (gen_expr (t1, e1), ir_of_ast_binop op, gen_expr (t2, e2))
    end
  | UnOp     (UMINUS, e1)    -> BinOp (Const (0L), SUB, gen_expr e1)
  | UnOp     (BANG,   e1)    -> not_expr (gen_expr e1)
  | Index    (a, i)          ->
      let index     = gen_expr i in
      let addr      = gen_expr a in
      let len       = Mem (BinOp (addr, SUB, word), NORMAL) in
      let in_bounds = BinOp (BinOp (index, LT, len), AND, BinOp (index, GEQ, Const(0L))) in
      let t_label = fresh_label () in
      let f_label = fresh_label () in
      ESeq (Seq ([
          CJump (in_bounds, t_label, f_label);
          Label f_label;
          Exp (Call (Name out_of_bounds_proc, []));
          Label t_label;
        ]),
        Mem (BinOp (addr, ADD, BinOp (word, MUL, index)), NORMAL)
      )
  | Length    a              -> BinOp (Mem (gen_expr a, NORMAL), SUB, word)
  | FuncCall ((_, id), args) ->
      let args_ir = List.fold_right args
                                    ~f:(fun elm acc -> (gen_expr elm)::acc)
                                    ~init:[] in
      Call (Name id, args_ir)

and gen_control ((t, e): Typecheck.expr) t_label f_label =
  match e with
  | Bool true -> Jump (Name t_label)
  | Bool false -> Jump (Name f_label)
  | BinOp (e1, AMP, e2) ->
    let inter_label = fresh_label () in
    Seq ([
        CJump (gen_expr e1, inter_label, f_label);
        Label inter_label;
        CJump (gen_expr e2, t_label, f_label)
      ])
  | BinOp (e1, BAR, e2) ->
    let inter_label = fresh_label () in
    Seq ([
        CJump (gen_expr e1, t_label, inter_label);
        Label inter_label;
        CJump (gen_expr e2, t_label, f_label)
      ])
  | UnOp (BANG, e1) -> gen_control e1 f_label t_label
  | _ -> CJump (gen_expr (t, e), t_label, f_label)

and gen_decl_help ((_, t): typ) : Ir.expr =
  let incr_ir e = (BinOp (e, ADD, const 1)) in
  match t with
  | TBool | TInt -> Temp (fresh_temp ())
  | TArray ((at', t'), index) ->
    let fill () = 
			match t' with
      | TInt | TBool -> const 0
      | TArray _ -> gen_decl_help (at', t') 
		in
    let array_size = 
			match index with
      | Some index_expr -> gen_expr index_expr
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

and gen_stmt ((_, s): Typecheck.stmt) =
  match s with
  | Decl varlist -> 
		begin
      let gen_var_decls ((_, x): Typecheck.var) seq =
        match x with
        | AVar (_, AId ((_, idstr), (at, TArray (t, i)))) ->
          Move (Temp (id_to_temp idstr), gen_decl_help (at, TArray (t, i))) :: seq
        | _ -> seq in
      Seq (List.fold_right ~f:gen_var_decls ~init:[] varlist)
    end
  | DeclAsgn ([(_,v)], exp) -> 
		begin
      match v with
      | AVar (_, AId (var_id, _)) ->
        let (_, var_id') = var_id in
        Move (Temp (id_to_temp var_id'), gen_expr exp)
      | _ -> Seq []
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
          if i = 0 then gen_expr (TupleT tlist, rawexp)
          else Temp (retreg i) in
        (i + 1, Move (Temp (id_to_temp idstr), retval) :: seq)
      | _ -> (i+1, seq) 
		in
    let (_, ret_seq) = List.fold_left ~f:gen_var_decls ~init:(0,[]) vlist in
    Seq (ret_seq)
  | DeclAsgn (_::_, _) -> failwith "impossible"
  | DeclAsgn ([], _) -> failwith "impossible"
  | Asgn ((_, lhs), fullrhs) -> 
		begin
      match lhs with
      | Id (_, idstr) -> Move (Temp (id_to_temp idstr), gen_expr fullrhs)
      | Index (arr, index) ->
          let mem_loc = gen_expr arr in
          Move (Mem (mem_loc$$(gen_expr index), NORMAL), gen_expr fullrhs)
      | _ -> failwith "impossible"
  	end
  | Block stmts -> Seq (List.map ~f:gen_stmt stmts)
  | Return exprlist ->
      let mov_ret (i, seq) expr  =
        let mov = Move (Temp (retreg i), gen_expr expr) in
        (i + 1, mov :: seq) in
      let (_, moves) = List.fold_left ~f:mov_ret ~init:(0, []) exprlist in
      Seq (moves @ [Ir.Return])
  | If (pred, t) ->
    let t_label = fresh_label () in
    let f_label = fresh_label () in
    Seq ([ 
				gen_control pred t_label f_label; 
				Label t_label;
        gen_stmt t;
        Label f_label;
      ])
  | IfElse (pred, t, f) ->
    let t_label = fresh_label () in
    let f_label = fresh_label () in
    let rest_label = fresh_label () in
    Seq ([
        gen_control pred t_label f_label;
        Label t_label;
        gen_stmt t;
        Jump (Name rest_label);
        Label f_label;
        gen_stmt f;
        Label rest_label;
      ])
  | While (pred, s) ->
    let while_label = fresh_label () in
    let t_label = fresh_label () in
    let f_label = fresh_label () in
    Seq ([
        Label while_label;
        gen_control pred t_label f_label;
        Label t_label;
        gen_stmt s;
        Jump (Name while_label);
        Label f_label;
      ])
  | ProcCall ((_, id), args) ->
    Exp (Call (Name id, List.map ~f:gen_expr args))

and gen_func_decl (c: Typecheck.callable) : (string * Ir.func_decl) =
  let (name, args, body) =
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
          Move (Temp (id_to_temp idstr), Temp (argreg i)) :: seq
      | _ -> seq
    in
    (i + 1, seq')
  in
  let (_, moves) = List.fold_left ~f:arg_mov ~init:(0, []) args in
  (format_callable_name c, (name, Seq(moves @ [gen_stmt body])))

and gen_comp_unit ((_, program): Typecheck.prog) : Ir.comp_unit =
  (* TODO: fix comp unit name to program name *)
  let Ast.S.Prog (_, callables) = program in
  let callables' = List.map ~f:gen_func_decl callables in
  let f map (orig_name, (cname, block)) =
    String.Map.add map ~key:orig_name ~data:(cname, block) in
  let map = List.fold_left ~f ~init:String.Map.empty callables' in
  ("program_name", map)


(******************************************************************************)
(* Lowering IR                                                                *)
(******************************************************************************)

let rec lower_expr e =
  match e with
  | BinOp (e1, binop, e2) ->
    let (s1, e1') = lower_expr e1 in
    let (s2, e2') = lower_expr e2 in
    let temp = Temp (fresh_temp ()) in
    let temp_move = Move (temp, e1') in
    (s1 @ [temp_move] @ s2, BinOp(temp, binop, e2'))
  | Call (e', es) ->
    let call_fold (acc, temps) elm =
      let (s1, e1) = lower_expr elm in
      let temp = fresh_temp () in
      let temp_move = Move (Temp temp, e1) in
      (temp_move::(List.rev_append s1 acc), (Temp temp)::temps)
    in
    let (name_s, name_e) = lower_expr e' in
    let temp_name = fresh_temp () in
    let temp_move_name = Move (Temp temp_name, name_e) in
    let (arg_stmts, arg_temps) = List.fold_left ~f: call_fold ~init: ([], []) es in
    let fn_stmts = name_s @ (temp_move_name :: (List.rev arg_stmts)) in
    let fn_args = List.rev arg_temps in
    let temp_fn = fresh_temp () in
    let temp_move_fn = Move (Temp temp_fn, Call(Temp temp_name, fn_args)) in
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
    let temp = fresh_temp () in
    let temp_move = Move (Temp temp, dest') in
    dest_s @ [temp_move] @ e_s @ [Move(Temp temp, e'')]
  | Seq ss -> List.concat_map ~f:lower_stmt ss
  | Label _
  | Return -> [s]
  | CJumpOne _ -> failwith "this node shouldn't exist"


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

(******************************************************************************)
(* IR-Level Constant Folding                                                  *)
(******************************************************************************)

let rec ir_constant_folding e =
  let open Long in
  let open Big_int in
  match e with
  | BinOp (Const 0L, ADD, Const i)
  | BinOp (Const i, (ADD|SUB), Const 0L)
  | BinOp (Const i, (MUL|DIV), Const 1L)
  | BinOp (Const 1L, MUL, Const i) -> Const i
  | BinOp (Const 0L, SUB, Const i) -> Const (neg i)
  | BinOp (Const i1, ADD, Const i2) -> Const (add i1 i2)
  | BinOp (Const i1, SUB, Const i2) -> Const (sub i1 i2)
  | BinOp (Const i1, MUL, Const i2) -> Const (mul i1 i2)
  | BinOp (Const i1, HMUL, Const i2) ->
    let i1' = big_int_of_int64 i1 in
    let i2' = big_int_of_int64 i2 in
    let mult = mult_big_int i1' i2' in
    let max_long = big_int_of_int64 max_int in
    let divided = div_big_int mult max_long in
    let result = int64_of_big_int divided in
    Const result
  | BinOp (Const i1, DIV, Const i2) -> Const (div i1 i2)
  | BinOp (Const i1, MOD, Const i2) -> Const (rem i1 i2)
  | BinOp (Const 1L, (AND|OR), Const 1L) -> Const 1L
  | BinOp (Const 0L, (AND|OR), Const 0L) -> Const 0L
  | BinOp (Const 1L, OR, Const _)
  | BinOp (Const _, OR, Const 1L) -> Const 1L
  | BinOp (Const 0L, AND, Const _)
  | BinOp (Const _, AND, Const 0L) -> Const 0L
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
      match (ir_constant_folding e1), (ir_constant_folding e2) with
      | (Const _ as c1), (Const _ as c2)-> ir_constant_folding (BinOp (c1, op, c2))
      | e1', e2' -> BinOp (e1', op, e2')
    end
  | Call (e', elist) ->
    let folded_list = List.map ~f: ir_constant_folding elist in
    let folded_e = ir_constant_folding e' in
    Call (folded_e, folded_list)
  | ESeq (s, e') -> ESeq (s, ir_constant_folding e')
  | Mem (e', t) -> Mem (ir_constant_folding e', t)
  | Const _
  | Name _
  | Temp _ -> e


