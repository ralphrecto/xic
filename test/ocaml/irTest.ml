open Core.Std
open Typecheck.Expr
open Ast.S
open Ir
open Ir_generation
open OUnit
open TestUtil

module Fresh = struct
  let t n = Ir.Temp  (temp n)
  let l n = Ir.Label (label n)
end

module Labels = struct
  let label0  = label 0
  let label1  = label 1
  let label2  = label 2
  let label3  = label 3
  let label4  = label 4
  let label5  = label 5
  let label6  = label 6
  let label7  = label 7
  let label8  = label 8
  let label9  = label 9
  let label10 = label 10
  let label11 = label 11
  let label12 = label 12
  let label13 = label 13
  let label14 = label 14
  let label15 = label 15
  let label16 = label 16
  let label17 = label 17
  let label18 = label 18
  let label19 = label 19
  let label20 = label 20
  let label21 = label 21
  let label22 = label 22
  let label23 = label 23
  let label24 = label 24
  let label25 = label 25
  let label26 = label 26
  let label27 = label 27
  let label28 = label 28
  let label29 = label 29
  let label30 = label 30
  let labela = "label_a"
  let labelb = "label_b"
  let labelc = "label_c"
  let labeld = "label_d"
  let labele = "label_e"
  let labelf = "label_f"

  let l0  = Fresh.l 0
  let l1  = Fresh.l 1
  let l2  = Fresh.l 2
  let l3  = Fresh.l 3
  let l4  = Fresh.l 4
  let l5  = Fresh.l 5
  let l6  = Fresh.l 6
  let l7  = Fresh.l 7
  let l8  = Fresh.l 8
  let l9  = Fresh.l 9
  let l10 = Fresh.l 10
  let l11 = Fresh.l 11
  let l12 = Fresh.l 12
  let l13 = Fresh.l 13
  let l14 = Fresh.l 14
  let l15 = Fresh.l 15
  let l16 = Fresh.l 16
  let l17 = Fresh.l 17
  let l18 = Fresh.l 18
  let l19 = Fresh.l 19
  let l20 = Fresh.l 20
  let l21 = Fresh.l 21
  let l22 = Fresh.l 22
  let l23 = Fresh.l 23
  let l24 = Fresh.l 24
  let l25 = Fresh.l 25
  let l26 = Fresh.l 26
  let l27 = Fresh.l 27
  let l28 = Fresh.l 28
  let l29 = Fresh.l 29
  let l30 = Fresh.l 30
  let la  = Ir.Label labela
  let lb  = Ir.Label labelb
  let lc  = Ir.Label labelc
  let ld  = Ir.Label labeld
  let le  = Ir.Label labele
  let lf  = Ir.Label labelf
end

let block l ss = Block (l, ss)
let node l ls = Node (l, ls)
let zero = Ir.Abbreviations.const 0L
let one  = Ir.Abbreviations.const 1L
let two  = Ir.Abbreviations.const 2L

(* By default, OUnit's assert_equal function doesn't print out anything useful
 * when its assertion fails. For example, 1 === 2 won't print either 1 or 2!
 * This module specializes === to a bunch of data types that do print
 * things out when tests fail! For example, if you want to check that two
 * expressions are equal, just do ExprEq.(a === b)` or `let open ExprEq in a
 * === b`. *)
module ExprEq = struct
  let (===) (a: expr) (b: expr) : unit =
    assert_equal ~printer:string_of_expr a b

  let (=/=) (a: expr) (b:expr) : unit =
    let soe = string_of_expr in
    try
      if ((a === b) = ()) then
        raise (Failure (sprintf "Expected %s but got %s" (soe a) (soe b)))
    with
      _ -> ()
end

module StmtEq = struct
  let (===) (a: stmt) (b: stmt) : unit =
    assert_equal ~printer:string_of_stmt a b
end

module StmtsEq = struct
  let (===) (a: stmt list) (b: stmt list) : unit =
    assert_equal ~printer:string_of_stmts a b
end

module PairEq = struct
  let (===) (a: stmt list * expr) (b: stmt list * expr) : unit =
    let printer (ss, e) = sprintf "<%s; %s>" (string_of_stmts ss) (string_of_expr e) in
    assert_equal ~printer a b
end

module BlocksEq = struct
  let indent (ss: stmt list) : string =
    List.map ~f:string_of_stmt ss
    |> List.map ~f:(fun s -> "  " ^ s)
    |> String.concat ~sep:"\n"

  let string_of_block (Block (l, ss)) =
    match ss with
    | [] -> sprintf "%s:" l
    | _  -> sprintf "%s:\n%s" l (indent ss)

  let string_of_blocks bs =
    "\n" ^ (String.concat ~sep:"\n" (List.map ~f:string_of_block bs)) ^ "\n"

  let (===) (a: block list) (b: block list) : unit =
    let uniq_labels blocks =
      List.map ~f:(fun (Block (l, _)) -> l) blocks
      |> List.contains_dup
      |> not
    in
    assert_true (uniq_labels a);
    assert_true (uniq_labels b);
    assert_equal ~printer:string_of_blocks a b
end

module GraphEq = struct
  let (===) (a: graph) (b: graph) : unit =
    assert_equal ~printer:string_of_graph a b
end

(**** HELPER FUNCTIONS for generating Typecheck exprs ****)
let int x  = (IntT, Int x)
let bool b = (BoolT, Bool b)
let arr t args = (t, Array args)
let iarr args = arr IntT args
let barr args = arr BoolT args
let earr = arr EmptyArray []
let binop i e1 op e2 : Typecheck.expr = (i, BinOp (e1, op, e2))
let ibinop e1 op e2 = binop IntT e1 op e2
let bbinop e1 op e2 = binop BoolT e1 op e2
let unop i op e : Typecheck.expr = (i, UnOp (op, e))
let iunop e = unop IntT UMINUS e
let bunop e = unop BoolT BANG e
let id x t = (t, Id ((), x))
let code c = Const (Int64.of_int (Char.to_int c))

module Ir_gen = Ir_generation

let test_ir_expr () =
  let open ExprEq in
  let open Abbreviations in

  Ir_gen.reset_fresh_temp ();

  (* Ir exprs *)
  let zero = const 0L in
  let one  = const 1L in
  let two  = const 2L in
  let tru  = one in
  let fls  = zero in
  let binop00 = BinOp (zero, ADD, zero) in
  let binop12 = BinOp (one, ADD, two) in
  let unopminus x = BinOp (zero, SUB, x) in
  let unopnot x = BinOp (BinOp (x, ADD, one), MOD, two) in
  let x = temp "%TEMP%x" in
  let y = temp "%TEMP%y" in
  let word = const 8L in

  (* Typecheck exprs *)
  let zerot = int 0L in
  let onet  = int 1L in
  let twot  = int 2L in
  let trut  = bool true in
  let flst  = bool false in
  let binop00t = ibinop zerot PLUS zerot in
  let binop12t = ibinop onet PLUS twot in

  (* Ints, Bools, and Chars tests *)
  zero === gen_expr zerot;
  one  === gen_expr onet;
  two  === gen_expr twot;
  tru  === gen_expr trut;
  fls  === gen_expr flst;

  (* Id tests *)
  (* TODO: are there any interesting cases? *)
  x === gen_expr (id "x" IntT);
  y === gen_expr (id "y" BoolT);

  x =/= gen_expr (id "y" IntT);

  (* BinOp tests *)
  binop00 === gen_expr binop00t;
  binop12  === gen_expr binop12t;
  BinOp (binop12, SUB, binop12)  === gen_expr (ibinop binop12t MINUS binop12t);
  BinOp (binop00, SUB, binop12)  === gen_expr (ibinop binop00t MINUS binop12t);
  BinOp (BinOp (tru, AND, fls), OR, BinOp (fls, OR, fls))
    ===
    gen_expr (bbinop (bbinop trut AMP flst) BAR (bbinop flst BAR flst));

  BinOp (one, ADD, two)  =/= gen_expr (ibinop twot PLUS onet);
  BinOp (one, ADD, zero) =/= gen_expr (bbinop onet BAR twot);

  (* UnOp tests *)
  unopminus zero === gen_expr (iunop zerot);
  unopminus one  === gen_expr (iunop onet);
  unopminus two  === gen_expr (iunop twot);
  unopnot tru    === gen_expr (bunop trut);
  unopnot fls    === gen_expr (bunop flst);

  (* String and Array tests *)
  Ir_gen.reset_fresh_temp ();
  eseq 
    (seq (
      (move ( Temp (Ir_gen.temp 0) ) ( Ir_gen.malloc_word 1 )) ::
      (move ( mem ( Temp (Ir_gen.temp 0) )) zero             ) ::
      []
    ))
    (BinOp (Temp (Ir_gen.temp 0), ADD, word))
  ===
  gen_expr earr;

  Ir_gen.reset_fresh_temp ();
  eseq 
    (seq (
      (move ( Temp (Ir_gen.temp 0) ) ( Ir_gen.malloc_word 2 )      ) ::
      (move ( mem ( Temp (Ir_gen.temp 0) )) one                    ) ::
      (move ( mem ( BinOp (Temp (Ir_gen.temp 0), ADD, word))) zero ) ::
      []
    ))
    (BinOp (Temp (Ir_gen.temp 0), ADD, word))
  ===
  gen_expr (iarr [zerot]);

  Ir_gen.reset_fresh_temp ();
  eseq 
    (seq (
      (move ( Temp (Ir_gen.temp 0) ) ( Ir_gen.malloc_word 3 )          ) ::
      (move ( mem ( Temp (Ir_gen.temp 0) )) two                        ) ::
      (move ( mem ( BinOp (Temp (Ir_gen.temp 0), ADD, const 16L))) one ) ::
      (move ( mem ( BinOp (Temp (Ir_gen.temp 0), ADD, word))) zero     ) ::
      []
    ))
    (BinOp (Temp (Ir_gen.temp 0), ADD, word))
  ===
  gen_expr (iarr [zerot; onet]);

  Ir_gen.reset_fresh_temp ();
  eseq 
    (seq (
      (move (Temp (Ir_gen.temp 0)) (Ir_gen.malloc_word 4)           ) ::
      (move (mem (Temp (Ir_gen.temp 0))) (const 3L)                 ) ::
      (move (mem (BinOp (Temp (Ir_gen.temp 0), ADD, const 24L))) two) ::
      (move (mem (BinOp (Temp (Ir_gen.temp 0), ADD, const 16L))) one) ::
      (move (mem (BinOp (Temp (Ir_gen.temp 0), ADD, word))) zero    ) ::
      []
    ))
    (BinOp (Temp (Ir_gen.temp 0), ADD, word))
  ===
  gen_expr (iarr [zerot; onet; twot]);

  Ir_gen.reset_fresh_temp ();
  eseq 
    (seq (
      (move (Temp (Ir_gen.temp 0)) (Ir_gen.malloc_word 1)) ::
      (move (mem (Temp (Ir_gen.temp 0))) zero            ) ::
      []
    ))
    (BinOp (Temp (Ir_gen.temp 0), ADD, word))
  ===
  gen_expr (EmptyArray, String "");

  Ir_gen.reset_fresh_temp ();
  eseq 
    (seq (
      (move (Temp (Ir_gen.temp 0)) (Ir_gen.malloc_word 2)              ) ::
      (move (mem (Temp (Ir_gen.temp 0))) one                           ) ::
      (move (mem (BinOp (Temp (Ir_gen.temp 0), ADD, word))) (const 65L)) ::
      []
    ))
    (BinOp (Temp (Ir_gen.temp 0), ADD, word))
  ===
  gen_expr (EmptyArray, String "A");

  Ir_gen.reset_fresh_temp ();
  eseq 
    (seq (
      (move (Temp (Ir_gen.temp 0)) (Ir_gen.malloc_word 9)                  ) ::
      (move (mem (Temp (Ir_gen.temp 0))) (const 8L)                        ) ::
      (move (mem (BinOp (Temp (Ir_gen.temp 0), ADD, const 64L))) (code '3')) ::
      (move (mem (BinOp (Temp (Ir_gen.temp 0), ADD, const 56L))) (code '<')) ::
      (move (mem (BinOp (Temp (Ir_gen.temp 0), ADD, const 48L))) (code ' ')) ::
      (move (mem (BinOp (Temp (Ir_gen.temp 0), ADD, const 40L))) (code 'l')) ::
      (move (mem (BinOp (Temp (Ir_gen.temp 0), ADD, const 32L))) (code 'm')) ::
      (move (mem (BinOp (Temp (Ir_gen.temp 0), ADD, const 24L))) (code 'a')) ::
      (move (mem (BinOp (Temp (Ir_gen.temp 0), ADD, const 16L))) (code 'C')) ::
      (move (mem (BinOp (Temp (Ir_gen.temp 0), ADD, word)))      (code 'O')) ::
      []
    ))
    (BinOp (Temp (Ir_gen.temp 0), ADD, word))
  ===
  gen_expr (ArrayT IntT, String "OCaml <3");

  Ir_gen.reset_fresh_temp ();
  eseq 
    (seq (
      (move (Temp (Ir_gen.temp 0)) (Ir_gen.malloc_word 5)                  ) ::
      (move (mem (Temp (Ir_gen.temp 0))) (const 4L)                        ) ::
      (move (mem (BinOp (Temp (Ir_gen.temp 0), ADD, const 32L))) (code ' ')) ::
      (move (mem (BinOp (Temp (Ir_gen.temp 0), ADD, const 24L))) (code ' ')) ::
      (move (mem (BinOp (Temp (Ir_gen.temp 0), ADD, const 16L))) (code ' ')) ::
      (move (mem (BinOp (Temp (Ir_gen.temp 0), ADD, word)))      (code ' ')) ::
      []
    ))
    (BinOp (Temp (Ir_gen.temp 0), ADD, word))
  ===
  gen_expr (ArrayT IntT, String "    ");

  (* Array Indexing tests *)

  (* Length tests *)


  (* FuncCall tests *)

  ()

let test_ir_stmt () =
	let open StmtEq in

	(* Vars *)
	let create_int_avar (id : string) : Typecheck.avar =
		(IntT, AId (((), id), (IntT, TInt)))
	in	

	let create_int_var (id : string) : Typecheck.var =
		(IntT, AVar (create_int_avar id))
	in

	let create_bool_avar (id : string) : Typecheck.avar =
		(BoolT, AId (((), id), (BoolT, TBool)))
	in

	let create_bool_var (id : string) : Typecheck.var =
		(BoolT, AVar (create_int_avar id))
	in

	let create_int_array_avar (id : string) (size : int64 option) : Typecheck.avar =
		match size with
		| Some x -> (ArrayT (IntT), AId (((), id), (ArrayT (IntT), TArray ((IntT, TInt), Some (IntT, Int x)))))
		| None -> (ArrayT (IntT), AId (((), id), (ArrayT (IntT), TArray ((IntT, TInt), None))))
	in

	let create_int_array_var (id: string) (size : int64 option) : Typecheck.var =
		(ArrayT (IntT), AVar (create_int_array_avar id size))
	in
	
	let create_bool_array_avar (id : string) (size : int64 option) : Typecheck.avar =
		match size with
		| Some x -> (ArrayT (BoolT), AId (((), id), (ArrayT (BoolT), TArray ((BoolT, TBool), Some (IntT, Int x)))))
		| None -> (ArrayT (BoolT), AId (((), id), (ArrayT (BoolT), TArray ((BoolT, TBool), None))))
	in

	let create_bool_array_var (id: string) (size : int64 option) : Typecheck.var =
		(ArrayT (BoolT), AVar (create_int_array_avar id size))
	in

	(* Decl tests *)
	(* ints *)		
	Seq [] === gen_stmt ((Zero, Decl [create_int_var "hello"]))
	
	(* bools *)

	(* arrays *)

	(* arrays of arrays *)

	(* mix of all sort of things *)

	(* DeclAsgn tests *)

	(* Asgn tests *)

	(* Block tests *)

	(* Retrun tests *)

	(* If tests *)

	(* IfElse tests *)

	(* While tests *)

	(* ProcCall tests *)

let test_lower_expr () =
  let open PairEq in
  let open Fresh in

  Ir_gen.reset_fresh_temp ();

  let one = Const 1L in
  let two = Const 2L in
  let stmts = [Return; Return; CJump (one, "t", "f")] in
  let seq = Seq stmts in
  let eseq e = ESeq (seq, e) in

  (* Const *)
  ([], Const 42L) === lower_expr (Const 42L);

  (* Name *)
  ([], Name "foo") === lower_expr (Name "foo");

  (* Temp *)
  ([], Temp "foo") === lower_expr (Temp "foo");

  (* Mem *)
  (stmts, Mem (one, NORMAL)) === lower_expr (Mem (eseq one, NORMAL));

  (* ESeq *)
  (stmts @ stmts, one) === lower_expr (ESeq (seq, eseq one));

  (* BinOp *)
  (stmts@[Move (t 0, one)]@stmts, BinOp(t 0, ADD, one))
  ===
  lower_expr (BinOp (eseq one, ADD, eseq one));

  (* Call *)
  (stmts@[Move (t 1, Name "f")]@
   stmts@[Move (t 2, one)]@
   stmts@[Move (t 3, two)]@
   [Move (t 4, Call(t 1, [t 2; t 3]))],
   t 4)
  ===
  lower_expr (Call (eseq (Name "f"), [eseq one; eseq two]));

  ()

let test_lower_stmt () =
  let open StmtsEq in
  let open Fresh in

  let one = Const 1L in
  let two = Const 2L in
  let stmts = [Return; Return; CJump (one, "t", "f")] in
  let seq = Seq stmts in
  let eseq e = ESeq (seq, e) in

  (* Label *)
  [Label "l"] === lower_stmt (Label "l");

  (* Return *)
  [Return] === lower_stmt Return;

  (* CJump *)
  stmts@[CJump (one, "t", "f")] === lower_stmt (CJump (eseq one, "t", "f"));

  (* Jump *)
  stmts@[Jump one] === lower_stmt (Jump (eseq one));

  (* Exp *)
  stmts === lower_stmt (Exp (eseq one));

  (* Move *)
  stmts@[Move (t 5, two)]@stmts@[Move (one, t 5)]
  ===
  lower_stmt (Move (eseq one, eseq two));

  (* Seq ss *)
  stmts@stmts@stmts
  ===
  lower_stmt (Seq [Seq stmts; Seq stmts; Seq stmts]);

  ()

let test_gen_block () =
  let open Labels in
  let open BlocksEq in
  let open Ir.Abbreviations in
  let open Ir.Infix in

  reset_fresh_label ();
  let stmts = [] in
  let expected = [] in
  expected === gen_block stmts;

  reset_fresh_label ();
  let stmts, expected = [
    exp one;
  ], [
    block label0 [exp one];
  ] in
  expected === gen_block stmts;

  reset_fresh_label ();
  let stmts, expected = [
    exp one;
    exp two;
  ], [
    block label0 [exp two;
                  exp one];
  ] in
  expected === gen_block stmts;

  reset_fresh_label ();
  let stmts, expected = [
    exp zero;
    exp one;
    exp two;
  ], [
    block label0 [exp two;
                  exp one;
                  exp zero];
  ] in
  expected === gen_block stmts;

  reset_fresh_label ();
  let stmts, expected = [
    l0;
  ], [
    block label0 []
  ] in
  expected === gen_block stmts;

  reset_fresh_label ();
  let stmts, expected = [
    la;
  ], [
    block labela []
  ] in
  expected === gen_block stmts;

  reset_fresh_label ();
  let stmts, expected = [
    la; exp one
  ], [
    block labela [exp one]
  ] in
  expected === gen_block stmts;

  reset_fresh_label ();
  let stmts, expected = [
    la; exp one;
        exp two
  ], [
    block labela [exp two; exp one]
  ] in
  expected === gen_block stmts;

  reset_fresh_label ();
  let stmts, expected = [
    la; exp zero;
        exp one;
        exp two;
  ], [
    block labela [exp two; exp one; exp zero]
  ] in
  expected === gen_block stmts;

  reset_fresh_label ();
  let stmts, expected = [
        exp zero;
    la; exp one;
  ], [
    block label0 [exp zero];
    block labela [exp one];
  ] in
  expected === gen_block stmts;

  reset_fresh_label ();
  let stmts, expected = [
        exp zero;
    la;
  ], [
    block label0 [exp zero];
    block labela [];
  ] in
  expected === gen_block stmts;

  reset_fresh_label ();
  let stmts, expected = [
    la;
    lb;
  ], [
    block labela [];
    block labelb [];
  ] in
  expected === gen_block stmts;

  reset_fresh_label ();
  let stmts, expected = [
        exp zero;
        exp one;
    la;
  ], [
    block label0 [exp one; exp zero];
    block labela [];
  ] in
  expected === gen_block stmts;

  reset_fresh_label ();
  let stmts, expected = [
        exp zero;
        exp one;
    la; exp two;
        exp one
  ], [
    block label0 [exp one; exp zero];
    block labela [exp one; exp two];
  ] in
  expected === gen_block stmts;

  reset_fresh_label ();
  let stmts, expected = [
        exp zero;
        exp one;
    la; exp two;
    lb; exp one
  ], [
    block label0 [exp one; exp zero];
    block labela [exp two];
    block labelb [exp one];
  ] in
  expected === gen_block stmts;

  reset_fresh_label ();
  let stmts, expected = [
    jump one
  ], [
    block label0 [jump one]
  ] in
  expected === gen_block stmts;

  reset_fresh_label ();
  let stmts, expected = [
    cjump one "a" "b"
  ], [
    block label0 [cjump one "a" "b"]
  ] in
  expected === gen_block stmts;

  reset_fresh_label ();
  let stmts, expected = [
    return
  ], [
    block label0 [return]
  ] in
  expected === gen_block stmts;

  reset_fresh_label ();
  let stmts, expected = [
    la; jump one
  ], [
    block labela [jump one]
  ] in
  expected === gen_block stmts;

  reset_fresh_label ();
  let stmts, expected = [
    la; cjump one "a" "b"
  ], [
    block labela [cjump one "a" "b"]
  ] in
  expected === gen_block stmts;

  reset_fresh_label ();
  let stmts, expected = [
    la; return
  ], [
    block labela [return]
  ] in
  expected === gen_block stmts;

  reset_fresh_label ();
  let stmts, expected = [
    jump one;
    jump one;
  ], [
    block label0 [jump one];
    block label1 [jump one];
  ] in
  expected === gen_block stmts;

  reset_fresh_label ();
  let stmts, expected = [
    cjump one "a" "b";
    cjump one "a" "b";
  ], [
    block label0 [cjump one "a" "b"];
    block label1 [cjump one "a" "b"];
  ] in
  expected === gen_block stmts;

  reset_fresh_label ();
  let stmts, expected = [
    return;
    return;
  ], [
    block label0 [return];
    block label1 [return];
  ] in
  expected === gen_block stmts;

  reset_fresh_label ();
  let stmts, expected = [
    jump one;
    cjump one "a" "b";
    return;
  ], [
    block label0 [jump one];
    block label1 [cjump one "a" "b"];
    block label2 [return];
  ] in
  expected === gen_block stmts;

  reset_fresh_label ();
  let stmts, expected = [
    jump one;
    cjump one "a" "b";
    return;
  ], [
    block label0 [jump one];
    block label1 [cjump one "a" "b"];
    block label2 [return];
  ] in
  expected === gen_block stmts;

  reset_fresh_label ();
  let stmts, expected = [
        exp one;
    la;
    lb; exp two;
        exp zero;
        jump one;
        jump one;
    lc; exp one;
    ld; return;
        exp one;
  ], [
    block label0 [exp one];
    block labela [];
    block labelb [jump one; exp zero; exp two;];
    block label1 [jump one];
    block labelc [exp one];
    block labeld [return];
    block label2 [exp one];
  ] in
  expected === gen_block stmts;

  ()

let test_connect_blocks () =
  let open Labels in
  let open BlocksEq in
  let open Ir.Abbreviations in
  let open Ir.Infix in

  let blocks = [] in
  let expected = [] in
  expected === connect_blocks blocks;

  let blocks = [block label0 [exp one]] in
  let expected = [block label0 [return; exp one]] in
  expected === connect_blocks blocks;

  let blocks = [block label0 [exp one; exp two]] in
  let expected = [block label0 [return; exp one; exp two]] in
  expected === connect_blocks blocks;

  let blocks = [block label0 [exp zero; exp one; exp two]] in
  let expected = [block label0 [return; exp zero; exp one; exp two]] in
  expected === connect_blocks blocks;

  let blocks = [block label0 [jump one]] in
  let expected = [block label0 [jump one]] in
  expected === connect_blocks blocks;

  let blocks = [block label0 [cjump one "a" "b"]] in
  let expected = [block label0 [cjump one "a" "b"]] in
  expected === connect_blocks blocks;

  let blocks = [block label0 [return]] in
  let expected = [block label0 [return]] in
  expected === connect_blocks blocks;

  let blocks = [
    block label0 [exp one];
    block label1 [return];
  ] in
  let expected = [
    block label0 [jump (name label1); exp one];
    block label1 [return];
  ] in
  expected === connect_blocks blocks;

  let blocks = [
    block label0 [exp one];
    block label1 [exp two];
    block label2 [return];
  ] in
  let expected = [
    block label0 [jump (name label1); exp one];
    block label1 [jump (name label2); exp two];
    block label2 [return];
  ] in
  expected === connect_blocks blocks;

  let blocks = [
    block label0 [exp zero; exp one; exp two];
    block label1 [exp two; exp one; exp zero];
    block label2 [return];
  ] in
  let expected = [
    block label0 [jump (name label1); exp zero; exp one; exp two];
    block label1 [jump (name label2); exp two; exp one; exp zero];
    block label2 [return];
  ] in
  expected === connect_blocks blocks;

  let blocks = [
    block label0 [exp zero; exp one; exp two];
    block label1 [exp two; exp one; exp zero];
  ] in
  let expected = [
    block label0 [jump (name label1); exp zero; exp one; exp two];
    block label1 [return; exp two; exp one; exp zero];
  ] in
  expected === connect_blocks blocks;

  let blocks = [
    block label0 [exp one];
    block label1 [jump one];
    block label2 [exp two];
    block label3 [cjump one "a" "b"];
    block label4 [return];
  ] in
  let expected = [
    block label0 [jump (name label1); exp one];
    block label1 [jump one];
    block label2 [jump (name label3); exp two];
    block label3 [cjump one "a" "b"];
    block label4 [return];
  ] in
  expected === connect_blocks blocks;

  let blocks = [
    block label0 [];
    block label1 [];
  ] in
  let expected = [
    block label0 [jump (name label1)];
    block label1 [return];
  ] in
  expected === connect_blocks blocks;

  ()

let test_create_graph () =
  let open Labels in
  let open GraphEq in
  let open Ir.Abbreviations in
  let open Ir.Infix in

  let blocks   = [] in
  let expected = [] in
  expected === create_graph blocks;

  let blocks   = [block label0 [exp one]] in
  let expected = [node label0 []] in
  expected === create_graph blocks;

  let blocks   = [block label0 [jump (name "a")]] in
  let expected = [node label0 ["a"]] in
  expected === create_graph blocks;

  let blocks   = [block label0 [cjump one "a" "b"]] in
  let expected = [node label0 ["a"; "b"]] in
  expected === create_graph blocks;

  let blocks   = [block label0 [return]] in
  let expected = [node label0 []] in
  expected === create_graph blocks;

  let blocks   = [
    block label0 [exp one];
    block label1 [exp two];
  ] in
  let expected = [
    node label0 [label1];
    node label1 [];
  ] in
  expected === create_graph blocks;

  let blocks   = [
    block label0 [exp one];
    block label1 [jump (name "a")];
  ] in
  let expected = [
    node label0 [label1];
    node label1 ["a"];
  ] in
  expected === create_graph blocks;

  let blocks   = [
    block label0 [exp one];
    block label1 [cjump one "a" "b"];
  ] in
  let expected = [
    node label0 [label1];
    node label1 ["a"; "b"];
  ] in
  expected === create_graph blocks;

  let blocks   = [
    block label0 [exp zero; exp one; exp two];
    block label1 [exp one];
    block label2 [jump (name "a")];
    block label3 [return];
    block label4 [];
    block label5 [exp zero];
  ] in
  let expected = [
    node label0 [label1];
    node label1 [label2];
    node label2 ["a"];
    node label3 [];
    node label4 [label5];
    node label5 [];
  ] in
  expected === create_graph blocks;

  ()

module Graphs = struct
  let n (l,ls) = Node (l, ls)
  let a, b, c, d = "a", "b", "c", "d"
  let all = [a;b;c;d]
  let line    = [n(a,[b]);   n(b,[c]);   n(c,[d]); n(d,[])   ]
  let square  = [n(a,[b]);   n(b,[c]);   n(c,[d]); n(d,[a])  ]
  let diamond = [n(a,[b;c]); n(b,[d]);   n(c,[d]); n(d,[])   ]
  let clique  = [n(a,all);   n(b,all);   n(c,all); n(d,all)  ]
  let pairs   = [n(a,[b]);   n(b,[a]);   n(c,[d]); n(d,[c])  ]
  let points  = [n(a,[]);    n(b,[]);    n(c,[]);  n(d,[])   ]
  let weird   = [n(a,[b;c]); n(b,[c;d]); n(c,[d]); n(d,[a;d])]
  let graphs = [line; square; diamond; clique; pairs; points; weird;]

  let in_graph graph l =
    let (===) (Node (l, _)) l' = l = l' in
    List.exists graph ~f:(fun n -> n === l)

  let get_node graph l =
    let (===) (Node (l, _)) l' = l = l' in
    List.find_exn graph ~f:(fun n -> n === l)
end

let test_valid_trace () =
  let open Graphs in
  (* In all graphs, single hop paths are good. *)
  List.iter graphs ~f:(fun g ->
    List.iter all ~f:(fun x -> assert_true (valid_trace g [x]))
  );

  let goods = [
    line, [a;b];
    line, [b;c];
    line, [c;d];
    line, [a;b;c];
    line, [b;c;d];
    line, [a;b;c;d];

    square, [a;b];
    square, [b;c];
    square, [c;d];
    square, [d;a];
    square, [a;b;c];
    square, [b;c;d];
    square, [c;d;a];
    square, [d;a;b];
    square, [a;b;c;d];
    square, [b;c;d;a];
    square, [c;d;a;b];
    square, [d;a;b;c];

    diamond, [a;b];
    diamond, [a;c];
    diamond, [a;b;d];
    diamond, [a;c;d];
    diamond, [b;d];

    clique, [a;b;c;d];
    clique, [a;b;d;c];
    clique, [a;c;b;d];
    clique, [a;c;d;b];
    clique, [a;d;b;c];
    clique, [a;d;c;b];
    clique, [b;a;c;d];
    clique, [b;a;d;c];
    clique, [b;c;a;d];
    clique, [b;c;d;a];
    clique, [b;d;a;c];
    clique, [b;d;c;a];
    clique, [c;a;b;d];
    clique, [c;a;d;b];
    clique, [c;b;a;d];
    clique, [c;b;d;a];
    clique, [c;d;a;b];
    clique, [c;d;b;a];
    clique, [d;a;b;c];
    clique, [d;a;c;b];
    clique, [d;b;a;c];
    clique, [d;b;c;a];
    clique, [d;c;a;b];
    clique, [d;c;b;a];

    pairs, [a;b];
    pairs, [b;a];
    pairs, [c;d];
    pairs, [d;c];

    weird, [a;b];
    weird, [a;c];
    weird, [a;b;d];
    weird, [a;b;c];
    weird, [a;c;d];
    weird, [b;d;a];
    weird, [c;d;a];
  ] in
  List.iter goods ~f:(fun (g, t) -> assert_true (valid_trace g t));

  (* In all graphs, empty hop paths are bad. *)
  List.iter graphs ~f:(fun g -> assert_false (valid_trace g []));

  (* In all graphs, duplicates are bad. *)
  List.iter graphs ~f:(fun g ->
    List.iter all ~f:(fun x -> assert_false (valid_trace g [x;x]));
    List.iter all ~f:(fun x -> assert_false (valid_trace g [x;x;x]));
    List.iter all ~f:(fun x -> assert_false (valid_trace g [x;x;x;x]));
  );

  let bads = [
    line, [a;c];
    line, [a;d];
    line, [b;d];
    line, [a;c;d];
    line, [a;b;d];
    line, [b;a];
    line, [c;a];
    line, [d;a];

    line, [a;a;c];
    line, [a;a;d];
    line, [a;b;d];
    line, [a;a;c;d];
    line, [a;a;b;d];
    line, [a;b;a];
    line, [a;c;a];
    line, [a;d;a];

    square, [a;c];
    square, [a;d];
    square, [b;d];
    square, [a;c;d];
    square, [a;b;d];
    square, [b;a];
    square, [c;a];
  ] in
  List.iter bads ~f:(fun (g, t) -> assert_false (valid_trace g t))

let test_get_trace () =
  let open Graphs in
  List.iter graphs ~f:(fun g ->
    List.iter all ~f:(fun x ->
      assert_true (valid_trace g (find_trace g (get_node g x)))
    )
  )

let test_valid_seq () =
  let open Graphs in

  (* In all graphs, single hop seqs are good. *)
  List.iter graphs ~f:(fun g ->
    let all_seq = List.map ~f:(fun x -> [x]) all in
    assert_true (valid_seq g all_seq)
  );

  let goods = [
    line, [[a];[b;c;d]];
    line, [[a;b];[c;d]];
    line, [[a];[b;c;d]];
    line, [[a;b];[c];[d]];
    line, [[a];[b;c];[d]];
    line, [[a];[b];[c;d]];
  ] in
  List.iter goods ~f:(fun (g, s) -> assert_true (valid_seq g s));

  let bads = [
    line, [[b;c;d];[a]];
    line, [[c;d];[a;b]];
    line, [[b;c;d];[a]];
    line, [[c];[d];[a;b]];
    line, [[b;c];[d];[a]];
    line, [[b];[c;d];[a]];
    line, [[];[b;c;d]];
    line, [[a];[c;d]];
    line, [[a];[b;d]];
    line, [[a];[b;c]];
    line, [[a];[b;c;d;d]];
    line, [[a;b];[b;c;d]];
  ] in
  List.iter bads ~f:(fun (g, s) -> assert_false (valid_seq g s));
  ()

let test_find_seq () =
  let open Graphs in
  List.iter graphs ~f:(fun g ->
    List.iter all ~f:(fun x ->
      assert_true (valid_seq g (find_seq g))
    )
  )

let test_tidy () =
  let open Labels in
  let open BlocksEq in
  let open Ir.Abbreviations in
  let open Ir.Infix in

  let blocks = [
  ] in
  let expected = [
  ] in
  expected === tidy blocks;

  let blocks = [
    block label0 [exp one];
  ] in
  let expected = [
    block label0 [exp one];
  ] in
  expected === tidy blocks;

  let blocks = [
    block label0 [exp one; exp two];
  ] in
  let expected = [
    block label0 [exp one; exp two];
  ] in
  expected === tidy blocks;

  let blocks = [
    block label0 [jump (name label1)];
    block label1 [exp one];
  ] in
  let expected = [
    block label0 [];
    block label1 [exp one];
  ] in
  expected === tidy blocks;

  let blocks = [
    block label0 [jump (name label2)];
    block label1 [exp one];
  ] in
  let expected = [
    block label0 [jump (name label2)];
    block label1 [exp one];
  ] in
  expected === tidy blocks;

  let blocks = [
    block label0 [cjump one label2 label1];
    block label1 [exp one];
  ] in
  let expected = [
    block label0 [cjumpone one label2];
    block label1 [exp one];
  ] in
  expected === tidy blocks;

  let blocks = [
    block label0 [cjump one label1 label2];
    block label1 [exp one];
  ] in
  let expected = [
    block label0 [cjumpone ((one + one) % two) label2];
    block label1 [exp one];
  ] in
  expected === tidy blocks;

  let blocks = [
    block label0 [cjump one label2 label3];
    block label1 [exp one];
  ] in
  let expected = [
    block label0 [jump (name label3); cjumpone one label2];
    block label1 [exp one];
  ] in
  expected === tidy blocks;

  let blocks = [
    block label0 [return];
    block label1 [exp one];
  ] in
  let expected = [
    block label0 [return];
    block label1 [exp one];
  ] in
  expected === tidy blocks;

  let blocks = [
    block label0 [cjump one label2 label3];
  ] in
  let expected = [
    block label0 [jump (name label3); cjumpone one label2];
  ] in
  expected === tidy blocks;

  ()

let test_reorder () =
  let open Labels in
  let open BlocksEq in
  let open Ir.Abbreviations in
  let open Ir.Infix in

  reset_fresh_label ();

  (* Test *)
  let stmts = [
    l1; cjump one label2 label3;
    l2; cjump one label2 label4;
    l3; jump (name label2);
    l4; jump (name label5);
    l5; return;
  ] in

  let expected = [
    block label1 [cjumpone one label2];
    block label3 [];
    block label2 [cjumpone one label2];
    block label4 [];
    block label5 [return];
  ] in

  expected === (block_reorder stmts);

  (* testing to make sure that last block in sequence properly jumps to
   * epilogue after reordering *)
  let stmts = [
    l1; jump (name label2);
    l2; cjump one label3 label4;
    l3;
    l4; jump (name label5);
    l5;
  ] in

  let expected = [
    block label1 [];
    block label2 [cjumpone one label3];
    block label4 [];
    block label5 [return];
    block label3 [jump (name label4)];
  ] in

  expected === (block_reorder stmts);

  (* Test *)
  let stmts = [
    l1; jump (name label2);
    l2; cjump one label3 label4;
    l3; jump (name label5);
    l4;
    l5;
  ] in

  let expected = [
    block label1 [];
    block label2 [cjumpone one label3];
    block label4 [];
    block label5 [return];
    block label3 [jump (name label5)];
  ] in

  expected === (block_reorder stmts);

  (* testing for reversing the boolean *)
  let stmts = [
    l1; cjump one label2 label3;
    l2; cjump one label4 label3;
    l4; move one two;
        move one zero;
    l3; return;
  ] in

  let expected = [
    block label1 [cjumpone one label2];
    block label3 [return];
    block label2 [cjumpone ((one + one) % two) label3];
    block label4 [move one two; move one zero; jump (name label3)];
  ] in

  expected === (block_reorder stmts);

  (* test case in powerpoint slides:
   * http://www.cs.cornell.edu/courses/cs4120/2013fa/lectures/lec17-fa13.pdf *)
  let stmts = [
        cjump one label2 label3;
    l2; move one two;
        jump (name label1);
    l1; move zero one;
        jump (name label2);
    l3; exp one;
  ] in

  let expected = [
    block label0 [cjumpone one label2];
    block label3 [exp one; return];
    block label2 [move one two];
    block label1 [move zero one; jump (name label2)];
  ] in

  expected === (block_reorder stmts);

  (* testing generating fresh labels *)
  reset_fresh_label ();

  let stmts = [
         cjump one label20 label30;
    l20; move one two;
         jump (name label30);
         move zero one;
         jump (name label20);
    l30; exp one;
  ] in

  let expected = [
    block label0 [cjumpone one label20];
    block label30 [exp one; return];
    block label20 [move one two; jump (name label30)];
    block label1 [move zero one; jump (name label20)];
  ] in

  expected === (block_reorder stmts);

  (* testing fresh labels / adding jump to epilogue at the end / no accidentaly
   * infinite loops *)
  reset_fresh_label ();

  let stmts = [
         jump (name label10);
         jump (name label10);
    l10;
  ] in

  let expected = [
    block label0 [];
    block label10 [return];
    block label1 [jump (name label10)];
  ] in

  expected === (block_reorder stmts);

  ()

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* ! DON'T FORGET TO ADD YOUR TESTS HERE                                     ! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let main () =
    "suite" >::: [
      "test_ir_expr"        >:: test_ir_expr;
			"test_ir_stmt" 			  >:: test_ir_stmt;
      "test_lower_expr"     >:: test_lower_expr;
      "test_lower_stmt"     >:: test_lower_stmt;
      "test_gen_block"      >:: test_gen_block;
      "test_connect_blocks" >:: test_connect_blocks;
      "test_create_graph"   >:: test_create_graph;
      "test_valid_trace"    >:: test_valid_trace;
      "test_get_trace"      >:: test_get_trace;
      "test_valid_seq"      >:: test_valid_seq;
      "test_find_seq"       >:: test_find_seq;
      "test_tidy"           >:: test_tidy;
      "test_reorder"        >:: test_reorder;
    ] |> run_test_tt_main

let _ = main ()
