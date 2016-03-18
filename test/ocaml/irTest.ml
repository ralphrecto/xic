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
    "\n" ^ (String.concat ~sep:"\n" (List.map ~f:string_of_block bs))

  let (===) (a: block list) (b: block list) : unit =
    assert_equal ~printer:string_of_blocks a b
end


(**** HELPER FUNCTIONS ***)
let int x  = (IntT, Int x)
let bool b = (BoolT, Bool b)
let binop i e1 op e2 : Typecheck.expr = (i, BinOp (e1, op, e2))
let ibinop e1 op e2 = binop IntT e1 op e2
let bbinop e1 op e2 = binop BoolT e1 op e2
let unop i op e : Typecheck.expr = (i, UnOp (op, e))
let iunop e = unop IntT UMINUS e
let bunop e = unop BoolT BANG e


let test_ir_expr () =
  let open ExprEq in

  (* Ir exprs *)
  let zero = Const 0L in
  let one  = Const 1L in
  let two  = Const 2L in
  let tru  = one in
  let fls  = zero in
  let binop00 = BinOp (zero, ADD, zero) in
  let binop12 = BinOp (one, ADD, two) in
  let unopminus x = BinOp (zero, SUB, x) in
  let unopnot x = BinOp (BinOp (x, ADD, one), MOD, two) in

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
  (*=== gen_expr (ArrayT IntT, String "OCaml <3") *)

  (* Array Indexing tests *)

  (* Length tests *)

  (* FuncCall tests *)

  ()

let test_lower_expr () =
  let open PairEq in
  let open Fresh in

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
end

let test_reorder () =
  let open Labels in
  let open BlocksEq in
  let open Ir.Abbreviations in
  let open Ir.Infix in

  let zero = const 0L in
  let one  = const 1L in
  let two  = const 2L in

  let block l ss = Block (l, ss) in
  let epilogue = block "done" [] in

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
    block label5 [return; jump (name "done")];
    epilogue;
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
    block label5 [jump (name "done")];
    block label3 [jump (name label4)];
    epilogue;
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
    block label5 [jump (name "done")];
    block label3 [jump (name label5)];
    epilogue
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
    block label3 [return; jump (name "done")];
    block label2 [cjumpone ((one + one) % two) label3];
    block label4 [move one two; move one zero; jump (name label3)];
    epilogue
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
    block label3 [exp one; jump (name "done")];
    block label2 [move one two];
    block label1 [move zero one; jump (name label2)];
    epilogue
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
    block label30 [exp one; jump (name "done")];
    block label20 [move one two; jump (name label30)];
    block label1 [move zero one; jump (name label20)];
    epilogue
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
    block label10 [jump (name "done")];
    block label1 [jump (name label10)];
    epilogue
  ] in

  expected === (block_reorder stmts);

  ()


(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* ! DON'T FORGET TO ADD YOUR TESTS HERE                                     ! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let main () =
    "suite" >::: [
      "test_ir_expr"    >:: test_ir_expr;
      "test_lower_expr" >:: test_lower_expr;
      "test_lower_stmt" >:: test_lower_stmt;
      "test_reorder" >:: test_reorder;
    ] |> run_test_tt_main

let _ = main ()
