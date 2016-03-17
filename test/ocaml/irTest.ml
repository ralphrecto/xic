open Core.Std
open Typecheck.Expr
open Ast.S
open Ir
open Ir_generation
open OUnit
open TestUtil

module Fresh = struct
  let t n =
    Temp (sprintf "temp%d" n)

  let l n =
    Label (sprintf "label%d" n)
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

let test_reorder () =
	
	(* helper function to print blocks to debug *)
	let print_blocks blocks = List.fold_left ~init:() blocks 
													~f: (fun _ (Block (l, ss)) -> Printf.printf "Block (%s, %s)\n" l (string_of_stmts ss)); 
													print_endline "";
	in
	
	(* labels *)
  let l1 = Ir.Label "label1" in
  let l2 = Ir.Label "label2" in
  let l3 = Ir.Label "label3" in
  let l4 = Ir.Label "label4" in
  let l5 = Ir.Label "label5" in

  (* statements *)
  let s1 = CJump (Const 1L, "label2", "label3") in
  let s2 = CJump (Const 1L, "label2", "label4") in
  let s3 = Jump (Name "label2") in
  let s4 = Jump (Name "label5") in
  let s5 = Return in

  let s_list = [l1; s1; l2; s2; l3; s3; l4; s4; l5; s5] in

  let reordered1 = block_reorder s_list in

  (* blocks *)
  let b1 = Block ("label1", [CJumpOne (Const 1L, "label2")]) in
  let b2 = Block ("label3", []) in
  let b3 = Block ("label2", [CJumpOne (Const 1L, "label2")]) in
  let b4 = Block ("label4", []) in
  let b5 = Block ("label5", [Return; Jump (Name "done")]) in
	let epilogue = Block ("done", []) in


  let expected1 = [b1; b2; b3; b4; b5; epilogue] in

	(* testing to make sure that last block in sequence properly jumps to epilogue
		 after reordering *)
	
  (* labels *)
  let l1 = Ir.Label "label1" in
  let l2 = Ir.Label "label2" in
  let l3 = Ir.Label "label3" in
  let l4 = Ir.Label "label4" in
  let l5 = Ir.Label "label5" in

  (* statements *)
  let s1 = Jump (Name "label2") in
  let s2 = CJump (Const 1L, "label3", "label4") in
	let s3 = Jump (Name "label5") in

  let s_list = [l1; s1; l2; s2; l3; l4; s3; l5] in

  let reordered2 = block_reorder s_list in

  (* blocks *)
  let b1 = Block ("label1", []) in
  let b2 = Block ("label2", [CJumpOne(Const 1L, "label3")]) in
  let b3 = Block ("label4", []) in
  let b4 = Block ("label5", [Jump (Name "done")]) in
  let b5 = Block ("label3", [Jump (Name "label4")]) in
	let epilogue = Block ("done", []) in

	let expected2 = [b1; b2; b3; b4; b5; epilogue] in

	(* labels *)
  let l1 = Ir.Label "label1" in
  let l2 = Ir.Label "label2" in
  let l3 = Ir.Label "label3" in
  let l4 = Ir.Label "label4" in
  let l5 = Ir.Label "label5" in

  (* statements *)
  let s1 = Jump (Name "label2") in
  let s2 = CJump (Const 1L, "label3", "label4") in
	let s3 = Jump (Name "label5") in

  let s_list = [l1; s1; l2; s2; l3; s3; l4; l5] in

  let reordered3 = block_reorder s_list in

  (* blocks *)
  let b1 = Block ("label1", []) in
  let b2 = Block ("label2", [CJumpOne(Const 1L, "label3")]) in
  let b3 = Block ("label4", []) in
  let b4 = Block ("label5", [Jump (Name "done")]) in
  let b5 = Block ("label3", [Jump (Name "label5")]) in
	let epilogue = Block ("done", []) in

	let expected3 = [b1; b2; b3; b4; b5; epilogue] in

	(* testing for reversing the boolean *)

	(* labels *)
  let l1 = Ir.Label "label1" in
  let l2 = Ir.Label "label2" in
  let l3 = Ir.Label "label3" in
  let l4 = Ir.Label "label4" in

  (* statements *)
	let s1 = CJump (Const 1L, "label2", "label3") in
	let s2 = CJump (Const 1L, "label4", "label3") in 
	let s3 = Move (Const 1L, Const 2L) in
	let s4 = Move (Const 1L, Const 0L) in
	let s5 = Return in 

  let s_list = [l1; s1; l2; s2; l4; s3; s4; l3; s5] in

  let reordered4 = block_reorder s_list in

	(* test case in powerpoint slides: 
		http://www.cs.cornell.edu/courses/cs4120/2013fa/lectures/lec17-fa13.pdf *)

  (* blocks *)
  let b1 = Block ("label1", [CJumpOne (Const 1L, "label2")]) in
	let b2 = Block ("label3", [Return; Jump (Name "done")]) in
	let b3 = Block ("label2", [CJumpOne (BinOp (BinOp (Const 1L, ADD, Const 1L), MOD, Const 2L), "label3")]) in
  let b4 = Block ("label4", [Move (Const 1L, Const 2L); Move (Const 1L, Const 0L); Jump (Name "label3")]) in
	let epilogue = Block ("done", []) in

	let expected4 = [b1; b2; b3; b4; epilogue] in

	(* labels *)
  let l1 = Ir.Label "label1" in
  let l2 = Ir.Label "label2" in
  let l3 = Ir.Label "label3" in

  (* statements *)
	let s1 = CJump (Const 1L, "label2", "label3") in
	let s2 = Move (Const 1L, Const 2L) in
	let s3 = Jump (Name "label1") in
	let s4 = Move (Const 0L, Const 1L) in
	let s5 = Jump (Name "label2") in
	let s6 = Exp (Const 1L) in

  let s_list = [s1; l2; s2; s3; l1; s4; s5; l3; s6] in

  let reordered5 = block_reorder s_list in

  (* blocks *)
	let b1 = Block ("label0", [CJumpOne (Const 1L, "label2")]) in
	let b2 = Block ("label3", [Exp (Const 1L); Jump (Name "done")]) in
	let b3 = Block ("label2", [Move (Const 1L, Const 2L)]) in
	let b4 = Block ("label1", [Move (Const 0L, Const 1L); Jump (Name "label2")]) in
	let epilogue = Block ("done", []) in

	let expected5 = [b1; b2; b3; b4; epilogue] in

	(* testing generating fresh labels *)

	(* labels *)
  let l2 = Ir.Label "label20" in
  let l3 = Ir.Label "label30" in

  (* statements *)
	let s1 = CJump (Const 1L, "label20", "label30") in
	let s2 = Move (Const 1L, Const 2L) in
	let s3 = Jump (Name "label30") in
	let s4 = Move (Const 0L, Const 1L) in
	let s5 = Jump (Name "label20") in
	let s6 = Exp (Const 1L) in

  let s_list = [s1; l2; s2; s3; s4; s5; l3; s6] in

  let reordered6 = block_reorder s_list in

  (* blocks *)
	let b1 = Block ("label1", [CJumpOne (Const 1L, "label20")]) in
	let b2 = Block ("label30", [Exp (Const 1L); Jump (Name "done")]) in
	let b3 = Block ("label20", [Move (Const 1L, Const 2L); Jump (Name "label30")]) in
	let b4 = Block ("label2", [Move (Const 0L, Const 1L); Jump (Name "label20")]) in
	let epilogue = Block ("done", []) in

	let expected6 = [b1; b2; b3; b4; epilogue] in
  
	assert_equal reordered1 expected1;
	assert_equal reordered2 expected2;
	assert_equal reordered3 expected3;
	assert_equal reordered4 expected4;
	assert_equal reordered5 expected5;
	assert_equal reordered6 expected6

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
