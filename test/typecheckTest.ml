open Core.Std
open Typecheck
open OUnit
open Expr
open Stmt
open Sigma
open Ast.S

let (>>=) = Result.(>>=)
let (>>|) = Result.(>>|)

let assert_true (b: bool) : unit =
  assert_equal b true

(* Dummy pos *)
let empty = Context.empty

let (|-) c e = (c, e)

module TestExpr = struct
  (* If <: is subtype, then =: is equal type. *)
  let (=:) ((c, e): context * Pos.expr) (t: Expr.t) : unit =
    let b = is_ok (expr_typecheck c e >>| fun e' -> assert_equal (fst e') t) in
    assert_true b
end

module TestCallable = struct
	let (=:) ((c, e): context * Pos.callable) (func_t: Expr.t * Expr.t) : unit =
		let b = is_ok (fst_func_pass c e >>= fun gamma ->
									 snd_func_pass gamma e >>= fun (t,_) -> Ok (assert_equal t func_t)) in
		assert_true b
end

let test_expr () =
    let open Pos in
    let open TestExpr in
    let one = int 1L in
    let two = int 1L in
    let tru = bool true in
    let fls = bool false in

    empty |- one =: IntT;
    empty |- tru =: BoolT;
    empty |- fls =: BoolT;
    empty |- string "a" =: ArrayT IntT;
    empty |- char 'c' =: IntT;
    empty |- (one + two) =: IntT;
    empty |- (one - two) =: IntT;
    empty |- (one * two) =: IntT;
    empty |- (one *>> two) =: IntT;
    empty |- (one / two) =: IntT;
    empty |- (one % two) =: IntT;
    empty |- (~~ one) =: IntT;
    empty |- (one == two) =: BoolT;
    empty |- (one != two) =: BoolT;
    empty |- (one < two) =: BoolT;
    empty |- (one <= two) =: BoolT;
    empty |- (one > two) =: BoolT;
    empty |- (one >= two) =: BoolT;
    empty |- (!tru) =: BoolT;
    empty |- (tru == fls) =: BoolT;
    empty |- (tru != fls) =: BoolT;
    empty |- (tru & fls) =: BoolT;
    empty |- (tru || fls) =: BoolT;
    ()

let test_callable () =
	let open Pos in
	let open TestCallable in
	let id_int = func "id" [(aid "x" tint)] [tint] (return [id "x"]) in
	empty |- id_int =: (IntT, IntT);
	let id_bool = func "id" [(aid "x" tbool)] [tbool] (return [id "x"]) in
	empty |- id_bool =: (BoolT, BoolT);
	let id_array = func "id" [(aid "x" (tarray tint None))] [(tarray tint None)] (return [id "x"]) in
	empty |- id_array =: (ArrayT IntT, ArrayT IntT);
	()
 
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* ! DON'T FORGET TO ADD YOUR TESTS HERE                                     ! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let main () =
    "suite" >::: [
        "test_expr" >:: test_expr;
    ] |> run_test_tt_main

let _ = main ()

