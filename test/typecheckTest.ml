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
    begin
      expr_typecheck c e >>| fun e' ->
      let t' = fst e' in
      if t' = t then
        ()
      else begin
        printf ">>> %s : %s != %s\n" (Ast.string_of_expr e')
                                     (to_string t')
                                     (to_string t);
        assert_equal t' t
      end
    end
    |> is_ok
    |> assert_true

  let (=/=) (c: context) (e: Pos.expr) : unit =
    begin
      expr_typecheck c e >>| fun e' ->
      printf ">>> %s : %s; expected error" (Ast.string_of_expr e')
                                           (to_string (fst e'))
    end
    |> is_error
    |> assert_true
end

module TestCallable = struct
	let (=:) ((c, e): context * Pos.callable) (func_t: Expr.t * Expr.t) : unit =
		let b = is_ok (fst_func_pass c e >>= fun gamma ->
									 match Result.error (snd_func_pass gamma e) with
									 |Some (e,s) -> let _ = printf "%s" s in Error (e,s)
									 | None -> Ok ()
									 (*>>= fun (t,_) -> 
									 Ok (assert_equal t func_t)*)) in
		assert_true b

  let (=/=) (c: context) (e: Pos.callable) : unit =
    begin
      fst_func_pass c e >>= fun gamma ->
			snd_func_pass gamma e
    end
    |> is_error
    |> assert_true
end

let one   = Pos.(int 1L)
let two   = Pos.(int 1L)
let three = Pos.(int 1L)
let tru   = Pos.(bool true)
let fls   = Pos.(bool false)

let test_expr () =
    let open Pos in
    let open TestExpr in

    empty |- (one) =: IntT;
    empty |- (tru) =: BoolT;
    empty |- (fls) =: BoolT;
    empty |- (string "a") =: ArrayT IntT;
    empty |- (char 'c') =: IntT;

    empty |- (one + two) =: IntT;
    empty |- (one - two) =: IntT;
    empty |- (one * two) =: IntT;
    empty |- (one *>> two) =: IntT;
    empty |- (one / two) =: IntT;
    empty |- (one % two) =: IntT;
    empty |- (~~ one) =: IntT;

    empty =/= (tru + two);
    empty =/= (tru - two);
    empty =/= (tru * two);
    empty =/= (tru *>> two);
    empty =/= (tru / two);
    empty =/= (tru % two);
    empty =/= (one + tru);
    empty =/= (one - tru);
    empty =/= (one * tru);
    empty =/= (one *>> tru);
    empty =/= (one / tru);
    empty =/= (one % tru);
    empty =/= (tru + tru);
    empty =/= (tru - tru);
    empty =/= (tru * tru);
    empty =/= (tru *>> tru);
    empty =/= (tru / tru);
    empty =/= (tru % tru);
    empty =/= (~~ tru);

    empty |- (one == two) =: BoolT;
    empty |- (one != two) =: BoolT;
    empty |- (one < two) =: BoolT;
    empty |- (one <= two) =: BoolT;
    empty |- (one > two) =: BoolT;
    empty |- (one >= two) =: BoolT;
    empty |- (!tru) =: BoolT;

    empty =/= (tru < two);
    empty =/= (tru <= two);
    empty =/= (tru > two);
    empty =/= (tru >= two);
    empty =/= (one < tru);
    empty =/= (one <= tru);
    empty =/= (one > tru);
    empty =/= (one >= tru);
    empty =/= (tru < tru);
    empty =/= (tru <= tru);
    empty =/= (tru > tru);
    empty =/= (tru >= tru);
    empty =/= (!one);

    empty |- (tru == fls) =: BoolT;
    empty |- (tru != fls) =: BoolT;
    empty |- (tru & fls) =: BoolT;
    empty |- (tru || fls) =: BoolT;

    empty =/= (one & fls);
    empty =/= (one || fls);
    empty =/= (tru & one);
    empty =/= (tru || one);
    empty =/= (one & one);
    empty =/= (one || one);

    empty |- (arr[]    == arr[]) =: BoolT;
    empty |- (arr[one] == arr[]) =: BoolT;
    empty |- (arr[]    == arr[one]) =: BoolT;
    empty |- (arr[one] == arr[one]) =: BoolT;
    empty |- (arr[]    != arr[]) =: BoolT;
    empty |- (arr[one] != arr[]) =: BoolT;
    empty |- (arr[]    != arr[one]) =: BoolT;
    empty |- (arr[one] != arr[one]) =: BoolT;

    empty |- (length (arr[])) =: IntT;
    empty |- (length (arr[one])) =: IntT;
    empty |- (arr []) =: EmptyArray;
    empty |- (arr [one]) =: ArrayT IntT;
    empty |- (arr [one; two]) =: ArrayT IntT;
    empty |- (arr [one; two; three]) =: ArrayT IntT;
    empty |- (arr [one; two; three; one]) =: ArrayT IntT;
    empty |- (arr [tru]) =: ArrayT BoolT;
    empty |- (arr [tru; fls]) =: ArrayT BoolT;
    empty |- (arr [tru; fls; tru]) =: ArrayT BoolT;
    empty |- (arr [tru; fls; tru; fls]) =: ArrayT BoolT;
    empty |- (arr [arr[tru]]) =: ArrayT (ArrayT BoolT);
    empty |- (arr [arr[tru]; arr[tru]]) =: ArrayT (ArrayT BoolT);
    empty |- (arr [arr[]; arr[tru]]) =: ArrayT (ArrayT BoolT);
    empty |- (arr [arr[]; arr[tru]; arr[tru]]) =: ArrayT (ArrayT BoolT);
    empty |- (arr [arr[tru]; arr[]]) =: ArrayT (ArrayT BoolT);
    empty |- (arr [arr[tru]; arr[]; arr[tru]]) =: ArrayT (ArrayT BoolT);
    empty |- (arr [arr[tru]; arr[tru]; arr[]]) =: ArrayT (ArrayT BoolT);
    empty |- (arr [arr[tru]]) =: ArrayT (ArrayT BoolT);
    empty |- (arr [arr[tru]; arr[tru]]) =: ArrayT (ArrayT BoolT);
    empty |- (arr [arr[]; arr[tru]]) =: ArrayT (ArrayT BoolT);
    empty |- (arr [arr[]; arr[tru]; arr[tru]]) =: ArrayT (ArrayT BoolT);
    empty |- (arr [arr[tru]; arr[]]) =: ArrayT (ArrayT BoolT);
    empty |- (arr [arr[tru]; arr[]; arr[tru]]) =: ArrayT (ArrayT BoolT);
    empty |- (arr [arr[tru]; arr[tru]; arr[]]) =: ArrayT (ArrayT BoolT);
    empty |- (arr [arr [arr[tru]]]) =: ArrayT (ArrayT (ArrayT BoolT));
    empty |- (arr [arr [arr[tru]; arr[tru]]]) =: ArrayT (ArrayT (ArrayT BoolT));
    empty |- (arr [arr [arr[]; arr[tru]]]) =: ArrayT (ArrayT (ArrayT BoolT));
    empty |- (arr [arr [arr[]; arr[tru]; arr[tru]]]) =: ArrayT (ArrayT (ArrayT BoolT));
    empty |- (arr [arr [arr[tru]; arr[]]]) =: ArrayT (ArrayT (ArrayT BoolT));
    empty |- (arr [arr [arr[tru]; arr[]; arr[tru]]]) =: ArrayT (ArrayT (ArrayT BoolT));
    empty |- (arr [arr [arr[tru]; arr[tru]; arr[]]]) =: ArrayT (ArrayT (ArrayT BoolT));

    empty |- (index (arr[one]) one) =: IntT;
    empty |- (index (arr[tru]) one) =: BoolT;
    empty |- (index (arr[arr[one]]) one) =: ArrayT IntT;
    empty |- (index (arr[arr[one]]) one) =: ArrayT IntT;
    empty |- (index (arr[arr[arr[tru]]]) one) =: ArrayT (ArrayT BoolT);
    empty |- (index (arr[arr[arr[tru]]]) one) =: ArrayT (ArrayT BoolT);

    ()

let test_callable () =
	let open Pos in
	let open TestCallable in
	empty =/= (func "id" [(aid "x" tint)] [tint] (return [id "x"]));
	empty |- (func "id" [(aid "x" tint)] [tint] (return [id "x"])) =: (IntT, IntT);
	(*empty |- (func "id" [(aid "x" tbool)] [tbool] (return [id "x"])) =: (BoolT, BoolT);
	empty |- (func "id" [(aid "x" (tarray tint None))] [(tarray tint None)] (return [id "x"]))  
						=: (ArrayT IntT, ArrayT IntT);
	empty =/= (func "has_dup" [(aid "x" tint); (aid "x" tint)] [tint] (return [id "x"]));*)
	()
 
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* ! DON'T FORGET TO ADD YOUR TESTS HERE                                     ! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let main () =
    "suite" >::: [
        "test_expr" >:: test_expr;
				"test_callable" >:: test_callable;
    ] |> run_test_tt_main

let _ = main ()

