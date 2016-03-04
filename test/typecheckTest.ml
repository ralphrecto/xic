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
    let err = begin
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
    end in
    match err with
    | Ok _ -> ()
    | Error (_, s) -> begin
        printf ">>> %s =: %s errored with %s\n" (Ast.string_of_expr e) (to_string t) s;
        assert_true false
    end

  let (=/=) (c: context) (e: Pos.expr) : unit =
    begin
      expr_typecheck c e >>| fun e' ->
      printf ">>> %s : %s; expected error\n" (Ast.string_of_expr e')
                                             (to_string (fst e'));
      assert_true false
    end
    |> is_error
    |> assert_true
end

module TestCallable = struct
	let (=:) ((c, e): context * Pos.callable) (func_t: Expr.t * Expr.t) : unit =
		let b = is_ok (fst_func_pass c e >>= fun gamma ->
                                     match Result.error (snd_func_pass gamma e) with
                                     |Some (_,s) -> printf "%s" s; Ok ()
                                     |None -> Ok () ) in
                                     (*
									 snd_func_pass gamma e >>= fun (t,_) ->
									 Ok (assert_equal t func_t)) in *)
		assert_true b

  let (=/=) (c: context) (e: Pos.callable) : unit =
    begin
      fst_func_pass c e >>= fun gamma ->
			snd_func_pass gamma e
    end
    |> is_error
    |> assert_true
end

module TestStmt = struct
  let is_equal c c' =
    let is_subset c c' =
        Context.fold c' ~init:true ~f:(fun ~key:k ~data:v a ->
            match Context.find c k with
            | None    -> false
            | Some v' -> (v = v') && a)
    in
    is_subset c c' && is_subset c' c

  let (=:) ((c, r), s: (context * Expr.t) * Pos.stmt) (t, c': Stmt.t * context) : unit =
    begin
      stmt_typecheck c r s >>| fun s' ->
      let t' = fst s' in
      match (t' = t), (is_equal c c') with
      | true, true -> ()
      | true, false -> printf ">>> The same context is not returned for a stmt"
      | false, _ -> begin
          printf ">>> Error T_T\n"; (* TODO: string_of_stmt *)
                                   (*(to_string t)
                                   (to_string t');*)
          assert_equal t' t
        end
    end
    |> is_ok
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

    empty =/= (arr[one] == arr[tru]);
    empty =/= (arr[tru] == arr[one]);
    empty =/= (arr[one] == arr[arr[one]]);
    empty =/= (arr[arr[one]] == arr[one]);
    empty =/= (arr[tru] == arr[arr[one]]);
    empty =/= (arr[arr[tru]] == arr[one]);
    empty =/= (arr[one] != arr[tru]);
    empty =/= (arr[tru] != arr[one]);
    empty =/= (arr[one] != arr[arr[one]]);
    empty =/= (arr[arr[one]] != arr[one]);
    empty =/= (arr[tru] != arr[arr[one]]);
    empty =/= (arr[arr[tru]] != arr[one]);

    empty |- (length (arr[])) =: IntT;
    empty |- (length (arr[one])) =: IntT;
    empty =/= (length one);
    empty =/= (length tru);

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

    empty =/= (arr[one;tru]);
    empty =/= (arr[tru;one]);
    empty =/= (arr[one;tru;one]);
    empty =/= (arr[tru;one;tru]);
    empty =/= (arr[one;one;tru]);
    empty =/= (arr[tru;tru;one]);
    empty =/= (arr[one;arr[]]);
    empty =/= (arr[arr[];one]);
    empty =/= (arr[one;arr[];one]);
    empty =/= (arr[arr[];one;arr[]]);
    empty =/= (arr[one;one;arr[]]);
    empty =/= (arr[one;arr[];arr[]]);
    empty =/= (arr[arr[one];arr[tru]]);
    empty =/= (arr[arr[tru];arr[one]]);
    empty =/= (arr[arr[one];arr[tru];arr[one]]);
    empty =/= (arr[arr[tru];arr[one];arr[tru]]);
    empty =/= (arr[arr[one];arr[one];arr[tru]]);
    empty =/= (arr[arr[tru];arr[tru];arr[one]]);
    empty =/= (arr[arr[one];arr[arr[]]]);
    empty =/= (arr[arr[arr[]];arr[one]]);
    empty =/= (arr[arr[one];arr[arr[]];arr[one]]);
    empty =/= (arr[arr[arr[]];arr[one];arr[arr[]]]);
    empty =/= (arr[arr[one];arr[one];arr[arr[]]]);
    empty =/= (arr[arr[one];arr[arr[]];arr[arr[]]]);

    empty |- (index (arr[one]) one) =: IntT;
    empty |- (index (arr[tru]) one) =: BoolT;
    empty |- (index (arr[arr[one]]) one) =: ArrayT IntT;
    empty |- (index (arr[arr[one]]) one) =: ArrayT IntT;
    empty |- (index (arr[arr[arr[tru]]]) one) =: ArrayT (ArrayT BoolT);
    empty |- (index (arr[arr[arr[tru]]]) one) =: ArrayT (ArrayT BoolT);

    empty =/= (index (arr[one]) tru);
    empty =/= (index (arr[tru]) tru);
    empty =/= (index (arr[arr[one]]) tru);
    empty =/= (index (arr[arr[one]]) tru);
    empty =/= (index (arr[arr[arr[tru]]]) tru);
    empty =/= (index (arr[arr[arr[tru]]]) tru);
    empty =/= (index (arr[one]) (arr[]));
    empty =/= (index (arr[tru]) (arr[]));
    empty =/= (index (arr[arr[one]]) (arr[]));
    empty =/= (index (arr[arr[one]]) (arr[]));
    empty =/= (index (arr[arr[arr[tru]]]) (arr[]));
    empty =/= (index (arr[arr[arr[tru]]]) (arr[]));
    empty =/= (index tru one);

    empty |- (arr[] + arr[]) =: EmptyArray;
    empty |- (arr[] + arr[one]) =: ArrayT IntT;
    empty |- (arr[one] + arr[]) =: ArrayT IntT;
    empty |- (arr[one] + arr[one]) =: ArrayT IntT;
    empty |- (arr[one] + arr[one]) =: ArrayT IntT;
    empty |- (arr[one;one] + arr[one]) =: ArrayT IntT;
    empty |- (arr[one] + arr[one;one]) =: ArrayT IntT;
    empty |- (arr[one;one] + arr[one;one]) =: ArrayT IntT;
    empty |- (arr[arr[]] + arr[]) =: ArrayT EmptyArray;
    empty |- (arr[arr[arr[]]] + arr[]) =: ArrayT (ArrayT EmptyArray);
    empty |- (arr[arr[arr[arr[]]]] + arr[]) =: ArrayT (ArrayT (ArrayT EmptyArray));
    empty |- (arr[arr[]] + arr[arr[]]) =: ArrayT EmptyArray;
    empty |- (arr[arr[arr[]]] + arr[arr[]]) =: ArrayT (ArrayT EmptyArray);
    empty |- (arr[arr[arr[arr[]]]] + arr[arr[]]) =: ArrayT (ArrayT (ArrayT EmptyArray));
    empty |- (arr[arr[arr[]]] + arr[arr[arr[]]]) =: ArrayT (ArrayT EmptyArray);
    empty |- (arr[arr[arr[arr[]]]] + arr[arr[arr[]]]) =: ArrayT (ArrayT (ArrayT EmptyArray));
    empty |- (arr[arr[]] + arr[arr[tru]]) =: ArrayT (ArrayT BoolT);
    empty |- (arr[arr[]] + arr[arr[arr[tru]]]) =: ArrayT (ArrayT (ArrayT BoolT));
    empty |- (arr[arr[arr[]]] + arr[arr[arr[tru]]]) =: ArrayT (ArrayT (ArrayT BoolT));

    ()

let test_stmt () =
    let open Pos in
    let open TestStmt in

    (* One is unit, Zero is void *)
    (* Decl *)
    (empty, UnitT) |- decl [avar (aid "x" tint)] =: (One, empty);
    (empty, UnitT) |- decl [avar (aid "y" tbool)] =: (One, empty);
    (empty, UnitT) |- decl [underscore] =: (One, empty);
    (empty, UnitT) |- decl [avar (aunderscore tint)] =: (One, empty);
    (empty, UnitT) |- decl [avar (aunderscore (tarray tbool None))] =: (One, empty);
    (* DeclAsgn *)
    (* Asgn *)
    (* Block *)
    (* Return *)
    (* If *)
    (* IfElse *)
    (* While *)
    (* ProcCall *)

    ()

let test_callable () =
	let open Pos in
	let open TestCallable in
	empty |- (func "id" [(aid "x" tint)] [tint] (return [id "x"])) =: (IntT, IntT);
	empty |- (func "id" [(aid "x" tbool)] [tbool] (return [id "x"])) =: (BoolT, BoolT);
	empty |- (func "id" [(aid "x" (tarray tint None))] [(tarray tint None)] (return [id "x"]))
						=: (ArrayT IntT, ArrayT IntT);
	empty |- (func "f" [(aid "x" tint); (aid "y" tint)] [tint] (return [id "x"])) =: (TupleT [IntT; IntT], IntT);
	empty |- (func "f" [(aid "x" tint); (aid "y" tint)] [tint] (return [id "y"])) =: (TupleT [IntT; IntT], IntT);
	empty |- (func "f" [(aid "x" tint); (aid "y" tint)] [tint;tint] (return [(id "y"); (id "x")]))
						=: (TupleT [IntT; IntT], TupleT [IntT; IntT]);
	empty |- (func "f" [(aid "x" tint); (aid "y" tbool)] [tbool;tint] (return [(id "y"); (id "x")]))
						=: (TupleT [IntT; BoolT], TupleT [BoolT; IntT]);
	empty |- (func "g" [aid "x" tint] [tint] (block [(asgn (id "x") (funccall "g" [id "x"])); (return [id "x"])]))
							=: (IntT, IntT);
	empty |- (func "f" [] [tint] (return [int 3L])) =: (UnitT, IntT);
	empty |- (func "f" [] [tint] (return [(funccall "f" [])])) =: (UnitT, IntT);
	empty |- (func "f" [] [tint; tint] (return [int 3L; int 2L])) =: (UnitT, TupleT [IntT; IntT]);

	empty =/= (func "f" [aid "x" tint] [tbool] (return [id "x"]));
	empty =/= (func "has_dup" [(aid "x" tint); (aid "x" tint)] [tint] (return [id "x"]));
	empty =/= (func "f" [aid "x" tint] [tint] (return [id "y"]));

	let f_binded = Context.bind empty "f" (Function (IntT, IntT)) in
	f_binded =/= (func "f" [aid "x" tint] [tint] (return [id "x"]));
	f_binded =/= (func "f" [aid "x" tbool] [tbool] (return [id "x"]));

	f_binded |- (func "g" [aid "x" tint] [tint] (return [id "x"])) =: (IntT, IntT);
	f_binded |- (func "g" [aid "x" tint] [tint] (block [(asgn (id "x") (funccall "f" [id "x"])); (return [id "x"])]))
							=: (IntT, IntT);
	f_binded |- (func "g" [aid "x" tint] [tint] (block [(asgn (id "x") (funccall "g" [id "x"])); (return [id "x"])]))
							=: (IntT, IntT);
	()

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* ! DON'T FORGET TO ADD YOUR TESTS HERE                                     ! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let main () =
    "suite" >::: [
        "test_expr" >:: test_expr;
        "test_stmt" >:: test_stmt;
		"test_callable" >:: test_callable;
    ] |> run_test_tt_main

let _ = main ()

