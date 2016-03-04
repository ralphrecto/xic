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

(* Context helpers *)
let empty = Context.empty
let gam (xs: (string * Sigma.t) list) =
  Context.of_alist_exn xs

let vars (vs: (string * Expr.t) list) =
  gam (List.map vs ~f:(fun (v, t) -> (v, Var t)))

let funcs (fs: (string * Expr.t * Expr.t) list) =
  gam (List.map fs ~f:(fun (v, a, b) -> (v, Function (a, b))))

module Vars = struct
  open Pos
  let a = id "a"
  let b = id "b"
  let c = id "c"
  let x = id "x"
  let y = id "y"
  let z = id "z"

  (* 0 -> 0/1 *)
  let u2u  = ("u2u", UnitT, UnitT)
  let u2i  = ("u2i", UnitT, IntT)
  let u2b  = ("u2b", UnitT, BoolT)
  let u2ia = ("u2ia", UnitT, ArrayT IntT)

  (* 1 -> 0/1 *)
  let i2u   = ("i2u",   IntT,        UnitT)
  let i2i   = ("i2i",   IntT,        IntT)
  let i2b   = ("i2b",   IntT,        BoolT)
  let i2ia  = ("i2ia",  IntT,        ArrayT IntT)
  let b2u   = ("b2u",   BoolT,       UnitT)
  let b2i   = ("b2i",   BoolT,       IntT)
  let b2b   = ("b2b",   BoolT,       BoolT)
  let b2ia  = ("b2ia",  BoolT,       ArrayT IntT)
  let ia2u  = ("ia2u",  ArrayT IntT, UnitT)
  let ia2i  = ("ia2i",  ArrayT IntT, IntT)
  let ia2b  = ("ia2b",  ArrayT IntT, BoolT)
  let ia2ia = ("ia2ia", ArrayT IntT, ArrayT IntT)

  (* 2 -> 0/1 *)
  let ii2u   = ("ii2u",   TupleT [IntT; IntT],        UnitT)
  let ii2i   = ("ii2i",   TupleT [IntT; IntT],        IntT)
  let ii2b   = ("ii2b",   TupleT [IntT; IntT],        BoolT)
  let ii2ia  = ("ii2ia",  TupleT [IntT; IntT],        ArrayT IntT)
  let ib2u   = ("ib2u",   TupleT [IntT; BoolT],       UnitT)
  let ib2i   = ("ib2i",   TupleT [IntT; BoolT],       IntT)
  let ib2b   = ("ib2b",   TupleT [IntT; BoolT],       BoolT)
  let ib2ia  = ("ib2ia",  TupleT [IntT; BoolT],       ArrayT IntT)
  let iia2u  = ("iia2u",  TupleT [IntT; ArrayT IntT], UnitT)
  let iia2i  = ("iia2i",  TupleT [IntT; ArrayT IntT], IntT)
  let iia2b  = ("iia2b",  TupleT [IntT; ArrayT IntT], BoolT)
  let iia2ia = ("iia2ia", TupleT [IntT; ArrayT IntT], ArrayT IntT)

  let bi2u   = ("bi2u",   TupleT [BoolT; IntT],        UnitT)
  let bi2i   = ("bi2i",   TupleT [BoolT; IntT],        IntT)
  let bi2b   = ("bi2b",   TupleT [BoolT; IntT],        BoolT)
  let bi2ia  = ("bi2ia",  TupleT [BoolT; IntT],        ArrayT IntT)
  let bb2u   = ("bb2u",   TupleT [BoolT; BoolT],       UnitT)
  let bb2i   = ("bb2i",   TupleT [BoolT; BoolT],       IntT)
  let bb2b   = ("bb2b",   TupleT [BoolT; BoolT],       BoolT)
  let bb2ia  = ("bb2ia",  TupleT [BoolT; BoolT],       ArrayT IntT)
  let bia2u  = ("bia2u",  TupleT [BoolT; ArrayT IntT], UnitT)
  let bia2i  = ("bia2i",  TupleT [BoolT; ArrayT IntT], IntT)
  let bia2b  = ("bia2b",  TupleT [BoolT; ArrayT IntT], BoolT)
  let bia2ia = ("bia2ia", TupleT [BoolT; ArrayT IntT], ArrayT IntT)

  let iai2u   = ("iai2u",   TupleT [ArrayT IntT; IntT],        UnitT)
  let iai2i   = ("iai2i",   TupleT [ArrayT IntT; IntT],        IntT)
  let iai2b   = ("iai2b",   TupleT [ArrayT IntT; IntT],        BoolT)
  let iai2ia  = ("iai2ia",  TupleT [ArrayT IntT; IntT],        ArrayT IntT)
  let iab2u   = ("iab2u",   TupleT [ArrayT IntT; BoolT],       UnitT)
  let iab2i   = ("iab2i",   TupleT [ArrayT IntT; BoolT],       IntT)
  let iab2b   = ("iab2b",   TupleT [ArrayT IntT; BoolT],       BoolT)
  let iab2ia  = ("iab2ia",  TupleT [ArrayT IntT; BoolT],       ArrayT IntT)
  let iaia2u  = ("iaia2u",  TupleT [ArrayT IntT; ArrayT IntT], UnitT)
  let iaia2i  = ("iaia2i",  TupleT [ArrayT IntT; ArrayT IntT], IntT)
  let iaia2b  = ("iaia2b",  TupleT [ArrayT IntT; ArrayT IntT], BoolT)
  let iaia2ia = ("iaia2ia", TupleT [ArrayT IntT; ArrayT IntT], ArrayT IntT)

  (* 1 -> 2 *)
  let i2ii   = ("i2ii",   IntT,        TupleT [IntT; IntT])
  let i2ib   = ("i2ib",   IntT,        TupleT [IntT; BoolT])
  let i2iia  = ("i2iia",  IntT,        TupleT [IntT; ArrayT IntT])
  let b2ii   = ("b2ii",   BoolT,       TupleT [IntT; IntT])
  let b2ib   = ("b2ib",   BoolT,       TupleT [IntT; BoolT])
  let b2iia  = ("b2iia",  BoolT,       TupleT [IntT; ArrayT IntT])
  let ia2ii  = ("ia2ii",  ArrayT IntT, TupleT [IntT; IntT])
  let ia2ib  = ("ia2ib",  ArrayT IntT, TupleT [IntT; BoolT])
  let ia2iia = ("ia2iia", ArrayT IntT, TupleT [IntT; ArrayT IntT])

  let i2bi   = ("i2bi",   IntT,        TupleT [BoolT; IntT])
  let i2bb   = ("i2bb",   IntT,        TupleT [BoolT; BoolT])
  let i2bia  = ("i2bia",  IntT,        TupleT [BoolT; ArrayT IntT])
  let b2bi   = ("b2bi",   BoolT,       TupleT [BoolT; IntT])
  let b2bb   = ("b2bb",   BoolT,       TupleT [BoolT; BoolT])
  let b2bia  = ("b2bia",  BoolT,       TupleT [BoolT; ArrayT IntT])
  let ia2bi  = ("ia2bi",  ArrayT IntT, TupleT [BoolT; IntT])
  let ia2bb  = ("ia2bb",  ArrayT IntT, TupleT [BoolT; BoolT])
  let ia2bia = ("ia2bia", ArrayT IntT, TupleT [BoolT; ArrayT IntT])

  let i2iai   = ("i2iai",   IntT,        TupleT [ArrayT IntT; IntT])
  let i2iab   = ("i2iab",   IntT,        TupleT [ArrayT IntT; BoolT])
  let i2iaia  = ("i2iaia",  IntT,        TupleT [ArrayT IntT; ArrayT IntT])
  let b2iai   = ("b2iai",   BoolT,       TupleT [ArrayT IntT; IntT])
  let b2iab   = ("b2iab",   BoolT,       TupleT [ArrayT IntT; BoolT])
  let b2iaia  = ("b2iaia",  BoolT,       TupleT [ArrayT IntT; ArrayT IntT])
  let ia2iai  = ("ia2iai",  ArrayT IntT, TupleT [ArrayT IntT; IntT])
  let ia2iab  = ("ia2iab",  ArrayT IntT, TupleT [ArrayT IntT; BoolT])
  let ia2iaia = ("ia2iaia", ArrayT IntT, TupleT [ArrayT IntT; ArrayT IntT])

  (* 2 -> 2 *)
  let ii2ii   = ("ii2ii",   TupleT [IntT; IntT],        TupleT [IntT; IntT])
  let ii2ib   = ("ii2ib",   TupleT [IntT; IntT],        TupleT [IntT; BoolT])
  let ii2iia  = ("ii2iia",  TupleT [IntT; IntT],        TupleT [IntT; ArrayT IntT])
  let ib2ii   = ("ib2ii",   TupleT [IntT; BoolT],       TupleT [IntT; IntT])
  let ib2ib   = ("ib2ib",   TupleT [IntT; BoolT],       TupleT [IntT; BoolT])
  let ib2iia  = ("ib2iia",  TupleT [IntT; BoolT],       TupleT [IntT; ArrayT IntT])
  let iia2ii  = ("iia2ii",  TupleT [IntT; ArrayT IntT], TupleT [IntT; IntT])
  let iia2ib  = ("iia2ib",  TupleT [IntT; ArrayT IntT], TupleT [IntT; BoolT])
  let iia2iia = ("iia2iia", TupleT [IntT; ArrayT IntT], TupleT [IntT; ArrayT IntT])

  let bi2bi   = ("bi2bi",   TupleT [BoolT; IntT],        TupleT [BoolT; IntT])
  let bi2bb   = ("bi2bb",   TupleT [BoolT; IntT],        TupleT [BoolT; BoolT])
  let bi2bia  = ("bi2bia",  TupleT [BoolT; IntT],        TupleT [BoolT; ArrayT IntT])
  let bb2bi   = ("bb2bi",   TupleT [BoolT; BoolT],       TupleT [BoolT; IntT])
  let bb2bb   = ("bb2bb",   TupleT [BoolT; BoolT],       TupleT [BoolT; BoolT])
  let bb2bia  = ("bb2bia",  TupleT [BoolT; BoolT],       TupleT [BoolT; ArrayT IntT])
  let bia2bi  = ("bia2bi",  TupleT [BoolT; ArrayT IntT], TupleT [BoolT; IntT])
  let bia2bb  = ("bia2bb",  TupleT [BoolT; ArrayT IntT], TupleT [BoolT; BoolT])
  let bia2bia = ("bia2bia", TupleT [BoolT; ArrayT IntT], TupleT [BoolT; ArrayT IntT])

  let iai2iai   = ("iai2iai",   TupleT [ArrayT IntT; IntT],        TupleT [ArrayT IntT; IntT])
  let iai2iab   = ("iai2iab",   TupleT [ArrayT IntT; IntT],        TupleT [ArrayT IntT; BoolT])
  let iai2iaia  = ("iai2iaia",  TupleT [ArrayT IntT; IntT],        TupleT [ArrayT IntT; ArrayT IntT])
  let iab2iai   = ("iab2iai",   TupleT [ArrayT IntT; BoolT],       TupleT [ArrayT IntT; IntT])
  let iab2iab   = ("iab2iab",   TupleT [ArrayT IntT; BoolT],       TupleT [ArrayT IntT; BoolT])
  let iab2iaia  = ("iab2iaia",  TupleT [ArrayT IntT; BoolT],       TupleT [ArrayT IntT; ArrayT IntT])
  let iaia2iai  = ("iaia2iai",  TupleT [ArrayT IntT; ArrayT IntT], TupleT [ArrayT IntT; IntT])
  let iaia2iab  = ("iaia2iab",  TupleT [ArrayT IntT; ArrayT IntT], TupleT [ArrayT IntT; BoolT])
  let iaia2iaia = ("iaia2iaia", TupleT [ArrayT IntT; ArrayT IntT], TupleT [ArrayT IntT; ArrayT IntT])

  let iaup = ("iaup", TupleT [ArrayT IntT; ArrayT (ArrayT IntT); ArrayT (ArrayT (ArrayT IntT))], IntT)
  let iadown = ("iadown", TupleT [ArrayT (ArrayT (ArrayT IntT)); ArrayT (ArrayT IntT); ArrayT IntT], IntT)

  let fgam = funcs [
    u2u; u2i; u2b; u2ia; i2u; i2i; i2b; i2ia; b2u; b2i; b2b; b2ia; ia2u; ia2i;
    ia2b; ia2ia; ii2u; ii2i; ii2b; ii2ia; ib2u; ib2i; ib2b; ib2ia; iia2u;
    iia2i; iia2b; iia2ia; bi2u; bi2i; bi2b; bi2ia; bb2u; bb2i; bb2b; bb2ia;
    bia2u; bia2i; bia2b; bia2ia; iai2u; iai2i; iai2b; iai2ia; iab2u; iab2i;
    iab2b; iab2ia; iaia2u; iaia2i; iaia2b; iaia2ia; i2ii; i2ib; i2iia; b2ii;
    b2ib; b2iia; ia2ii; ia2ib; ia2iia; i2bi; i2bb; i2bia; b2bi; b2bb; b2bia;
    ia2bi; ia2bb; ia2bia; i2iai; i2iab; i2iaia; b2iai; b2iab; b2iaia; ia2iai;
    ia2iab; ia2iaia; ii2ii; ii2ib; ii2iia; ib2ii; ib2ib; ib2iia; iia2ii;
    iia2ib; iia2iia; bi2bi; bi2bb; bi2bia; bb2bi; bb2bb; bb2bia; bia2bi;
    bia2bb; bia2bia; iai2iai; iai2iab; iai2iaia; iab2iai; iab2iab; iab2iaia;
    iaia2iai; iaia2iab; iaia2iaia;

    iaup; iadown
  ]
end

let (|-) c e = (c, e)

module TestExpr = struct
  (* If <: is subtype, then =: is equal type. *)
  let (=:) ((c, e): context * Pos.expr) (t: Expr.t) : unit =
    match expr_typecheck c e with
    | Ok e' -> begin
      let t' = fst e' in
      if t' <> t then begin
        printf ">>> %s : %s != %s\n" (Ast.string_of_expr e') (to_string t') (to_string t);
        assert_true false
      end
    end
    | Error (_, s) -> begin
        printf ">>> %s =: %s errored with %s\n" (Ast.string_of_expr e) (to_string t) s;
        assert_true false
    end

  let (=/=) (c: context) (e: Pos.expr) : unit =
    match expr_typecheck c e with
    | Ok e' -> begin
        printf ">>> %s : %s; expected error\n" (Ast.string_of_expr e') (to_string (fst e'));
        assert_true false
    end
    | Error _ -> ()
end

module TestCallable = struct
	let (=:) ((c, e): context * Pos.callable) (func_t: Expr.t * Expr.t) : unit =
		let b = is_ok (func_typecheck c e >>= fun gamma ->
									 match snd_func_pass gamma e with
									 | Ok (t, _) -> Ok (assert_equal t func_t)
									 | Error (p, s) -> printf "%s" s;  Error (p, s)) in
		assert_true b

  let (=/=) (c: context) (e: Pos.callable) : unit =
    begin
      func_typecheck c e >>= fun gamma ->
			snd_func_pass gamma e
    end
    |> is_error
    |> assert_true
end

module TestStmt = struct
  let (=:) (((c, r), s): (context * Expr.t) * Pos.stmt) (t: Stmt.t) : unit =
    match stmt_typecheck c r s with
    | Ok s' -> begin
      let t' = fst s' in
      if t' <> t then begin
        printf ">>> %s : %s != %s\n"
          (Sexp.to_string (Pos.sexp_of_stmt s))
          (Sexp.to_string (Stmt.sexp_of_t t'))
          (Sexp.to_string (Stmt.sexp_of_t t));
        assert_true false
      end
    end
    | Error (_, msg) -> begin
        printf ">>> %s =: %s errored with %s\n"
          (Sexp.to_string (Pos.sexp_of_stmt s))
          (Sexp.to_string (Stmt.sexp_of_t t))
          msg;
        assert_true false
    end
end

let one   = Pos.(int 1L)
let two   = Pos.(int 1L)
let three = Pos.(int 1L)
let tru   = Pos.(bool true)
let fls   = Pos.(bool false)

let test_expr () =
    let open Pos in
    let open Vars in
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
    empty |- (arr[one] + arr[one;one]) =: ArrayT IntT;
    empty |- (arr[arr[two];arr[one]] + arr[arr[one;one]]) =: ArrayT (ArrayT IntT);
    empty |- (arr[arr[arr[two];arr[one]]] + arr[arr[arr[one;one]]]) =: ArrayT (ArrayT (ArrayT IntT));

    empty =/= (arr[one] + arr[tru]);
    empty =/= (arr[arr[one]] + arr[tru]);
    empty =/= (arr[one] + arr[arr[tru]]);
    empty =/= (arr[arr[one]] + arr[arr[tru]]);
    empty =/= (arr[arr[arr[one]]] + arr[arr[tru]]);
    empty =/= (arr[arr[one]] + arr[arr[arr[tru]]]);
    empty =/= (arr[arr[arr[one]]] + arr[arr[arr[tru]]]);

    vars["x",IntT] |- (x) =: IntT;
    vars["x",BoolT] |- (x) =: BoolT;
    vars["x",EmptyArray] |- (x) =: EmptyArray;
    vars["x",ArrayT IntT] |- (x) =: ArrayT IntT;
    vars["x",ArrayT (ArrayT IntT)] |- (x) =: ArrayT (ArrayT IntT);
    vars["x",ArrayT (ArrayT BoolT)] |- (x) =: ArrayT (ArrayT BoolT);
    vars["x",ArrayT (ArrayT EmptyArray)] |- (x) =: ArrayT (ArrayT EmptyArray);
    vars["x",IntT; "y",IntT] |- (x) =: IntT;
    vars["x",BoolT; "y",IntT] |- (x) =: BoolT;
    vars["x",EmptyArray; "y",IntT] |- (x) =: EmptyArray;
    vars["x",ArrayT IntT; "y",IntT] |- (x) =: ArrayT IntT;
    vars["x",IntT; "y",BoolT] |- (x) =: IntT;
    vars["x",BoolT; "y",BoolT] |- (x) =: BoolT;
    vars["x",EmptyArray; "y",BoolT] |- (x) =: EmptyArray;
    vars["x",ArrayT IntT; "y",BoolT] |- (x) =: ArrayT IntT;
    vars["x",IntT; "y",IntT] |- (y) =: IntT;
    vars["x",BoolT; "y",IntT] |- (y) =: IntT;
    vars["x",EmptyArray; "y",IntT] |- (y) =: IntT;
    vars["x",ArrayT IntT; "y",IntT] |- (y) =: IntT;
    vars["x",IntT; "y",BoolT] |- (y) =: BoolT;
    vars["x",BoolT; "y",BoolT] |- (y) =: BoolT;
    vars["x",EmptyArray; "y",BoolT] |- (y) =: BoolT;
    vars["x",ArrayT IntT; "y",BoolT] |- (y) =: BoolT;
    vars["x",IntT; "y",BoolT; "z",EmptyArray] |- (x) =: IntT;
    vars["x",IntT; "y",BoolT; "z",EmptyArray] |- (y) =: BoolT;
    vars["x",IntT; "y",BoolT; "z",EmptyArray] |- (z) =: EmptyArray;

    empty =/= (x);
    empty =/= (y);
    vars["x",BoolT] =/= (x + one);
    vars["x",BoolT] =/= (x - one);
    vars["x",BoolT] =/= (x / one);

    fgam |- (funccall "u2i" []) =: IntT;
    fgam |- (funccall "u2b" []) =: BoolT;
    fgam |- (funccall "u2ia" []) =: ArrayT IntT;
    fgam |- (funccall "i2i" [one]) =: IntT;
    fgam |- (funccall "i2b" [one]) =: BoolT;
    fgam |- (funccall "i2ia" [one]) =: ArrayT IntT;
    fgam |- (funccall "b2i" [tru]) =: IntT;
    fgam |- (funccall "b2b" [tru]) =: BoolT;
    fgam |- (funccall "b2ia" [tru]) =: ArrayT IntT;
    fgam |- (funccall "ia2i" [arr[one]]) =: IntT;
    fgam |- (funccall "ia2b" [arr[one]]) =: BoolT;
    fgam |- (funccall "ia2ia" [arr[one]]) =: ArrayT IntT;
    fgam |- (funccall "ii2i" [one;one]) =: IntT;
    fgam |- (funccall "ii2b" [one;one]) =: BoolT;
    fgam |- (funccall "ii2ia" [one;one]) =: ArrayT IntT;
    fgam |- (funccall "ib2i" [one;tru]) =: IntT;
    fgam |- (funccall "ib2b" [one;tru]) =: BoolT;
    fgam |- (funccall "ib2ia" [one;tru]) =: ArrayT IntT;
    fgam |- (funccall "iia2i" [one;arr[one]]) =: IntT;
    fgam |- (funccall "iia2b" [one;arr[one]]) =: BoolT;
    fgam |- (funccall "iia2ia" [one;arr[one]]) =: ArrayT IntT;
    fgam |- (funccall "bi2i" [tru;one]) =: IntT;
    fgam |- (funccall "bi2b" [tru;one]) =: BoolT;
    fgam |- (funccall "bi2ia" [tru;one]) =: ArrayT IntT;
    fgam |- (funccall "bb2i" [tru;tru]) =: IntT;
    fgam |- (funccall "bb2b" [tru;tru]) =: BoolT;
    fgam |- (funccall "bb2ia" [tru;tru]) =: ArrayT IntT;
    fgam |- (funccall "bia2i" [tru;arr[one]]) =: IntT;
    fgam |- (funccall "bia2b" [tru;arr[one]]) =: BoolT;
    fgam |- (funccall "bia2ia" [tru;arr[one]]) =: ArrayT IntT;
    fgam |- (funccall "iai2i" [arr[one];one]) =: IntT;
    fgam |- (funccall "iai2b" [arr[one];one]) =: BoolT;
    fgam |- (funccall "iai2ia" [arr[one];one]) =: ArrayT IntT;
    fgam |- (funccall "iab2i" [arr[one];tru]) =: IntT;
    fgam |- (funccall "iab2b" [arr[one];tru]) =: BoolT;
    fgam |- (funccall "iab2ia" [arr[one];tru]) =: ArrayT IntT;
    fgam |- (funccall "iaia2i" [arr[one];arr[one]]) =: IntT;
    fgam |- (funccall "iaia2b" [arr[one];arr[one]]) =: BoolT;
    fgam |- (funccall "iaia2ia" [arr[one];arr[one]]) =: ArrayT IntT;
    fgam |- (funccall "i2ii" [one]) =: TupleT[IntT;IntT];
    fgam |- (funccall "i2ib" [one]) =: TupleT[IntT;BoolT];
    fgam |- (funccall "i2iia" [one]) =: TupleT[IntT;ArrayT IntT];
    fgam |- (funccall "b2ii" [tru]) =: TupleT[IntT;IntT];
    fgam |- (funccall "b2ib" [tru]) =: TupleT[IntT;BoolT];
    fgam |- (funccall "b2iia" [tru]) =: TupleT[IntT;ArrayT IntT];
    fgam |- (funccall "ia2ii" [arr[one]]) =: TupleT[IntT;IntT];
    fgam |- (funccall "ia2ib" [arr[one]]) =: TupleT[IntT;BoolT];
    fgam |- (funccall "ia2iia" [arr[one]]) =: TupleT[IntT;ArrayT IntT];
    fgam |- (funccall "i2bi" [one]) =: TupleT[BoolT;IntT];
    fgam |- (funccall "i2bb" [one]) =: TupleT[BoolT;BoolT];
    fgam |- (funccall "i2bia" [one]) =: TupleT[BoolT;ArrayT IntT];
    fgam |- (funccall "b2bi" [tru]) =: TupleT[BoolT;IntT];
    fgam |- (funccall "b2bb" [tru]) =: TupleT[BoolT;BoolT];
    fgam |- (funccall "b2bia" [tru]) =: TupleT[BoolT;ArrayT IntT];
    fgam |- (funccall "ia2bi" [arr[one]]) =: TupleT[BoolT;IntT];
    fgam |- (funccall "ia2bb" [arr[one]]) =: TupleT[BoolT;BoolT];
    fgam |- (funccall "ia2bia" [arr[one]]) =: TupleT[BoolT;ArrayT IntT];
    fgam |- (funccall "i2iai" [one]) =: TupleT[ArrayT IntT;IntT];
    fgam |- (funccall "i2iab" [one]) =: TupleT[ArrayT IntT;BoolT];
    fgam |- (funccall "i2iaia" [one]) =: TupleT[ArrayT IntT;ArrayT IntT];
    fgam |- (funccall "b2iai" [tru]) =: TupleT[ArrayT IntT;IntT];
    fgam |- (funccall "b2iab" [tru]) =: TupleT[ArrayT IntT;BoolT];
    fgam |- (funccall "b2iaia" [tru]) =: TupleT[ArrayT IntT;ArrayT IntT];
    fgam |- (funccall "ia2iai" [arr[one]]) =: TupleT[ArrayT IntT;IntT];
    fgam |- (funccall "ia2iab" [arr[one]]) =: TupleT[ArrayT IntT;BoolT];
    fgam |- (funccall "ia2iaia" [arr[one]]) =: TupleT[ArrayT IntT;ArrayT IntT];
    fgam |- (funccall "ii2ii" [one;one]) =: TupleT[IntT;IntT];
    fgam |- (funccall "ii2ib" [one;one]) =: TupleT[IntT;BoolT];
    fgam |- (funccall "ii2iia" [one;one]) =: TupleT[IntT;ArrayT IntT];
    fgam |- (funccall "ib2ii" [one;tru]) =: TupleT[IntT;IntT];
    fgam |- (funccall "ib2ib" [one;tru]) =: TupleT[IntT;BoolT];
    fgam |- (funccall "ib2iia" [one;tru]) =: TupleT[IntT;ArrayT IntT];
    fgam |- (funccall "iia2ii" [one;arr[one]]) =: TupleT[IntT;IntT];
    fgam |- (funccall "iia2ib" [one;arr[one]]) =: TupleT[IntT;BoolT];
    fgam |- (funccall "iia2iia" [one;arr[one]]) =: TupleT[IntT;ArrayT IntT];
    fgam |- (funccall "bi2bi" [tru;one]) =: TupleT[BoolT;IntT];
    fgam |- (funccall "bi2bb" [tru;one]) =: TupleT[BoolT;BoolT];
    fgam |- (funccall "bi2bia" [tru;one]) =: TupleT[BoolT;ArrayT IntT];
    fgam |- (funccall "bb2bi" [tru;tru]) =: TupleT[BoolT;IntT];
    fgam |- (funccall "bb2bb" [tru;tru]) =: TupleT[BoolT;BoolT];
    fgam |- (funccall "bb2bia" [tru;tru]) =: TupleT[BoolT;ArrayT IntT];
    fgam |- (funccall "bia2bi" [tru;arr[one]]) =: TupleT[BoolT;IntT];
    fgam |- (funccall "bia2bb" [tru;arr[one]]) =: TupleT[BoolT;BoolT];
    fgam |- (funccall "bia2bia" [tru;arr[one]]) =: TupleT[BoolT;ArrayT IntT];
    fgam |- (funccall "iai2iai" [arr[one];one]) =: TupleT[ArrayT IntT;IntT];
    fgam |- (funccall "iai2iab" [arr[one];one]) =: TupleT[ArrayT IntT;BoolT];
    fgam |- (funccall "iai2iaia" [arr[one];one]) =: TupleT[ArrayT IntT;ArrayT IntT];
    fgam |- (funccall "iab2iai" [arr[one];tru]) =: TupleT[ArrayT IntT;IntT];
    fgam |- (funccall "iab2iab" [arr[one];tru]) =: TupleT[ArrayT IntT;BoolT];
    fgam |- (funccall "iab2iaia" [arr[one];tru]) =: TupleT[ArrayT IntT;ArrayT IntT];
    fgam |- (funccall "iaia2iai" [arr[one];arr[one]]) =: TupleT[ArrayT IntT;IntT];
    fgam |- (funccall "iaia2iab" [arr[one];arr[one]]) =: TupleT[ArrayT IntT;BoolT];

    fgam |- (funccall "iaia2i" [arr[];arr[one]]) =: IntT;
    fgam |- (funccall "iaia2i" [arr[one];arr[]]) =: IntT;
    fgam |- (funccall "iaia2i" [arr[];arr[]]) =: IntT;

    fgam |- (funccall "iaup" [arr[]; arr[]; arr[]]) =: IntT;
    fgam |- (funccall "iaup" [arr[]; arr[arr[]]; arr[]]) =: IntT;
    fgam |- (funccall "iaup" [arr[]; arr[]; arr[arr[]]]) =: IntT;
    fgam |- (funccall "iaup" [arr[]; arr[arr[]]; arr[arr[]]]) =: IntT;
    fgam |- (funccall "iaup" [arr[]; arr[arr[]]; arr[arr[arr[]]]]) =: IntT;
    fgam |- (funccall "iaup" [arr[one]; arr[]; arr[]]) =: IntT;
    fgam |- (funccall "iaup" [arr[one]; arr[arr[]]; arr[]]) =: IntT;
    fgam |- (funccall "iaup" [arr[one]; arr[]; arr[arr[]]]) =: IntT;
    fgam |- (funccall "iaup" [arr[one]; arr[arr[]]; arr[arr[]]]) =: IntT;
    fgam |- (funccall "iaup" [arr[one]; arr[arr[]]; arr[arr[arr[]]]]) =: IntT;
    fgam |- (funccall "iaup" [arr[]; arr[arr[one]]; arr[]]) =: IntT;
    fgam |- (funccall "iaup" [arr[]; arr[arr[one]]; arr[arr[]]]) =: IntT;
    fgam |- (funccall "iaup" [arr[]; arr[arr[one]]; arr[arr[arr[]]]]) =: IntT;
    fgam |- (funccall "iaup" [arr[]; arr[arr[]]; arr[arr[arr[one]]]]) =: IntT;
    fgam |- (funccall "iaup" [arr[one]; arr[arr[one]]; arr[arr[arr[one]]]]) =: IntT;

    fgam |- (funccall "iadown" (List.rev [arr[]; arr[]; arr[]])) =: IntT;
    fgam |- (funccall "iadown" (List.rev [arr[]; arr[arr[]]; arr[]])) =: IntT;
    fgam |- (funccall "iadown" (List.rev [arr[]; arr[]; arr[arr[]]])) =: IntT;
    fgam |- (funccall "iadown" (List.rev [arr[]; arr[arr[]]; arr[arr[]]])) =: IntT;
    fgam |- (funccall "iadown" (List.rev [arr[]; arr[arr[]]; arr[arr[arr[]]]])) =: IntT;
    fgam |- (funccall "iadown" (List.rev [arr[one]; arr[]; arr[]])) =: IntT;
    fgam |- (funccall "iadown" (List.rev [arr[one]; arr[arr[]]; arr[]])) =: IntT;
    fgam |- (funccall "iadown" (List.rev [arr[one]; arr[]; arr[arr[]]])) =: IntT;
    fgam |- (funccall "iadown" (List.rev [arr[one]; arr[arr[]]; arr[arr[]]])) =: IntT;
    fgam |- (funccall "iadown" (List.rev [arr[one]; arr[arr[]]; arr[arr[arr[]]]])) =: IntT;
    fgam |- (funccall "iadown" (List.rev [arr[]; arr[arr[one]]; arr[]])) =: IntT;
    fgam |- (funccall "iadown" (List.rev [arr[]; arr[arr[one]]; arr[arr[]]])) =: IntT;
    fgam |- (funccall "iadown" (List.rev [arr[]; arr[arr[one]]; arr[arr[arr[]]]])) =: IntT;
    fgam |- (funccall "iadown" (List.rev [arr[]; arr[arr[]]; arr[arr[arr[one]]]])) =: IntT;
    fgam |- (funccall "iadown" (List.rev [arr[one]; arr[arr[one]]; arr[arr[arr[one]]]])) =: IntT;

    fgam =/= (funccall "u2u" []);
    fgam =/= (funccall "i2u" [one]);
    fgam =/= (funccall "b2u" [tru]);
    fgam =/= (funccall "ia2u" [arr[one]]);
    fgam =/= (funccall "ii2u" [one; one]);
    fgam =/= (funccall "ib2u" [one; tru]);
    fgam =/= (funccall "iia2u" [one; arr[one]]);
    fgam =/= (funccall "bi2u" [tru; one]);
    fgam =/= (funccall "bia2u" [tru; arr[one]]);
    fgam =/= (funccall "bb2u" [tru; tru]);
    fgam =/= (funccall "iai2u" [arr[one]; one]);
    fgam =/= (funccall "iab2u" [arr[one]; tru]);
    fgam =/= (funccall "iaia2u" [arr[one]; arr[one]]);
    fgam =/= (funccall "u2u" [arr[]]);
    fgam =/= (funccall "i2u" [arr[]]);
    fgam =/= (funccall "i2u" [arr[arr[]]]);

    fgam =/= (funccall "u2i" [one]);
    fgam =/= (funccall "u2b" [one]);
    fgam =/= (funccall "u2ia" [one]);
    fgam =/= (funccall "i2i" [one; one]);
    fgam =/= (funccall "i2b" [one; one]);
    fgam =/= (funccall "i2ia" [one; one]);
    fgam =/= (funccall "b2i" [one; tru]);
    fgam =/= (funccall "b2b" [one; tru]);
    fgam =/= (funccall "b2ia" [one; tru]);
    fgam =/= (funccall "ia2i" [one; arr[one]]);
    fgam =/= (funccall "ia2b" [one; arr[one]]);
    fgam =/= (funccall "ia2ia" [one; arr[one]]);
    fgam =/= (funccall "ii2i" [one; one;one]);
    fgam =/= (funccall "ii2b" [one; one;one]);
    fgam =/= (funccall "ii2ia" [one; one;one]);
    fgam =/= (funccall "ib2i" [one; one;tru]);
    fgam =/= (funccall "ib2b" [one; one;tru]);
    fgam =/= (funccall "ib2ia" [one; one;tru]);
    fgam =/= (funccall "iia2i" [one; one;arr[one]]);
    fgam =/= (funccall "iia2b" [one; one;arr[one]]);
    fgam =/= (funccall "iia2ia" [one; one;arr[one]]);
    fgam =/= (funccall "bi2i" [one; tru;one]);
    fgam =/= (funccall "bi2b" [one; tru;one]);
    fgam =/= (funccall "bi2ia" [one; tru;one]);
    fgam =/= (funccall "bb2i" [one; tru;tru]);
    fgam =/= (funccall "bb2b" [one; tru;tru]);
    fgam =/= (funccall "bb2ia" [one; tru;tru]);
    fgam =/= (funccall "bia2i" [one; tru;arr[one]]);
    fgam =/= (funccall "bia2b" [one; tru;arr[one]]);
    fgam =/= (funccall "bia2ia" [one; tru;arr[one]]);
    fgam =/= (funccall "iai2i" [one; arr[one];one]);
    fgam =/= (funccall "iai2b" [one; arr[one];one]);
    fgam =/= (funccall "iai2ia" [one; arr[one];one]);
    fgam =/= (funccall "iab2i" [one; arr[one];tru]);
    fgam =/= (funccall "iab2b" [one; arr[one];tru]);
    fgam =/= (funccall "iab2ia" [one; arr[one];tru]);
    fgam =/= (funccall "iaia2i" [one; arr[one];arr[one]]);
    fgam =/= (funccall "iaia2b" [one; arr[one];arr[one]]);
    fgam =/= (funccall "iaia2ia" [one; arr[one];arr[one]]);
    fgam =/= (funccall "i2ii" [one; one]);
    fgam =/= (funccall "i2ib" [one; one]);
    fgam =/= (funccall "i2iia" [one; one]);
    fgam =/= (funccall "b2ii" [one; tru]);
    fgam =/= (funccall "b2ib" [one; tru]);
    fgam =/= (funccall "b2iia" [one; tru]);
    fgam =/= (funccall "ia2ii" [one; arr[one]]);
    fgam =/= (funccall "ia2ib" [one; arr[one]]);
    fgam =/= (funccall "ia2iia" [one; arr[one]]);
    fgam =/= (funccall "i2bi" [one; one]);
    fgam =/= (funccall "i2bb" [one; one]);
    fgam =/= (funccall "i2bia" [one; one]);
    fgam =/= (funccall "b2bi" [one; tru]);
    fgam =/= (funccall "b2bb" [one; tru]);
    fgam =/= (funccall "b2bia" [one; tru]);
    fgam =/= (funccall "ia2bi" [one; arr[one]]);
    fgam =/= (funccall "ia2bb" [one; arr[one]]);
    fgam =/= (funccall "ia2bia" [one; arr[one]]);
    fgam =/= (funccall "i2iai" [one; one]);
    fgam =/= (funccall "i2iab" [one; one]);
    fgam =/= (funccall "i2iaia" [one; one]);
    fgam =/= (funccall "b2iai" [one; tru]);
    fgam =/= (funccall "b2iab" [one; tru]);
    fgam =/= (funccall "b2iaia" [one; tru]);
    fgam =/= (funccall "ia2iai" [one; arr[one]]);
    fgam =/= (funccall "ia2iab" [one; arr[one]]);
    fgam =/= (funccall "ia2iaia" [one; arr[one]]);
    fgam =/= (funccall "ii2ii" [one; one;one]);
    fgam =/= (funccall "ii2ib" [one; one;one]);
    fgam =/= (funccall "ii2iia" [one; one;one]);
    fgam =/= (funccall "ib2ii" [one; one;tru]);
    fgam =/= (funccall "ib2ib" [one; one;tru]);
    fgam =/= (funccall "ib2iia" [one; one;tru]);
    fgam =/= (funccall "iia2ii" [one; one;arr[one]]);
    fgam =/= (funccall "iia2ib" [one; one;arr[one]]);
    fgam =/= (funccall "iia2iia" [one; one;arr[one]]);
    fgam =/= (funccall "bi2bi" [one; tru;one]);
    fgam =/= (funccall "bi2bb" [one; tru;one]);
    fgam =/= (funccall "bi2bia" [one; tru;one]);
    fgam =/= (funccall "bb2bi" [one; tru;tru]);
    fgam =/= (funccall "bb2bb" [one; tru;tru]);
    fgam =/= (funccall "bb2bia" [one; tru;tru]);
    fgam =/= (funccall "bia2bi" [one; tru;arr[one]]);
    fgam =/= (funccall "bia2bb" [one; tru;arr[one]]);
    fgam =/= (funccall "bia2bia" [one; tru;arr[one]]);
    fgam =/= (funccall "iai2iai" [one; arr[one];one]);
    fgam =/= (funccall "iai2iab" [one; arr[one];one]);
    fgam =/= (funccall "iai2iaia" [one; arr[one];one]);
    fgam =/= (funccall "iab2iai" [one; arr[one];tru]);
    fgam =/= (funccall "iab2iab" [one; arr[one];tru]);
    fgam =/= (funccall "iab2iaia" [one; arr[one];tru]);
    fgam =/= (funccall "iaia2iai" [one; arr[one];arr[one]]);
    fgam =/= (funccall "iaia2iab" [one; one; arr[one];arr[one]]);

    fgam =/= (funccall "ib2i" [tru;one]);
    fgam =/= (funccall "ib2i" [one;tru;tru]);
    fgam =/= (funccall "ib2i" [one;one;tru]);
    fgam =/= (funccall "ib2i" []);
    fgam =/= (funccall "ib2i" [tru]);
    fgam =/= (funccall "ib2i" [one]);
    fgam =/= (funccall "ib2i" [arr[]; arr[]]);
    fgam =/= (funccall "ib2i" [arr[]]);
    fgam =/= (funccall "ib2i" [arr[]; arr[]; arr[]]);
    fgam =/= (funccall "ib2i" [one; arr[]]);
    fgam =/= (funccall "ib2i" [arr[]; tru]);
    fgam =/= (funccall "ib2i" [one; arr[arr[]]]);
    fgam =/= (funccall "ib2i" [arr[arr[]]; tru]);

    ()

let test_stmt () =
    let open Pos in
    let open TestStmt in
    let open Vars in

    (* Decl *)
    (empty, UnitT) |- decl [avar (aid "x" tint)] =: One;
    (empty, UnitT) |- decl [avar (aid "y" tbool)] =: One;
    (empty, UnitT) |- decl [avar (aid "z" (tarray tint None))] =: One;
    (empty, UnitT) |- decl [avar (aid "x" (tarray (tarray tint None) None))] =: One;
    (empty, UnitT) |- decl [underscore] =: One;
    (empty, UnitT) |- decl [avar (aunderscore tint)] =: One;
    (empty, UnitT) |- decl [avar (aunderscore (tarray tbool None))] =: One;
    (empty, UnitT) |- decl [avar (aunderscore (tarray (tarray tbool None) None))] =: One;
    (empty, UnitT) |- decl [avar (aid "x" tint);
                            avar (aid "y" tbool);
                            avar (aid "z" (tarray tint None))] =: One;
    (empty, UnitT) |- decl [avar (aid "x" tint);
                            underscore;
                            avar (aunderscore tbool)] =: One;
    (empty, BoolT) |- decl [underscore; underscore; underscore] =: One;

    (* DeclAsgn *)
    (empty, UnitT) |- declasgn [avar (aid "x" tint)] one =: One;
    (empty, UnitT) |- declasgn [avar (aid "y" tbool)] tru =: One;
    (empty, UnitT) |- declasgn [avar (aid "z" (tarray tbool None))]
                               (arr[]) =: One;
    (empty, UnitT) |- declasgn [avar (aid "z" (tarray tint None))]
                               (arr[]) =: One;
    (empty, UnitT) |- declasgn [avar (aid "z" (tarray tint None))]
                               (arr[one]) =: One;
    (empty, UnitT) |- declasgn [avar (aid "z" (tarray tint None))]
                               (arr[one; two]) =: One;
    (empty, UnitT) |- declasgn [avar (aid "x" (tarray (tarray tint None) None))]
                               (arr[arr[one]; arr[two]; arr[one;two]; arr[]])
                               =: One;
    (empty, UnitT) |- declasgn [avar (aid "x" (tarray (tarray tint None) None))]
                               (arr[arr[]]) =: One;

    (empty, UnitT) |- declasgn [avar (aunderscore tbool)] fls =: One;
    (empty, UnitT) |- declasgn [avar (aunderscore (tarray tbool None))]
                               (arr[]) =: One;
    (empty, UnitT) |- declasgn [avar (aunderscore (tarray tbool None))]
                               (arr[fls]) =: One;
    (empty, UnitT) |- declasgn [avar (aunderscore (tarray (tarray tbool None) None))]
                               (arr[arr[]]) =: One;
    (empty, UnitT) |- declasgn [avar (aunderscore (tarray (tarray tbool None) None))]
                               (arr[arr[fls]]) =: One;

    (* Asgn *)
    (* Block *)
    (* Return *)

    (* If *)
    (empty, UnitT) |- if_ tru (block []) =: One;
    (empty, UnitT) |- if_ fls (block []) =: One;
    (empty, UnitT) |- if_ ((one == one) & (two == two)) (block [decl [avar (aid "x" tint)]]) =: One;
    (empty, UnitT) |- if_ tru (block [declasgn [avar (aid "x" (tarray (tarray tint None) None))] (arr[arr[]])]) =: One;
    (empty, UnitT) |- if_ tru (decl [avar (aid "x" tint)]) =: One;
    (empty, UnitT) |- if_ tru (declasgn [avar (aid "x" (tarray (tarray tint None) None))] (arr[arr[]])) =: One;
    (vars["x", IntT], UnitT) |- if_ tru (asgn x two) =: One;
    (vars["x", IntT], UnitT) |- if_ tru (block [asgn x two]) =: One;
    (vars["x", IntT], IntT) |- if_ tru (block [return [x]]) =: Zero;
    (vars["x", IntT], UnitT) |- if_ tru (if_ fls (asgn x two)) =: One;
    (vars["x", IntT], UnitT) |- if_ tru (ifelse fls (asgn x two) (asgn x three)) =: One;
    (vars["x", IntT], UnitT) |- if_ tru (while_ fls (asgn x two)) =: One;
    (fgam, UnitT) |- if_ tru (proccall "i2u" []) =: One;

    (* IfElse *)
    
    (* While *)
    (* ProcCall *)

    ()

let test_callable () =
	let open Pos in
	let open TestCallable in

	(* empty context *)
	(* Functions *)

	(* [], [x] *)
	empty |- (func "f" [] [tint] (return [int 3L])) =: (UnitT, IntT);
	empty |- (func "f" [] [tint] (return [(funccall "f" [])])) =: (UnitT, IntT);

	(* [x], [x] *)
	empty |- (func "id" [(aid "x" tint)] [tint] (return [id "x"])) =: (IntT, IntT);
	empty |- (func "id" [(aid "x" tbool)] [tbool] (return [id "x"])) =: (BoolT, BoolT);
	empty |- (func "id" [(aid "x" (tarray tint None))] [(tarray tint None)] (return [id "x"]))
						=: (ArrayT IntT, ArrayT IntT);

	(* _::_, [x] *)
	empty |- (func "f" [(aid "x" tint); (aid "y" tint)] [tint] (return [id "x"])) =: (TupleT [IntT; IntT], IntT);
	empty |- (func "f" [(aid "x" tint); (aid "y" tint)] [tint] (return [id "y"])) =: (TupleT [IntT; IntT], IntT);

	(* [], _::_ *)
	empty |- (func "f" [] [tint; tint] (return [int 3L; int 2L])) =: (UnitT, TupleT [IntT; IntT]);

	(* [x], _::_ *)
	empty |- (func "f" [(aid "x" tint)] [tint; tint] (return [id "x"; id "x"])) =: (IntT, TupleT [IntT; IntT]);

	(* _::_, _::_ *)
	empty |- (func "f" [(aid "x" tint); (aid "y" tint)] [tint;tint] (return [(id "y"); (id "x")]))
						=: (TupleT [IntT; IntT], TupleT [IntT; IntT]);
	empty |- (func "f" [(aid "x" tint); (aid "y" tbool)] [tbool;tint] (return [(id "y"); (id "x")]))
						=: (TupleT [IntT; BoolT], TupleT [BoolT; IntT]);

	(* recursion *)
	empty |- (func "g" [aid "x" tint] [tint] (block [(asgn (id "x") (funccall "g" [id "x"])); (return [id "x"])]))
							=: (IntT, IntT);

	(* wrong return type *)
	empty =/= (func "f" [aid "x" tint] [tbool] (return [id "x"]));

	(* dup args *)
	empty =/= (func "has_dup" [(aid "x" tint); (aid "x" tint)] [tint] (return [id "x"]));

	(* unbound variable y *)
	empty =/= (func "f" [aid "x" tint] [tint] (return [id "y"]));

	(* procedures *)

	(* [] *)
	empty |- (proc "f" [] (proccall "f" [])) =: (UnitT, UnitT);

	(* [x] *)
	empty |- (proc "f" [aid "x" tint] (proccall "f" [id "x"])) =: (IntT, UnitT);

	(* _::_ *)
	empty |- (proc "f" [aid "x" tint; aid "y" tint] (proccall "f" [id "x"; id "x"])) =: (TupleT [IntT; IntT], UnitT);

	(* dup args *)
	empty =/= (proc "f" [aid "x" tint; aid "x" tint] (proccall "f" []));

	(* unbound variable y *)
	empty =/= (proc "f" [] (proccall "y" []));

	(* non-empty context *)
	let f_binded = Context.bind empty "f" (Function (IntT, IntT)) in
	let g_binded = Context.bind empty "g" (Function (UnitT, UnitT)) in

	(* dup func bind *)
	f_binded =/= (func "f" [aid "x" tint] [tint] (return [id "x"]));
	f_binded =/= (func "f" [aid "x" tbool] [tbool] (return [id "x"]));

	(* [x], [x] *)
	f_binded |- (func "g" [aid "x" tint] [tint] (return [id "x"])) =: (IntT, IntT);
	f_binded |- (func "g" [aid "x" tint] [tint] (block [(asgn (id "x") (funccall "f" [id "x"])); (return [id "x"])]))
							=: (IntT, IntT);

	(* recursion *)
	f_binded |- (func "g" [aid "x" tint] [tint] (block [(asgn (id "x") (funccall "g" [id "x"])); (return [id "x"])]))
							=: (IntT, IntT);

	g_binded |- (proc "f" [] (proccall "g" [])) =: (UnitT, UnitT);

	g_binded |- (proc "f" [aid "x" tint] (proccall "g" [])) =: (IntT, UnitT);

	g_binded |- (proc "f" [aid "x" tint; aid "y" tint] (proccall "g" [])) =: (TupleT [IntT; IntT], UnitT);

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

