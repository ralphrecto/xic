open Core.Std
open Typecheck
open OUnit
open Expr
open Stmt
open Sigma
open Ast.S
open TestUtil

let (>>=) = Result.(>>=)
let (>>|) = Result.(>>|)

(* Context helpers *)
let empty = {
  locals        = Context.empty;
  globals       = String.Map.empty;
  delta_m       = String.Map.empty;
  class_context = None;
  delta_i       = String.Map.empty;
  subtype       = (fun _ _ -> false);
}

let gam (xs: (string * Sigma.t) list) =
  {empty with locals = Context.of_alist_exn xs}

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

  (* 0 -> 2 *)
  let u2ii  = ("u2ii",  UnitT, TupleT [IntT; IntT])
  let u2ib  = ("u2ib",  UnitT, TupleT [IntT; BoolT])
  let u2iia = ("u2iia", UnitT, TupleT [IntT; ArrayT IntT])
  let u2bi  = ("u2bi",  UnitT, TupleT [BoolT; IntT])
  let u2bb  = ("u2bb",  UnitT, TupleT [BoolT; BoolT])
  let u2bia = ("u2bia", UnitT, TupleT [BoolT; ArrayT IntT])
  let u2iai  = ("u2iai",  UnitT, TupleT [ArrayT IntT; IntT])
  let u2iab  = ("u2iab",  UnitT, TupleT [ArrayT IntT; BoolT])
  let u2iaia = ("u2iaia", UnitT, TupleT [ArrayT IntT; ArrayT IntT])

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

  (* 3 -> 0 *)
  let iii2u   = ("iii2u",   TupleT [IntT; IntT; IntT],               UnitT)
  let iib2u   = ("iib2u",   TupleT [IntT; IntT; BoolT],              UnitT)
  let iiia2u  = ("iiia2u",  TupleT [IntT; IntT; ArrayT IntT],        UnitT)
  let ibi2u   = ("ibi2u",   TupleT [IntT; BoolT; IntT],              UnitT)
  let ibb2u   = ("ibb2u",   TupleT [IntT; BoolT; BoolT],             UnitT)
  let ibia2u  = ("ibia2u",  TupleT [IntT; BoolT; ArrayT IntT],       UnitT)
  let iiai2u  = ("iiai2u",  TupleT [IntT; ArrayT IntT; IntT],        UnitT)
  let iiab2u  = ("iiab2u",  TupleT [IntT; ArrayT IntT; BoolT],       UnitT)
  let iiaia2u = ("iiaia2u", TupleT [IntT; ArrayT IntT; ArrayT IntT], UnitT)

  let bii2u   = ("bii2u",   TupleT [BoolT; IntT; IntT],               UnitT)
  let bib2u   = ("bib2u",   TupleT [BoolT; IntT; BoolT],              UnitT)
  let biia2u  = ("biia2u",  TupleT [BoolT; IntT; ArrayT IntT],        UnitT)
  let bbi2u   = ("bbi2u",   TupleT [BoolT; BoolT; IntT],              UnitT)
  let bbb2u   = ("bbb2u",   TupleT [BoolT; BoolT; BoolT],             UnitT)
  let bbia2u  = ("bbia2u",  TupleT [BoolT; BoolT; ArrayT IntT],       UnitT)
  let biai2u  = ("biai2u",  TupleT [BoolT; ArrayT IntT; IntT],        UnitT)
  let biab2u  = ("biab2u",  TupleT [BoolT; ArrayT IntT; BoolT],       UnitT)
  let biaia2u = ("biaia2u", TupleT [BoolT; ArrayT IntT; ArrayT IntT], UnitT)

  let iaii2u   = ("iaii2u",   TupleT [ArrayT IntT; IntT; IntT],               UnitT)
  let iaib2u   = ("iaib2u",   TupleT [ArrayT IntT; IntT; BoolT],              UnitT)
  let iaiia2u  = ("iaiia2u",  TupleT [ArrayT IntT; IntT; ArrayT IntT],        UnitT)
  let iabi2u   = ("iabi2u",   TupleT [ArrayT IntT; BoolT; IntT],              UnitT)
  let iabb2u   = ("iabb2u",   TupleT [ArrayT IntT; BoolT; BoolT],             UnitT)
  let iabia2u  = ("iabia2u",  TupleT [ArrayT IntT; BoolT; ArrayT IntT],       UnitT)
  let iaiai2u  = ("iaiai2u",  TupleT [ArrayT IntT; ArrayT IntT; IntT],        UnitT)
  let iaiab2u  = ("iaiab2u",  TupleT [ArrayT IntT; ArrayT IntT; BoolT],       UnitT)
  let iaiaia2u = ("iaiaia2u", TupleT [ArrayT IntT; ArrayT IntT; ArrayT IntT], UnitT)

  let iaup = ("iaup", TupleT [ArrayT IntT; ArrayT (ArrayT IntT); ArrayT (ArrayT (ArrayT IntT))], IntT)
  let iadown = ("iadown", TupleT [ArrayT (ArrayT (ArrayT IntT)); ArrayT (ArrayT IntT); ArrayT IntT], IntT)
  let piaup = ("piaup", TupleT [ArrayT IntT; ArrayT (ArrayT IntT); ArrayT (ArrayT (ArrayT IntT))], UnitT)
  let piadown = ("piadown", TupleT [ArrayT (ArrayT (ArrayT IntT)); ArrayT (ArrayT IntT); ArrayT IntT], UnitT)

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
    iaia2iai; iaia2iab; iaia2iaia; iii2u; iib2u; iiia2u; ibi2u; ibb2u; ibia2u;
    iiai2u; iiab2u; iiaia2u; bii2u; bib2u; biia2u; bbi2u; bbb2u; bbia2u;
    biai2u; biab2u; biaia2u; iaii2u; iaib2u; iaiia2u; iabi2u; iabb2u; iabia2u;
    iaiai2u; iaiab2u; iaiaia2u; u2ii; u2ib; u2iia; u2bi; u2bb; u2bia; u2iai;
    u2iab; u2iaia;

    iaup; iadown; piaup; piadown
  ]
end

let (|-) c e = (c, e)

module TestExpr = struct
  (* If <: is subtype, then =: is equal type. *)
  let (=:) ((c, e): contexts * Pos.expr) (t: Expr.t) : unit =
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

  let (=/=) (c: contexts) (e: Pos.expr) : unit =
    match expr_typecheck c e with
    | Ok e' -> begin
        printf ">>> %s : %s; expected error\n" (Ast.string_of_expr e') (to_string (fst e'));
        assert_true false
    end
    | Error _ -> ()
end

module TestCallable = struct
	let (=:) ((c, e): contexts * Pos.callable) (func_t: Expr.t * Expr.t) : unit =
		let b = is_ok (func_typecheck c e >>= fun gamma ->
									 match snd_func_pass gamma e with
									 | Ok (t, _) -> Ok (assert_equal t func_t)
									 | Error (p, s) -> printf "%s" s;  Error (p, s)) in
		assert_true b

  let (=/=) (c: contexts) (e: Pos.callable) : unit =
    begin
      func_typecheck c e >>= fun gamma ->
			snd_func_pass gamma e
    end
    |> is_error
    |> assert_true
end

module TestProg = struct
	let (=:) (_: unit) (p: Pos.full_prog) : unit =
		begin
			match prog_typecheck p with
			| Error (p, s) -> printf "%s" s; Error (p, s)
			| Ok o -> Ok o
		end
		|> is_ok
		|> assert_true

  let (=/=) (_: unit) (p: Pos.full_prog) : unit =
		prog_typecheck p
    |> is_error
    |> assert_true
end

module TestStmt = struct
  let (=:) (((c, r), s): (contexts * Expr.t) * Pos.stmt) (t: Stmt.t) : unit =
    match stmt_typecheck c r s with
    | Ok s' -> begin
      let t' = fst s' in
      if t' <> t then begin
        printf ">>> %s : %s != %s\n"
          (Ast.string_of_stmt s)
          (Sexp.to_string (Stmt.sexp_of_t t'))
          (Sexp.to_string (Stmt.sexp_of_t t));
        assert_true false
      end
    end
    | Error (_, msg) -> begin
        printf ">>> %s =: %s errored with %s\n"
          (Ast.string_of_stmt s)
          (Sexp.to_string (Stmt.sexp_of_t t))
          msg;
        assert_true false
    end

  let (=/=) ((c, r): (contexts * Expr.t)) (s: Pos.stmt) : unit =
    match stmt_typecheck c r s with
    | Ok s' -> begin
        printf ">>> %s : %s; expected error\n"
          (Ast.string_of_stmt s')
          (Sexp.to_string (Stmt.sexp_of_t (fst s')));
        assert_true false
    end
    | Error _ -> ()
end

let one   = Pos.(int 1L)
let two   = Pos.(int 1L)
let three = Pos.(int 1L)
let tru   = Pos.(bool true)
let fls   = Pos.(bool false)

let test_expr _ =
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

    empty |- (string "") =: EmptyArray;
    empty |- (string "" + string "") =: EmptyArray;
    empty |- (string "" + string "a") =: ArrayT IntT;
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

    (* multiple returns as argument *)
    fgam =/= (funccall "ii2i" [funccall "ii2ii" [one; two]]);

    ()

let test_stmt _ =
    let open Pos in
    let open Vars in
    let open TestStmt in
    let open Vars in

    (* Decl *)
    (empty, UnitT) |- decl [avar (aid "x" tint)] =: One;
    (empty, UnitT) |- decl [avar (aid "y" tbool)] =: One;
    (empty, BoolT) |- decl [avar (aid "y" tbool)] =: One;
    (empty, UnitT) |- decl [avar (aid "z" (tarray tint None))] =: One;
    (empty, UnitT) |- decl [avar (aid "z" (tarray tint (Some one)))] =: One;
    (empty, UnitT) |- decl [avar (aid "x" (tarray (tarray tint None) None))] =: One;

    (empty, UnitT) |- decl [avar (aid "x" (tarray (tarray tint None) (Some one)))] =: One;
    (empty, UnitT) |- decl [avar (aid "x" (tarray (tarray tint (Some one)) (Some one)))] =: One;

    (empty, UnitT) =/= decl [avar (aid "z" (tarray tint (Some tru)))];
    (empty, UnitT) =/= decl [avar (aid "z" (tarray tint (Some (arr[]))))];
    (empty, UnitT) =/= decl [avar (aid "z" (tarray tint (Some (arr[one]))))];

    (empty, UnitT) |- decl [underscore] =: One;
    (empty, UnitT) |- decl [avar (aunderscore tint)] =: One;
    (empty, UnitT) |- decl [avar (aunderscore (tarray tbool None))] =: One;
    (empty, UnitT) |- decl [avar (aunderscore (tarray tbool (Some one)))] =: One;
    (empty, UnitT) |- decl [avar (aunderscore (tarray (tarray tbool None) (Some one)))] =: One;
    (empty, UnitT) |- decl [avar (aunderscore (tarray (tarray tbool (Some one)) (Some one)))] =: One;

    (vars["x",IntT], IntT)        =/= decl [avar (aid "x" tint)];
    (vars["x",BoolT], IntT)       =/= decl [avar (aid "x" tint)];
    (vars["x",ArrayT IntT], IntT) =/= decl [avar (aid "x" tint)];
    (vars["x",IntT], IntT)        =/= decl [avar (aid "x" tbool)];
    (vars["x",BoolT], IntT)       =/= decl [avar (aid "x" tbool)];
    (vars["x",ArrayT IntT], IntT) =/= decl [avar (aid "x" tbool)];

    (empty, UnitT) |- decl [avar (aid "x" tint); avar (aid "y" tint)] =: One;
    (empty, UnitT) |- decl [avar (aid "x" tint); avar (aid "y" tbool)] =: One;
    (empty, UnitT) |- decl [avar (aid "x" tbool); avar (aid "y" tint)] =: One;
    (empty, UnitT) |- decl [avar (aid "x" tbool); avar (aid "y" tbool)] =: One;
    (empty, UnitT) |- decl [underscore; avar (aid "y" tbool)] =: One;
    (empty, UnitT) |- decl [avar (aid "x" tint); underscore] =: One;
    (empty, UnitT) |- decl [underscore; underscore] =: One;
    (empty, UnitT) |- decl [avar (aunderscore tbool); avar (aid "y" tbool)] =: One;
    (empty, UnitT) |- decl [avar (aid "x" tint); avar (aunderscore tbool)] =: One;
    (empty, UnitT) |- decl [avar (aunderscore tbool); avar (aunderscore tbool)] =: One;
    (empty, UnitT) |- decl [avar (aid "x" tint); avar (aid "y" tbool); avar (aid "z" (tarray tint None))] =: One;
    (empty, UnitT) |- decl [avar (aid "x" tint); underscore; avar (aunderscore tbool)] =: One;
    (empty, UnitT) |- decl [underscore; underscore; underscore] =: One;
    (empty, UnitT) |- decl [avar(aid "x" tint); avar(aid "y" tint); underscore] =: One;
    (empty, UnitT) |- decl [avar(aid "x" tint); underscore; avar(aid "z" tint)] =: One;
    (empty, UnitT) |- decl [underscore; avar(aid "y" tint); avar(aid "z" tint)] =: One;
    (empty, UnitT) |- decl [avar(aid "x" tint); underscore; underscore] =: One;
    (empty, UnitT) |- decl [underscore; avar(aid "y" tint); underscore] =: One;
    (empty, UnitT) |- decl [underscore; underscore; avar(aid "z" tint)] =: One;
    (empty, UnitT) |- decl [underscore; underscore; underscore]; =: One;
    (empty, UnitT) |- decl [avar(aid "x" tint); avar(aid "y" tint); avar(aunderscore tbool)] =: One;
    (empty, UnitT) |- decl [avar(aid "x" tint); avar(aunderscore tbool); avar(aid "z" tint)] =: One;
    (empty, UnitT) |- decl [avar(aunderscore tbool); avar(aid "y" tint); avar(aid "z" tint)] =: One;
    (empty, UnitT) |- decl [avar(aid "x" tint); avar(aunderscore tbool); avar(aunderscore tbool)] =: One;
    (empty, UnitT) |- decl [avar(aunderscore tbool); avar(aid "y" tint); avar(aunderscore tbool)] =: One;
    (empty, UnitT) |- decl [avar(aunderscore tbool); avar(aunderscore tbool); avar(aid "z" tint)] =: One;
    (empty, UnitT) |- decl [avar(aunderscore tbool); avar(aunderscore tbool); avar(aunderscore tbool)] =: One;

    (empty, UnitT) =/= decl [avar (aid "x" tint); avar (aid "x" tint)];
    (empty, UnitT) =/= decl [avar (aid "x" tint); avar (aid "x" tint)];
    (empty, UnitT) =/= decl [avar (aid "x" tint); avar (aid "x" tbool)];
    (empty, UnitT) =/= decl [avar (aid "x" tbool); avar (aid "x" tint)];
    (empty, UnitT) =/= decl [avar (aid "x" tbool); avar (aid "x" tbool)];
    (empty, UnitT) =/= decl [avar (aid "x" tint); avar (aid "x" tbool); avar (aid "z" (tarray tint None))];
    (empty, UnitT) =/= decl [avar (aid "x" tint); avar (aid "x" tbool); avar (aid "x" (tarray tint None))];
    (empty, UnitT) =/= decl [avar(aid "x" tint); avar(aid "x" tint); underscore];
    (empty, UnitT) =/= decl [avar(aid "x" tint); underscore; avar(aid "x" tint)];
    (empty, UnitT) =/= decl [underscore; avar(aid "x" tint); avar(aid "x" tint)];
    (empty, UnitT) =/= decl [avar(aid "x" tint); avar(aid "x" tint); avar(aunderscore tbool)];
    (empty, UnitT) =/= decl [avar(aid "x" tint); avar(aunderscore tbool); avar(aid "x" tint)];
    (empty, UnitT) =/= decl [avar(aunderscore tbool); avar(aid "x" tint); avar(aid "x" tint)];

    (vars["x",IntT], UnitT) =/= decl [avar (aid "x" tint); avar (aid "y" tint)];
    (vars["x",IntT], UnitT) =/= decl [avar (aid "x" tint); avar (aid "y" tbool)];
    (vars["x",IntT], UnitT) =/= decl [avar (aid "x" tbool); avar (aid "y" tint)];
    (vars["x",IntT], UnitT) =/= decl [avar (aid "x" tbool); avar (aid "y" tbool)];
    (vars["x",IntT], UnitT) =/= decl [avar (aid "x" tint); underscore];
    (vars["x",IntT], UnitT) =/= decl [avar (aid "x" tint); avar (aunderscore tbool)];
    (vars["x",IntT], UnitT) =/= decl [avar (aid "x" tint); avar (aid "y" tbool); avar (aid "z" (tarray tint None))];
    (vars["x",IntT], UnitT) =/= decl [avar (aid "x" tint); underscore; avar (aunderscore tbool)];
    (vars["x",IntT], UnitT) =/= decl [avar(aid "x" tint); avar(aid "y" tint); underscore];
    (vars["x",IntT], UnitT) =/= decl [avar(aid "x" tint); underscore; avar(aid "z" tint)];
    (vars["x",IntT], UnitT) =/= decl [avar(aid "x" tint); underscore; underscore];
    (vars["x",IntT], UnitT) =/= decl [avar(aid "x" tint); avar(aid "y" tint); avar(aunderscore tbool)];
    (vars["x",IntT], UnitT) =/= decl [avar(aid "x" tint); avar(aunderscore tbool); avar(aid "z" tint)];
    (vars["x",IntT], UnitT) =/= decl [avar(aid "x" tint); avar(aunderscore tbool); avar(aunderscore tbool)];
    (vars["x",BoolT], UnitT) =/= decl [avar (aid "x" tint); avar (aid "y" tint)];
    (vars["x",BoolT], UnitT) =/= decl [avar (aid "x" tint); avar (aid "y" tbool)];
    (vars["x",BoolT], UnitT) =/= decl [avar (aid "x" tbool); avar (aid "y" tint)];
    (vars["x",BoolT], UnitT) =/= decl [avar (aid "x" tbool); avar (aid "y" tbool)];
    (vars["x",BoolT], UnitT) =/= decl [avar (aid "x" tint); underscore];
    (vars["x",BoolT], UnitT) =/= decl [avar (aid "x" tint); avar (aunderscore tbool)];
    (vars["x",BoolT], UnitT) =/= decl [avar (aid "x" tint); avar (aid "y" tbool); avar (aid "z" (tarray tint None))];
    (vars["x",BoolT], UnitT) =/= decl [avar (aid "x" tint); underscore; avar (aunderscore tbool)];
    (vars["x",BoolT], UnitT) =/= decl [avar(aid "x" tint); avar(aid "y" tint); underscore];
    (vars["x",BoolT], UnitT) =/= decl [avar(aid "x" tint); underscore; avar(aid "z" tint)];
    (vars["x",BoolT], UnitT) =/= decl [avar(aid "x" tint); underscore; underscore];
    (vars["x",BoolT], UnitT) =/= decl [avar(aid "x" tint); avar(aid "y" tint); avar(aunderscore tbool)];
    (vars["x",BoolT], UnitT) =/= decl [avar(aid "x" tint); avar(aunderscore tbool); avar(aid "z" tint)];
    (vars["x",BoolT], UnitT) =/= decl [avar(aid "x" tint); avar(aunderscore tbool); avar(aunderscore tbool)];

    (* DeclAsgn *)
    (empty, UnitT) |- declasgn [avar (aid "x" tint)] one =: One;
    (empty, UnitT) |- declasgn [avar (aid "y" tbool)] tru =: One;
    (empty, UnitT) |- declasgn [avar (aid "z" (tarray tbool None))] (arr[]) =: One;
    (empty, UnitT) |- declasgn [avar (aid "z" (tarray tbool None))] (arr[tru]) =: One;
    (empty, UnitT) |- declasgn [avar (aid "z" (tarray tint None))] (arr[]) =: One;
    (empty, UnitT) |- declasgn [avar (aid "z" (tarray tint None))] (arr[one]) =: One;
    (empty, UnitT) |- declasgn [avar (aid "z" (tarray tint None))] (arr[one; two]) =: One;

    (empty, UnitT) =/= declasgn [avar (aid "x" tint)] tru;
    (empty, UnitT) =/= declasgn [avar (aid "y" tbool)] one;
    (empty, UnitT) =/= declasgn [avar (aid "z" (tarray tbool None))] (arr[arr[]]);
    (empty, UnitT) =/= declasgn [avar (aid "z" (tarray tbool None))] (arr[one]);
    (empty, UnitT) =/= declasgn [avar (aid "z" (tarray tint None))] (arr[arr[]]);
    (empty, UnitT) =/= declasgn [avar (aid "z" (tarray tint None))] (arr[tru]);
    (empty, UnitT) =/= declasgn [avar (aid "z" (tarray tint None))] (arr[tru; tru]);

    (empty, UnitT) |- declasgn [avar (aid "x" (tarray (tarray tint None) None))]
                               (arr[arr[one]; arr[two]; arr[one;two]; arr[]])
                               =: One;
    (empty, UnitT) |- declasgn [avar (aid "x" (tarray (tarray tint None) None))] (arr[arr[]]) =: One;
    (empty, UnitT) |- declasgn [avar (aunderscore tbool)] fls =: One;
    (empty, UnitT) |- declasgn [avar (aunderscore (tarray tbool None))] (arr[]) =: One;
    (empty, UnitT) |- declasgn [avar (aunderscore (tarray tbool None))] (arr[fls]) =: One;
    (empty, UnitT) |- declasgn [avar (aunderscore (tarray (tarray tbool None) None))] (arr[arr[]]) =: One;
    (empty, UnitT) |- declasgn [avar (aunderscore (tarray (tarray tbool None) None))] (arr[arr[fls]]) =: One;

    (fgam, UnitT) |- declasgn [avar(aid "x" tint); avar(aid "y" tbool)] (funccall "u2ib" []) =: One;
    (fgam, UnitT) |- declasgn [avar(aid "y" tbool); avar(aid "x" tint)] (funccall "u2bi" []) =: One;
    (fgam, UnitT) |- declasgn [avar(aid "y" tint); avar(aid "x" tint)] (funccall "u2ii" []) =: One;
    (fgam, UnitT) |- declasgn [avar(aid "y" (tarray tint None)); avar(aid "x" (tarray tint None))] (funccall "u2iaia" []) =: One;
    (fgam, UnitT) |- declasgn [avar(aid "y" (tarray tint (Some one))); avar(aid "x" (tarray tint None))] (funccall "u2iaia" []) =: One;
    (fgam, UnitT) |- declasgn [avar(aid "y" (tarray tint None)); avar(aid "x" (tarray tint (Some one)))] (funccall "u2iaia" []) =: One;

    (fgam, UnitT) =/= declasgn [avar(aid "y" tbool); avar(aid "x" tint)] one;
    (fgam, UnitT) =/= declasgn [avar(aid "y" tbool); avar(aid "x" tint)] tru;
    (fgam, UnitT) =/= declasgn [avar(aid "y" tbool); avar(aid "x" tint)] (arr[]);
    (fgam, UnitT) =/= declasgn [avar(aid "y" tbool); avar(aid "x" tint)] (arr[]);

    (fgam, UnitT) =/= declasgn [avar(aid "x" tint); avar(aid "x" tbool)] (funccall "u2ib" []);
    (fgam, UnitT) =/= declasgn [avar(aid "x" tbool); avar(aid "x" tint)] (funccall "u2bi" []);
    (fgam, UnitT) =/= declasgn [avar(aid "x" tint); avar(aid "x" tint)] (funccall "u2ii" []);
    (fgam, UnitT) =/= declasgn [avar(aid "x" (tarray tint None)); avar(aid "x" (tarray tint None))] (funccall "u2iaia" []);
    (fgam, UnitT) =/= declasgn [avar(aid "x" (tarray tint (Some one))); avar(aid "x" (tarray tint None))] (funccall "u2iaia" []);
    (fgam, UnitT) =/= declasgn [avar(aid "x" (tarray tint None)); avar(aid "x" (tarray tint (Some one)))] (funccall "u2iaia" []);

    let g = {empty with locals = Context.bind fgam.locals "x" (Var IntT)} in
    (g, UnitT) =/= declasgn [avar(aid "x" tint); avar(aid "y" tbool)] (funccall "u2ib" []);
    (g, UnitT) =/= declasgn [avar(aid "y" tbool); avar(aid "x" tint)] (funccall "u2bi" []);
    (g, UnitT) =/= declasgn [avar(aid "y" tint); avar(aid "x" tint)] (funccall "u2ii" []);
    (g, UnitT) =/= declasgn [avar(aid "y" (tarray tint None)); avar(aid "x" (tarray tint None))] (funccall "u2iaia" []);
    (g, UnitT) =/= declasgn [avar(aid "y" (tarray tint (Some one))); avar(aid "x" (tarray tint None))] (funccall "u2iaia" []);
    (g, UnitT) =/= declasgn [avar(aid "y" (tarray tint None)); avar(aid "x" (tarray tint (Some one)))] (funccall "u2iaia" []);

    (g, UnitT) =/= declasgn [avar(aid "x" tint)] (funccall "u2ii" []);
    (g, UnitT) =/= declasgn [avar(aunderscore tint)] (funccall "u2ii" []);
    (g, UnitT) =/= declasgn [underscore] (funccall "u2ii" []);

    (* Asgn *)
    (vars["x",IntT], IntT) |- (asgn x one) =: One;
    (vars["x",IntT], IntT) |- (asgn x (one + one)) =: One;
    (vars["x",BoolT], IntT) |- (asgn x tru) =: One;
    (vars["x",BoolT], IntT) |- (asgn x (tru & fls)) =: One;
    (vars["x",ArrayT IntT], IntT) |- (asgn x (arr[])) =: One;
    (vars["x",ArrayT IntT], IntT) |- (asgn x (arr[one])) =: One;
    (vars["x",ArrayT (ArrayT IntT)], IntT) |- (asgn x (arr[])) =: One;
    (vars["x",ArrayT (ArrayT IntT)], IntT) |- (asgn x (arr[arr[]])) =: One;
    (vars["x",ArrayT (ArrayT IntT)], IntT) |- (asgn x (arr[arr[one]])) =: One;
    (vars["y",IntT;"x",IntT], IntT) |- (asgn x one) =: One;
    (vars["y",IntT;"x",IntT], IntT) |- (asgn x (one + one)) =: One;
    (vars["y",IntT;"x",BoolT], IntT) |- (asgn x tru) =: One;
    (vars["y",IntT;"x",BoolT], IntT) |- (asgn x (tru & fls)) =: One;
    (vars["y",IntT;"x",ArrayT IntT], IntT) |- (asgn x (arr[])) =: One;
    (vars["y",IntT;"x",ArrayT IntT], IntT) |- (asgn x (arr[one])) =: One;
    (vars["y",IntT;"x",ArrayT (ArrayT IntT)], IntT) |- (asgn x (arr[])) =: One;
    (vars["y",IntT;"x",ArrayT (ArrayT IntT)], IntT) |- (asgn x (arr[arr[]])) =: One;
    (vars["y",IntT;"x",ArrayT (ArrayT IntT)], IntT) |- (asgn x (arr[arr[one]])) =: One;
    (fgam, IntT) |- (asgn (index (funccall "u2ia" []) one) one) =: One;

    (empty, IntT) =/= (asgn x one);
    (empty, IntT) =/= (asgn x tru);
    (empty, IntT) =/= (asgn x (arr[]));
    (empty, IntT) =/= (asgn x (arr[one]));
    (empty, IntT) =/= (asgn x (arr[arr[]]));
    (empty, IntT) =/= (asgn x (arr[arr[one]]));
    (empty, IntT) =/= (asgn (index (funccall "f" []) one) one);
    (empty, IntT) =/= (asgn (index (string "") one) one);

    (vars["x",IntT], IntT) =/= (asgn x tru);
    (vars["x",IntT], IntT) =/= (asgn x (arr[]));
    (vars["x",IntT], IntT) =/= (asgn x (arr[one]));
    (vars["x",BoolT], IntT) =/= (asgn x one);
    (vars["x",BoolT], IntT) =/= (asgn x (arr[]));
    (vars["x",BoolT], IntT) =/= (asgn x (arr[tru]));
    (vars["x",ArrayT IntT], IntT) =/= (asgn x (arr[arr[arr[arr[arr[]]]]]));
    (vars["x",ArrayT IntT], IntT) =/= (asgn x (arr[tru]));

    (fgam, IntT) =/= (asgn (index (funccall "u2ia" []) one) tru);
    (fgam, IntT) =/= (asgn (index (funccall "u2ia" []) one) (arr[]));
    (fgam, IntT) =/= (asgn (index (funccall "u2ia" []) one) (arr[one]));
    (fgam, IntT) =/= (asgn (index (string "") one) tru);
    (fgam, IntT) =/= (asgn (index (string "") one) (arr[]));
    (fgam, IntT) =/= (asgn (index (string "") one) (arr[one]));
    (fgam, IntT) =/= (asgn (index (string "") one) (arr[arr[]]));

    (* Block *)
    (empty, UnitT) |- block [] =: One;

    (empty, UnitT) |- block [decl [avar (aid "x" tint)]] =: One;
    (empty, UnitT) |- block [decl [avar (aid "x" tint)];
                             decl [avar (aid "y" tbool)]] =: One;
    (empty, UnitT) |- block [declasgn [avar (aid "x" (tarray (tarray tint None) None))] (arr[arr[]])] =: One;
    (empty, UnitT) |- block [declasgn [avar (aid "x" (tarray (tarray tint None) None))] (arr[arr[]])] =: One;
    (vars["x", IntT], UnitT) |- block [asgn x two] =: One;
    (vars["x", IntT], UnitT) |- block [block [asgn x two]] =: One;
    (vars["x", IntT], UnitT) |- block [block[]; block[]; block[]] =: One;
    (vars["x", IntT], IntT)  |- block [block [return [x]]] =: Zero;
    (vars["x", IntT], UnitT) |- block [if_ fls (asgn x two)] =: One;
    (vars["x", IntT], UnitT) |- block [ifelse fls (asgn x two) (asgn x three)] =: One;
    (vars["x", IntT], UnitT) |- block [while_ fls (asgn x two)] =: One;
    (fgam, UnitT) |- block [proccall "i2u" [one]] =: One;

    (empty, UnitT) |- block [block [decl [avar (aid "x" tint)]];
                             block [decl [avar (aid "x" tint)]]] =: One;
    (empty, UnitT) |- block [block [declasgn [avar (aid "x" tint)] one];
                             block [declasgn [avar (aid "x" tint)] two]] =: One;
    (empty, UnitT) |- block [block [declasgn [avar (aid "x" tint)] one];
                             declasgn [avar (aid "x" tint)] two] =: One;

    (empty, UnitT) |- block [decl [avar (aid "x" tint)]; asgn x two] =: One;
    (empty, UnitT) |- block [declasgn [avar (aid "x" tint)] one; asgn x two] =: One;
    (empty, UnitT) |- block [decl [avar (aid "x" tint)];
                              block [asgn x two]] =: One;
    (empty, UnitT) |- block [declasgn [avar (aid "x" tint)] one;
                             block [asgn x two]] =: One;
    (vars["x", IntT], UnitT) |- block [asgn x two] =: One;
    (vars["x", IntT], UnitT) |- block [block [asgn x two]] =: One;

    (* Block errors *)
    (empty, UnitT) =/= (block [decl [avar (aid "x" tint)];
                               decl [avar (aid "x" tint)]]);
    (empty, UnitT) =/= (block [declasgn [avar (aid "x" tint)] one;
                               declasgn [avar (aid "x" tint)] two]);
    (empty, UnitT) =/= (block [decl [avar (aid "x" tint)];
                               block [declasgn [avar (aid "x" tbool)] tru]]);

    (vars["x", IntT], UnitT) =/= (block [decl [avar (aid "x" tint)]]);
    (vars["x", IntT], UnitT) =/= (block [declasgn [avar (aid "x" tint)] one]);

    (empty, UnitT) =/= block [decl [avar (aid "x" tint)]; return [x]];

    (* Return *)
    (empty, UnitT) |- (return []) =: Zero;

    (empty, IntT) =/= (return []);
    (empty, BoolT) =/= (return []);
    (empty, ArrayT IntT) =/= (return []);

    (empty, ArrayT (ArrayT IntT)) =/= (return []);
    (empty, IntT) |- (return [one]) =: Zero;
    (empty, BoolT) |- (return [tru]) =: Zero;
    (empty, ArrayT BoolT) |- (return [arr[tru]]) =: Zero;
    (empty, ArrayT IntT) |- (return [arr[one]]) =: Zero;
    (empty, ArrayT IntT) |- (return [arr[]]) =: Zero;
    (empty, ArrayT (ArrayT IntT)) |- (return [arr[]]) =: Zero;
    (empty, ArrayT (ArrayT IntT)) |- (return [arr[arr[]]]) =: Zero;
    (empty, ArrayT (ArrayT (ArrayT IntT))) |- (return [arr[]]) =: Zero;
    (empty, ArrayT (ArrayT (ArrayT IntT))) |- (return [arr[arr[]]]) =: Zero;
    (empty, ArrayT (ArrayT (ArrayT IntT))) |- (return [arr[arr[arr[]]]]) =: Zero;

    (empty, IntT) =/= (return [tru]);
    (empty, BoolT) =/= (return [one]);
    (empty, ArrayT IntT) =/= (return [one]);
    (empty, ArrayT IntT) =/= (return [tru]);
    (empty, ArrayT IntT) =/= (return [arr[tru]]);
    (empty, ArrayT BoolT) =/= (return [arr[one]]);

    (empty, TupleT [IntT; IntT]) |- (return [one; one]) =: Zero;
    (empty, TupleT [IntT; BoolT]) |- (return [one; tru]) =: Zero;
    (empty, TupleT [BoolT; IntT]) |- (return [tru; one]) =: Zero;
    (empty, TupleT [BoolT; BoolT]) |- (return [tru; tru]) =: Zero;
    (empty, TupleT [IntT; IntT; IntT]) |- (return [one; one; one]) =: Zero;
    (empty, TupleT [IntT; IntT; BoolT]) |- (return [one; one; tru]) =: Zero;
    (empty, TupleT [IntT; BoolT; IntT]) |- (return [one; tru; one]) =: Zero;
    (empty, TupleT [IntT; BoolT; BoolT]) |- (return [one; tru; tru]) =: Zero;
    (empty, TupleT [BoolT; IntT; IntT]) |- (return [tru; one; one]) =: Zero;
    (empty, TupleT [BoolT; IntT; BoolT]) |- (return [tru; one; tru]) =: Zero;
    (empty, TupleT [BoolT; BoolT; IntT]) |- (return [tru; tru; one]) =: Zero;
    (empty, TupleT [BoolT; BoolT; BoolT]) |- (return [tru; tru; tru]) =: Zero;

    (empty, TupleT [ArrayT IntT; ArrayT (ArrayT IntT)]) |- (return [arr[]; arr[]]) =: Zero;
    (empty, TupleT [ArrayT IntT; ArrayT (ArrayT IntT)]) |- (return [arr[one]; arr[]]) =: Zero;
    (empty, TupleT [ArrayT IntT; ArrayT (ArrayT IntT)]) |- (return [arr[one]; arr[arr[]]]) =: Zero;
    (empty, TupleT [ArrayT IntT; ArrayT (ArrayT IntT)]) |- (return [arr[]; arr[arr[one]]]) =: Zero;
    (empty, TupleT [ArrayT IntT; ArrayT (ArrayT IntT)]) |- (return [arr[one]; arr[arr[one]]]) =: Zero;
    (empty, TupleT [ArrayT BoolT; ArrayT IntT; ArrayT (ArrayT IntT)]) |- (return [arr[]; arr[]; arr[]]) =: Zero;
    (empty, TupleT [ArrayT BoolT; ArrayT IntT; ArrayT (ArrayT IntT)]) |- (return [arr[]; arr[one]; arr[]]) =: Zero;
    (empty, TupleT [ArrayT BoolT; ArrayT IntT; ArrayT (ArrayT IntT)]) |- (return [arr[]; arr[one]; arr[arr[]]]) =: Zero;
    (empty, TupleT [ArrayT BoolT; ArrayT IntT; ArrayT (ArrayT IntT)]) |- (return [arr[]; arr[]; arr[arr[one]]]) =: Zero;
    (empty, TupleT [ArrayT BoolT; ArrayT IntT; ArrayT (ArrayT IntT)]) |- (return [arr[]; arr[one]; arr[arr[one]]]) =: Zero;
    (empty, TupleT [ArrayT BoolT; ArrayT IntT; ArrayT (ArrayT IntT)]) |- (return [arr[tru]; arr[]; arr[]]) =: Zero;
    (empty, TupleT [ArrayT BoolT; ArrayT IntT; ArrayT (ArrayT IntT)]) |- (return [arr[tru]; arr[one]; arr[]]) =: Zero;
    (empty, TupleT [ArrayT BoolT; ArrayT IntT; ArrayT (ArrayT IntT)]) |- (return [arr[tru]; arr[one]; arr[arr[]]]) =: Zero;
    (empty, TupleT [ArrayT BoolT; ArrayT IntT; ArrayT (ArrayT IntT)]) |- (return [arr[tru]; arr[]; arr[arr[one]]]) =: Zero;
    (empty, TupleT [ArrayT BoolT; ArrayT IntT; ArrayT (ArrayT IntT)]) |- (return [arr[tru]; arr[one]; arr[arr[one]]]) =: Zero;

    (empty, TupleT [IntT; IntT])          =/= (return []);
    (empty, TupleT [IntT; BoolT])         =/= (return []);
    (empty, TupleT [BoolT; IntT])         =/= (return []);
    (empty, TupleT [BoolT; BoolT])        =/= (return []);
    (empty, TupleT [IntT; IntT; IntT])    =/= (return []);
    (empty, TupleT [IntT; IntT; BoolT])   =/= (return []);
    (empty, TupleT [IntT; BoolT; IntT])   =/= (return []);
    (empty, TupleT [IntT; BoolT; BoolT])  =/= (return []);
    (empty, TupleT [BoolT; IntT; IntT])   =/= (return []);
    (empty, TupleT [BoolT; IntT; BoolT])  =/= (return []);
    (empty, TupleT [BoolT; BoolT; IntT])  =/= (return []);
    (empty, TupleT [BoolT; BoolT; BoolT]) =/= (return []);
    (empty, TupleT [IntT; IntT])          =/= (return [one]);
    (empty, TupleT [IntT; BoolT])         =/= (return [one]);
    (empty, TupleT [BoolT; IntT])         =/= (return [one]);
    (empty, TupleT [BoolT; BoolT])        =/= (return [one]);
    (empty, TupleT [IntT; IntT; IntT])    =/= (return [one]);
    (empty, TupleT [IntT; IntT; BoolT])   =/= (return [one]);
    (empty, TupleT [IntT; BoolT; IntT])   =/= (return [one]);
    (empty, TupleT [IntT; BoolT; BoolT])  =/= (return [one]);
    (empty, TupleT [BoolT; IntT; IntT])   =/= (return [one]);
    (empty, TupleT [BoolT; IntT; BoolT])  =/= (return [one]);
    (empty, TupleT [BoolT; BoolT; IntT])  =/= (return [one]);
    (empty, TupleT [BoolT; BoolT; BoolT]) =/= (return [one]);
    (empty, TupleT [IntT; IntT])          =/= (return [one; one; one; one]);
    (empty, TupleT [IntT; BoolT])         =/= (return [one; one; one; one]);
    (empty, TupleT [BoolT; IntT])         =/= (return [one; one; one; one]);
    (empty, TupleT [BoolT; BoolT])        =/= (return [one; one; one; one]);
    (empty, TupleT [IntT; IntT; IntT])    =/= (return [one; one; one; one]);
    (empty, TupleT [IntT; IntT; BoolT])   =/= (return [one; one; one; one]);
    (empty, TupleT [IntT; BoolT; IntT])   =/= (return [one; one; one; one]);
    (empty, TupleT [IntT; BoolT; BoolT])  =/= (return [one; one; one; one]);
    (empty, TupleT [BoolT; IntT; IntT])   =/= (return [one; one; one; one]);
    (empty, TupleT [BoolT; IntT; BoolT])  =/= (return [one; one; one; one]);
    (empty, TupleT [BoolT; BoolT; IntT])  =/= (return [one; one; one; one]);
    (empty, TupleT [BoolT; BoolT; BoolT]) =/= (return [one; one; one; one]);
    (empty, TupleT [IntT; BoolT]) =/= (return [one; one]);
    (empty, TupleT [IntT; BoolT]) =/= (return [tru; tru]);
    (empty, TupleT [IntT; BoolT]) =/= (return [tru; one]);
    (empty, TupleT [IntT; BoolT]) =/= (return [one; tru; tru]);
    (empty, TupleT [IntT; BoolT]) =/= (return [one; one; tru]);
    (empty, TupleT [IntT; BoolT]) =/= (return [tru; one; one; tru]);
    (empty, TupleT [IntT; BoolT]) =/= (return [tru; one; one; tru; one]);
    (empty, TupleT [IntT; BoolT]) =/= (return [one; tru; one; one; tru; one]);
    (empty, TupleT [ArrayT IntT; ArrayT BoolT]) =/= (return [arr[one]; arr[one]]);
    (empty, TupleT [ArrayT IntT; ArrayT BoolT]) =/= (return [arr[tru]; arr[tru]]);
    (empty, TupleT [ArrayT IntT; ArrayT BoolT]) =/= (return [arr[tru]; arr[one]]);
    (empty, TupleT [ArrayT IntT; ArrayT BoolT]) =/= (return [arr[arr[tru]]; arr[arr[one]]]);

    (* If *)
    (empty, UnitT) |- if_ tru (block []) =: One;
    (empty, UnitT) |- if_ fls (block []) =: One;
    (empty, UnitT) |- if_ ((one == one) & (two == two)) (block [decl [avar (aid "x" tint)]]) =: One;
    (empty, UnitT) |- if_ tru (block [declasgn [avar (aid "x" (tarray (tarray tint None) None))] (arr[arr[]])]) =: One;
    (empty, UnitT) |- if_ tru (decl [avar (aid "x" tint)]) =: One;
    (empty, UnitT) |- if_ tru (declasgn [avar (aid "x" (tarray (tarray tint None) None))] (arr[arr[]])) =: One;
    (vars["x", IntT], UnitT) |- if_ tru (asgn x two) =: One;
    (vars["x", IntT], UnitT) |- if_ tru (block [asgn x two]) =: One;
    (vars["x", IntT], IntT)  |- if_ tru (block [return [x]]) =: One;
    (vars["x", IntT], UnitT) |- if_ tru (if_ fls (asgn x two)) =: One;
    (vars["x", IntT], UnitT) |- if_ tru (ifelse fls (asgn x two) (asgn x three)) =: One;
    (vars["x", IntT], UnitT) |- if_ tru (while_ fls (asgn x two)) =: One;
    (fgam, UnitT) |- if_ tru (proccall "i2u" [one]) =: One;

    (empty, UnitT) =/= (if_ one (block []));
    (empty, UnitT) =/= (if_ tru (return [one]));

    (* IfElse *)
    (empty, UnitT) |- ifelse tru (block []) (block []) =: One;
    (empty, UnitT) |- ifelse fls (block []) (block []) =: One;
    (empty, UnitT) |- ifelse ((one == one) & (two == two)) (block [decl [avar (aid "x" tint)]]) (block [decl [avar (aid "x" tint)]]) =: One;
    (empty, UnitT) |- ifelse ((one == one) & (two == two)) (block [decl [avar (aid "x" tint)]]) (block [decl [avar (aid "y" tbool)]]) =: One;
    (empty, UnitT) |- ifelse tru
                             (block [declasgn [avar (aid "x" (tarray (tarray tint None) None))] (arr[arr[]])])
                             (block [declasgn [avar (aid "x" (tarray (tarray tint None) None))] (arr[arr[]])]) =: One;
    (empty, UnitT) |- ifelse tru (decl [avar (aid "x" tint)]) (block []) =: One;
    (empty, UnitT) |- ifelse tru (declasgn [avar (aid "x" (tarray (tarray tint None) None))] (arr[arr[]])) (block []) =: One;
    (vars["x", IntT], UnitT) |- ifelse tru (asgn x two) (asgn x three) =: One;
    (vars["x", IntT], UnitT) |- ifelse tru (block [asgn x two]) (asgn x three) =: One;
    (vars["x", IntT], IntT)  |- ifelse tru (block [return [x]]) (asgn x three) =: One;
    (vars["x", IntT], IntT)  |- ifelse tru (asgn x three) (block [return [x]]) =: One;
    (vars["x", IntT], IntT)  |- ifelse tru (block [return [x]]) (block [return [x]]) =: Zero;
    (vars["x", IntT], IntT)  |- ifelse tru (if_ tru (block [return [x]])) (block [return [x]]) =: One;
    (vars["x", IntT], UnitT) |- ifelse tru (if_ fls (asgn x two)) (asgn x three) =: One;
    (vars["x", IntT], UnitT) |- ifelse tru (ifelse fls (asgn x two) (asgn x three)) (asgn x three) =: One;
    (vars["x", IntT], UnitT) |- ifelse tru (while_ fls (asgn x two)) (asgn x three) =: One;
    (fgam, UnitT) |- ifelse tru (proccall "i2u" [one]) (proccall "bi2u" [tru; one]) =: One;

    (empty, UnitT) =/= (ifelse one (block []) (block []));
    (empty, UnitT) =/= (ifelse tru (return [one]) (block []));
    (empty, UnitT) =/= (ifelse tru (block []) (return [one]));

    (* While *)
    (empty, UnitT) |- while_ tru (block []) =: One;
    (empty, UnitT) |- while_ fls (block []) =: One;
    (empty, UnitT) |- while_ ((one == one) & (two == two)) (block [decl [avar (aid "x" tint)]]) =: One;
    (empty, UnitT) |- while_ tru (block [declasgn [avar (aid "x" (tarray (tarray tint None) None))] (arr[arr[]])]) =: One;
    (empty, UnitT) |- while_ tru (decl [avar (aid "x" tint)]) =: One;
    (empty, UnitT) |- while_ tru (declasgn [avar (aid "x" (tarray (tarray tint None) None))] (arr[arr[]])) =: One;
    (vars["x", IntT], UnitT) |- while_ tru (asgn x two) =: One;
    (vars["x", IntT], UnitT) |- while_ tru (block [asgn x two]) =: One;
    (vars["x", IntT], IntT)  |- while_ tru (block [return [x]]) =: One;
    (vars["x", IntT], UnitT) |- while_ tru (if_ fls (asgn x two)) =: One;
    (vars["x", IntT], UnitT) |- while_ tru (ifelse fls (asgn x two) (asgn x three)) =: One;
    (vars["x", IntT], UnitT) |- while_ tru (while_ fls (asgn x two)) =: One;
    (fgam, UnitT) |- while_ tru (proccall "i2u" [one]) =: One;

    (empty, UnitT) =/= (while_ one (block []));
    (empty, UnitT) =/= (while_ tru (return [one]));

    (* ProcCall *)
    (fgam, IntT) |- (proccall "u2u" []) =: One;

    (fgam, IntT) |- (proccall "iii2u"    [one; one; one]) =: One;
    (fgam, IntT) |- (proccall "iib2u"    [one; one; tru]) =: One;
    (fgam, IntT) |- (proccall "iiia2u"   [one; one; arr[one]]) =: One;
    (fgam, IntT) |- (proccall "ibi2u"    [one; tru; one]) =: One;
    (fgam, IntT) |- (proccall "ibb2u"    [one; tru; tru]) =: One;
    (fgam, IntT) |- (proccall "ibia2u"   [one; tru; arr[one]]) =: One;
    (fgam, IntT) |- (proccall "iiai2u"   [one; arr[one]; one]) =: One;
    (fgam, IntT) |- (proccall "iiab2u"   [one; arr[one]; tru]) =: One;
    (fgam, IntT) |- (proccall "iiaia2u"  [one; arr[one]; arr[one]]) =: One;
    (fgam, IntT) |- (proccall "bii2u"    [tru; one; one]) =: One;
    (fgam, IntT) |- (proccall "bib2u"    [tru; one; tru]) =: One;
    (fgam, IntT) |- (proccall "biia2u"   [tru; one; arr[one]]) =: One;
    (fgam, IntT) |- (proccall "bbi2u"    [tru; tru; one]) =: One;
    (fgam, IntT) |- (proccall "bbb2u"    [tru; tru; tru]) =: One;
    (fgam, IntT) |- (proccall "bbia2u"   [tru; tru; arr[one]]) =: One;
    (fgam, IntT) |- (proccall "biai2u"   [tru; arr[one]; one]) =: One;
    (fgam, IntT) |- (proccall "biab2u"   [tru; arr[one]; tru]) =: One;
    (fgam, IntT) |- (proccall "biaia2u"  [tru; arr[one]; arr[one]]) =: One;
    (fgam, IntT) |- (proccall "iaii2u"   [arr[one]; one; one]) =: One;
    (fgam, IntT) |- (proccall "iaib2u"   [arr[one]; one; tru]) =: One;
    (fgam, IntT) |- (proccall "iaiia2u"  [arr[one]; one; arr[one]]) =: One;
    (fgam, IntT) |- (proccall "iabi2u"   [arr[one]; tru; one]) =: One;
    (fgam, IntT) |- (proccall "iabb2u"   [arr[one]; tru; tru]) =: One;
    (fgam, IntT) |- (proccall "iabia2u"  [arr[one]; tru; arr[one]]) =: One;
    (fgam, IntT) |- (proccall "iaiai2u"  [arr[one]; arr[one]; one]) =: One;
    (fgam, IntT) |- (proccall "iaiab2u"  [arr[one]; arr[one]; tru]) =: One;
    (fgam, IntT) |- (proccall "iaiaia2u" [arr[one]; arr[one]; arr[one]]) =: One;

    (fgam, BoolT) |- (proccall "iii2u"    [one; one; one]) =: One;
    (fgam, BoolT) |- (proccall "iib2u"    [one; one; tru]) =: One;
    (fgam, BoolT) |- (proccall "iiia2u"   [one; one; arr[one]]) =: One;
    (fgam, BoolT) |- (proccall "ibi2u"    [one; tru; one]) =: One;
    (fgam, BoolT) |- (proccall "ibb2u"    [one; tru; tru]) =: One;
    (fgam, BoolT) |- (proccall "ibia2u"   [one; tru; arr[one]]) =: One;
    (fgam, BoolT) |- (proccall "iiai2u"   [one; arr[one]; one]) =: One;
    (fgam, BoolT) |- (proccall "iiab2u"   [one; arr[one]; tru]) =: One;
    (fgam, BoolT) |- (proccall "iiaia2u"  [one; arr[one]; arr[one]]) =: One;
    (fgam, BoolT) |- (proccall "bii2u"    [tru; one; one]) =: One;
    (fgam, BoolT) |- (proccall "bib2u"    [tru; one; tru]) =: One;
    (fgam, BoolT) |- (proccall "biia2u"   [tru; one; arr[one]]) =: One;
    (fgam, BoolT) |- (proccall "bbi2u"    [tru; tru; one]) =: One;
    (fgam, BoolT) |- (proccall "bbb2u"    [tru; tru; tru]) =: One;
    (fgam, BoolT) |- (proccall "bbia2u"   [tru; tru; arr[one]]) =: One;
    (fgam, BoolT) |- (proccall "biai2u"   [tru; arr[one]; one]) =: One;
    (fgam, BoolT) |- (proccall "biab2u"   [tru; arr[one]; tru]) =: One;
    (fgam, BoolT) |- (proccall "biaia2u"  [tru; arr[one]; arr[one]]) =: One;
    (fgam, BoolT) |- (proccall "iaii2u"   [arr[one]; one; one]) =: One;
    (fgam, BoolT) |- (proccall "iaib2u"   [arr[one]; one; tru]) =: One;
    (fgam, BoolT) |- (proccall "iaiia2u"  [arr[one]; one; arr[one]]) =: One;
    (fgam, BoolT) |- (proccall "iabi2u"   [arr[one]; tru; one]) =: One;
    (fgam, BoolT) |- (proccall "iabb2u"   [arr[one]; tru; tru]) =: One;
    (fgam, BoolT) |- (proccall "iabia2u"  [arr[one]; tru; arr[one]]) =: One;
    (fgam, BoolT) |- (proccall "iaiai2u"  [arr[one]; arr[one]; one]) =: One;
    (fgam, BoolT) |- (proccall "iaiab2u"  [arr[one]; arr[one]; tru]) =: One;
    (fgam, BoolT) |- (proccall "iaiaia2u" [arr[one]; arr[one]; arr[one]]) =: One;

    (fgam, IntT) |- (proccall "piaup" [arr[];arr[];arr[]]) =: One;
    (fgam, IntT) |- (proccall "piaup" [arr[];arr[arr[]];arr[]]) =: One;
    (fgam, IntT) |- (proccall "piaup" [arr[];arr[];arr[arr[]]]) =: One;
    (fgam, IntT) |- (proccall "piaup" [arr[];arr[arr[]];arr[arr[]]]) =: One;
    (fgam, IntT) |- (proccall "piaup" [arr[];arr[];arr[arr[arr[]]]]) =: One;
    (fgam, IntT) |- (proccall "piaup" [arr[];arr[arr[]];arr[arr[arr[]]]]) =: One;
    (fgam, IntT) |- (proccall "piaup" [arr[one];arr[];arr[]]) =: One;
    (fgam, IntT) |- (proccall "piaup" [arr[one];arr[arr[one]];arr[]]) =: One;
    (fgam, IntT) |- (proccall "piaup" [arr[one];arr[];arr[arr[]]]) =: One;
    (fgam, IntT) |- (proccall "piaup" [arr[one];arr[arr[one]];arr[arr[]]]) =: One;
    (fgam, IntT) |- (proccall "piaup" [arr[one];arr[];arr[arr[arr[one]]]]) =: One;
    (fgam, IntT) |- (proccall "piaup" [arr[one];arr[arr[one]];arr[arr[arr[one]]]]) =: One;
    (fgam, IntT) |- (proccall "piadown" (List.rev [arr[];arr[];arr[]])) =: One;
    (fgam, IntT) |- (proccall "piadown" (List.rev [arr[];arr[arr[]];arr[]])) =: One;
    (fgam, IntT) |- (proccall "piadown" (List.rev [arr[];arr[];arr[arr[]]])) =: One;
    (fgam, IntT) |- (proccall "piadown" (List.rev [arr[];arr[arr[]];arr[arr[]]])) =: One;
    (fgam, IntT) |- (proccall "piadown" (List.rev [arr[];arr[];arr[arr[arr[]]]])) =: One;
    (fgam, IntT) |- (proccall "piadown" (List.rev [arr[];arr[arr[]];arr[arr[arr[]]]])) =: One;
    (fgam, IntT) |- (proccall "piadown" (List.rev [arr[one];arr[];arr[]])) =: One;
    (fgam, IntT) |- (proccall "piadown" (List.rev [arr[one];arr[arr[one]];arr[]])) =: One;
    (fgam, IntT) |- (proccall "piadown" (List.rev [arr[one];arr[];arr[arr[]]])) =: One;
    (fgam, IntT) |- (proccall "piadown" (List.rev [arr[one];arr[arr[one]];arr[arr[]]])) =: One;
    (fgam, IntT) |- (proccall "piadown" (List.rev [arr[one];arr[];arr[arr[arr[one]]]])) =: One;
    (fgam, IntT) |- (proccall "piadown" (List.rev [arr[one];arr[arr[one]];arr[arr[arr[one]]]])) =: One;

    (fgam, IntT) =/= (proccall "iii2u"    [one; one; one; one]);
    (fgam, IntT) =/= (proccall "iib2u"    [one; one; one; tru]);
    (fgam, IntT) =/= (proccall "iiia2u"   [one; one; one; arr[one]]);
    (fgam, IntT) =/= (proccall "ibi2u"    [one; one; tru; one]);
    (fgam, IntT) =/= (proccall "ibb2u"    [one; one; tru; tru]);
    (fgam, IntT) =/= (proccall "ibia2u"   [one; one; tru; arr[one]]);
    (fgam, IntT) =/= (proccall "iiai2u"   [one; one; arr[one]; one]);
    (fgam, IntT) =/= (proccall "iiab2u"   [one; one; arr[one]; tru]);
    (fgam, IntT) =/= (proccall "iiaia2u"  [one; one; arr[one]; arr[one]]);
    (fgam, IntT) =/= (proccall "bii2u"    [one; tru; one; one]);
    (fgam, IntT) =/= (proccall "bib2u"    [one; tru; one; tru]);
    (fgam, IntT) =/= (proccall "biia2u"   [one; tru; one; arr[one]]);
    (fgam, IntT) =/= (proccall "bbi2u"    [one; tru; tru; one]);
    (fgam, IntT) =/= (proccall "bbb2u"    [one; tru; tru; tru]);
    (fgam, IntT) =/= (proccall "bbia2u"   [one; tru; tru; arr[one]]);
    (fgam, IntT) =/= (proccall "biai2u"   [one; tru; arr[one]; one]);
    (fgam, IntT) =/= (proccall "biab2u"   [one; tru; arr[one]; tru]);
    (fgam, IntT) =/= (proccall "biaia2u"  [one; tru; arr[one]; arr[one]]);
    (fgam, IntT) =/= (proccall "iaii2u"   [one; arr[one]; one; one]);
    (fgam, IntT) =/= (proccall "iaib2u"   [one; arr[one]; one; tru]);
    (fgam, IntT) =/= (proccall "iaiia2u"  [one; arr[one]; one; arr[one]]);
    (fgam, IntT) =/= (proccall "iabi2u"   [one; arr[one]; tru; one]);
    (fgam, IntT) =/= (proccall "iabb2u"   [one; arr[one]; tru; tru]);
    (fgam, IntT) =/= (proccall "iabia2u"  [one; arr[one]; tru; arr[one]]);
    (fgam, IntT) =/= (proccall "iaiai2u"  [one; arr[one]; arr[one]; one]);
    (fgam, IntT) =/= (proccall "iaiab2u"  [one; arr[one]; arr[one]; tru]);
    (fgam, IntT) =/= (proccall "iaiaia2u" [one; arr[one]; arr[one]; arr[one]]);

    (fgam, IntT) =/= (proccall "iii2u"    [tru; one; one; one]);
    (fgam, IntT) =/= (proccall "iib2u"    [tru; one; one; tru]);
    (fgam, IntT) =/= (proccall "iiia2u"   [tru; one; one; arr[one]]);
    (fgam, IntT) =/= (proccall "ibi2u"    [tru; one; tru; one]);
    (fgam, IntT) =/= (proccall "ibb2u"    [tru; one; tru; tru]);
    (fgam, IntT) =/= (proccall "ibia2u"   [tru; one; tru; arr[one]]);
    (fgam, IntT) =/= (proccall "iiai2u"   [tru; one; arr[one]; one]);
    (fgam, IntT) =/= (proccall "iiab2u"   [tru; one; arr[one]; tru]);
    (fgam, IntT) =/= (proccall "iiaia2u"  [tru; one; arr[one]; arr[one]]);
    (fgam, IntT) =/= (proccall "bii2u"    [tru; tru; one; one]);
    (fgam, IntT) =/= (proccall "bib2u"    [tru; tru; one; tru]);
    (fgam, IntT) =/= (proccall "biia2u"   [tru; tru; one; arr[one]]);
    (fgam, IntT) =/= (proccall "bbi2u"    [tru; tru; tru; one]);
    (fgam, IntT) =/= (proccall "bbb2u"    [tru; tru; tru; tru]);
    (fgam, IntT) =/= (proccall "bbia2u"   [tru; tru; tru; arr[one]]);
    (fgam, IntT) =/= (proccall "biai2u"   [tru; tru; arr[one]; one]);
    (fgam, IntT) =/= (proccall "biab2u"   [tru; tru; arr[one]; tru]);
    (fgam, IntT) =/= (proccall "biaia2u"  [tru; tru; arr[one]; arr[one]]);
    (fgam, IntT) =/= (proccall "iaii2u"   [tru; arr[one]; one; one]);
    (fgam, IntT) =/= (proccall "iaib2u"   [tru; arr[one]; one; tru]);
    (fgam, IntT) =/= (proccall "iaiia2u"  [tru; arr[one]; one; arr[one]]);
    (fgam, IntT) =/= (proccall "iabi2u"   [tru; arr[one]; tru; one]);
    (fgam, IntT) =/= (proccall "iabb2u"   [tru; arr[one]; tru; tru]);
    (fgam, IntT) =/= (proccall "iabia2u"  [tru; arr[one]; tru; arr[one]]);
    (fgam, IntT) =/= (proccall "iaiai2u"  [tru; arr[one]; arr[one]; one]);
    (fgam, IntT) =/= (proccall "iaiab2u"  [tru; arr[one]; arr[one]; tru]);
    (fgam, IntT) =/= (proccall "iaiaia2u" [tru; arr[one]; arr[one]; arr[one]]);

    (fgam, IntT) =/= (proccall "u2u" [one]);
    (fgam, IntT) =/= (proccall "u2u" [tru]);
    (fgam, IntT) =/= (proccall "u2u" [arr[]]);
    (fgam, IntT) =/= (proccall "u2u" [arr[arr[]]]);
    (fgam, IntT) =/= (proccall "u2u" [arr[arr[arr[]]]]);

    (fgam, IntT) =/= (proccall "u2i" []);
    (fgam, IntT) =/= (proccall "u2b" []);
    (fgam, IntT) =/= (proccall "u2ia" []);
    (fgam, IntT) =/= (proccall "i2i" [one]);
    (fgam, IntT) =/= (proccall "i2b" [one]);
    (fgam, IntT) =/= (proccall "i2ia" [one]);
    (fgam, IntT) =/= (proccall "b2i" [tru]);
    (fgam, IntT) =/= (proccall "b2b" [tru]);
    (fgam, IntT) =/= (proccall "b2ia" [tru]);
    (fgam, IntT) =/= (proccall "ia2i" [arr[one]]);
    (fgam, IntT) =/= (proccall "ia2b" [arr[one]]);
    (fgam, IntT) =/= (proccall "ia2ia" [arr[one]]);
    (fgam, IntT) =/= (proccall "ii2i" [one;one]);
    (fgam, IntT) =/= (proccall "ii2b" [one;one]);
    (fgam, IntT) =/= (proccall "ii2ia" [one;one]);
    (fgam, IntT) =/= (proccall "ib2i" [one;tru]);
    (fgam, IntT) =/= (proccall "ib2b" [one;tru]);
    (fgam, IntT) =/= (proccall "ib2ia" [one;tru]);
    (fgam, IntT) =/= (proccall "iia2i" [one;arr[one]]);
    (fgam, IntT) =/= (proccall "iia2b" [one;arr[one]]);
    (fgam, IntT) =/= (proccall "iia2ia" [one;arr[one]]);
    (fgam, IntT) =/= (proccall "bi2i" [tru;one]);
    (fgam, IntT) =/= (proccall "bi2b" [tru;one]);
    (fgam, IntT) =/= (proccall "bi2ia" [tru;one]);
    (fgam, IntT) =/= (proccall "bb2i" [tru;tru]);
    (fgam, IntT) =/= (proccall "bb2b" [tru;tru]);
    (fgam, IntT) =/= (proccall "bb2ia" [tru;tru]);
    (fgam, IntT) =/= (proccall "bia2i" [tru;arr[one]]);
    (fgam, IntT) =/= (proccall "bia2b" [tru;arr[one]]);
    (fgam, IntT) =/= (proccall "bia2ia" [tru;arr[one]]);
    (fgam, IntT) =/= (proccall "iai2i" [arr[one];one]);
    (fgam, IntT) =/= (proccall "iai2b" [arr[one];one]);
    (fgam, IntT) =/= (proccall "iai2ia" [arr[one];one]);
    (fgam, IntT) =/= (proccall "iab2i" [arr[one];tru]);
    (fgam, IntT) =/= (proccall "iab2b" [arr[one];tru]);
    (fgam, IntT) =/= (proccall "iab2ia" [arr[one];tru]);
    (fgam, IntT) =/= (proccall "iaia2i" [arr[one];arr[one]]);
    (fgam, IntT) =/= (proccall "iaia2b" [arr[one];arr[one]]);
    (fgam, IntT) =/= (proccall "iaia2ia" [arr[one];arr[one]]);
    (fgam, IntT) =/= (proccall "i2ii" [one]);
    (fgam, IntT) =/= (proccall "i2ib" [one]);
    (fgam, IntT) =/= (proccall "i2iia" [one]);
    (fgam, IntT) =/= (proccall "b2ii" [tru]);
    (fgam, IntT) =/= (proccall "b2ib" [tru]);
    (fgam, IntT) =/= (proccall "b2iia" [tru]);
    (fgam, IntT) =/= (proccall "ia2ii" [arr[one]]);
    (fgam, IntT) =/= (proccall "ia2ib" [arr[one]]);
    (fgam, IntT) =/= (proccall "ia2iia" [arr[one]]);
    (fgam, IntT) =/= (proccall "i2bi" [one]);
    (fgam, IntT) =/= (proccall "i2bb" [one]);
    (fgam, IntT) =/= (proccall "i2bia" [one]);
    (fgam, IntT) =/= (proccall "b2bi" [tru]);
    (fgam, IntT) =/= (proccall "b2bb" [tru]);
    (fgam, IntT) =/= (proccall "b2bia" [tru]);
    (fgam, IntT) =/= (proccall "ia2bi" [arr[one]]);
    (fgam, IntT) =/= (proccall "ia2bb" [arr[one]]);
    (fgam, IntT) =/= (proccall "ia2bia" [arr[one]]);
    (fgam, IntT) =/= (proccall "i2iai" [one]);
    (fgam, IntT) =/= (proccall "i2iab" [one]);
    (fgam, IntT) =/= (proccall "i2iaia" [one]);
    (fgam, IntT) =/= (proccall "b2iai" [tru]);
    (fgam, IntT) =/= (proccall "b2iab" [tru]);
    (fgam, IntT) =/= (proccall "b2iaia" [tru]);
    (fgam, IntT) =/= (proccall "ia2iai" [arr[one]]);
    (fgam, IntT) =/= (proccall "ia2iab" [arr[one]]);
    (fgam, IntT) =/= (proccall "ia2iaia" [arr[one]]);
    (fgam, IntT) =/= (proccall "ii2ii" [one;one]);
    (fgam, IntT) =/= (proccall "ii2ib" [one;one]);
    (fgam, IntT) =/= (proccall "ii2iia" [one;one]);
    (fgam, IntT) =/= (proccall "ib2ii" [one;tru]);
    (fgam, IntT) =/= (proccall "ib2ib" [one;tru]);
    (fgam, IntT) =/= (proccall "ib2iia" [one;tru]);
    (fgam, IntT) =/= (proccall "iia2ii" [one;arr[one]]);
    (fgam, IntT) =/= (proccall "iia2ib" [one;arr[one]]);
    (fgam, IntT) =/= (proccall "iia2iia" [one;arr[one]]);
    (fgam, IntT) =/= (proccall "bi2bi" [tru;one]);
    (fgam, IntT) =/= (proccall "bi2bb" [tru;one]);
    (fgam, IntT) =/= (proccall "bi2bia" [tru;one]);
    (fgam, IntT) =/= (proccall "bb2bi" [tru;tru]);
    (fgam, IntT) =/= (proccall "bb2bb" [tru;tru]);
    (fgam, IntT) =/= (proccall "bb2bia" [tru;tru]);
    (fgam, IntT) =/= (proccall "bia2bi" [tru;arr[one]]);
    (fgam, IntT) =/= (proccall "bia2bb" [tru;arr[one]]);
    (fgam, IntT) =/= (proccall "bia2bia" [tru;arr[one]]);
    (fgam, IntT) =/= (proccall "iai2iai" [arr[one];one]);
    (fgam, IntT) =/= (proccall "iai2iab" [arr[one];one]);
    (fgam, IntT) =/= (proccall "iai2iaia" [arr[one];one]);
    (fgam, IntT) =/= (proccall "iab2iai" [arr[one];tru]);
    (fgam, IntT) =/= (proccall "iab2iab" [arr[one];tru]);
    (fgam, IntT) =/= (proccall "iab2iaia" [arr[one];tru]);
    (fgam, IntT) =/= (proccall "iaia2iai" [arr[one];arr[one]]);
    (fgam, IntT) =/= (proccall "iaia2iab" [arr[one];arr[one]]);

    (fgam, IntT) =/= (proccall "iaia2i" [arr[];arr[one]]);
    (fgam, IntT) =/= (proccall "iaia2i" [arr[one];arr[]]);
    (fgam, IntT) =/= (proccall "iaia2i" [arr[];arr[]]);

    (fgam, IntT) =/= (proccall "iaup" [arr[]; arr[]; arr[]]);
    (fgam, IntT) =/= (proccall "iaup" [arr[]; arr[arr[]]; arr[]]);
    (fgam, IntT) =/= (proccall "iaup" [arr[]; arr[]; arr[arr[]]]);
    (fgam, IntT) =/= (proccall "iaup" [arr[]; arr[arr[]]; arr[arr[]]]);
    (fgam, IntT) =/= (proccall "iaup" [arr[]; arr[arr[]]; arr[arr[arr[]]]]);
    (fgam, IntT) =/= (proccall "iaup" [arr[one]; arr[]; arr[]]);
    (fgam, IntT) =/= (proccall "iaup" [arr[one]; arr[arr[]]; arr[]]);
    (fgam, IntT) =/= (proccall "iaup" [arr[one]; arr[]; arr[arr[]]]);
    (fgam, IntT) =/= (proccall "iaup" [arr[one]; arr[arr[]]; arr[arr[]]]);
    (fgam, IntT) =/= (proccall "iaup" [arr[one]; arr[arr[]]; arr[arr[arr[]]]]);
    (fgam, IntT) =/= (proccall "iaup" [arr[]; arr[arr[one]]; arr[]]);
    (fgam, IntT) =/= (proccall "iaup" [arr[]; arr[arr[one]]; arr[arr[]]]);
    (fgam, IntT) =/= (proccall "iaup" [arr[]; arr[arr[one]]; arr[arr[arr[]]]]);
    (fgam, IntT) =/= (proccall "iaup" [arr[]; arr[arr[]]; arr[arr[arr[one]]]]);
    (fgam, IntT) =/= (proccall "iaup" [arr[one]; arr[arr[one]]; arr[arr[arr[one]]]]);

    (fgam, IntT) =/= (proccall "iadown" (List.rev [arr[]; arr[]; arr[]]));
    (fgam, IntT) =/= (proccall "iadown" (List.rev [arr[]; arr[arr[]]; arr[]]));
    (fgam, IntT) =/= (proccall "iadown" (List.rev [arr[]; arr[]; arr[arr[]]]));
    (fgam, IntT) =/= (proccall "iadown" (List.rev [arr[]; arr[arr[]]; arr[arr[]]]));
    (fgam, IntT) =/= (proccall "iadown" (List.rev [arr[]; arr[arr[]]; arr[arr[arr[]]]]));
    (fgam, IntT) =/= (proccall "iadown" (List.rev [arr[one]; arr[]; arr[]]));
    (fgam, IntT) =/= (proccall "iadown" (List.rev [arr[one]; arr[arr[]]; arr[]]));
    (fgam, IntT) =/= (proccall "iadown" (List.rev [arr[one]; arr[]; arr[arr[]]]));
    (fgam, IntT) =/= (proccall "iadown" (List.rev [arr[one]; arr[arr[]]; arr[arr[]]]));
    (fgam, IntT) =/= (proccall "iadown" (List.rev [arr[one]; arr[arr[]]; arr[arr[arr[]]]]));
    (fgam, IntT) =/= (proccall "iadown" (List.rev [arr[]; arr[arr[one]]; arr[]]));
    (fgam, IntT) =/= (proccall "iadown" (List.rev [arr[]; arr[arr[one]]; arr[arr[]]]));
    (fgam, IntT) =/= (proccall "iadown" (List.rev [arr[]; arr[arr[one]]; arr[arr[arr[]]]]));
    (fgam, IntT) =/= (proccall "iadown" (List.rev [arr[]; arr[arr[]]; arr[arr[arr[one]]]]));
    (fgam, IntT) =/= (proccall "iadown" (List.rev [arr[one]; arr[arr[one]]; arr[arr[arr[one]]]]));

    ()

let test_callable _ =
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
	empty |- (func "f" [aunderscore tint] [tint] (return [int 3L])) =: (IntT, IntT);
	empty |- (func "f" [aunderscore tint] [tint] (return [(funccall "f" [int 3L])])) =: (IntT, IntT);

	(* _::_, [x] *)
	empty |- (func "f" [(aid "x" tint); (aid "y" tint)] [tint] (return [id "x"])) =: (TupleT [IntT; IntT], IntT);
	empty |- (func "f" [(aid "x" tint); (aid "y" tint)] [tint] (return [id "y"])) =: (TupleT [IntT; IntT], IntT);
	empty |- (func "f" [(aid "x" tint); (aunderscore tint)] [tint] (return [id "x"]))
						=: (TupleT [IntT; IntT], IntT);
	empty |- (func "f" [aunderscore tint; aunderscore tint] [tint] (return [int 3L]))
						=: (TupleT [IntT; IntT], IntT);
	empty |- (func "f" [aunderscore tint; aid "x" tint; aunderscore tint] [tint] (return [id "x"]))
						=: (TupleT [IntT; IntT; IntT], IntT);

	(* [], _::_ *)
	empty |- (func "f" [] [tint; tint] (return [int 3L; int 2L])) =: (UnitT, TupleT [IntT; IntT]);
	empty |- (func "f" [] [tint; tint; tbool] (return [int 3L; int 1L; bool true]))
					  =: (UnitT, TupleT [IntT; IntT; BoolT]);

	(* [x], _::_ *)
	empty |- (func "f" [(aid "x" tint)] [tint; tint] (return [id "x"; id "x"])) =: (IntT, TupleT [IntT; IntT]);
	empty |- (func "f" [(aid "x" tbool)] [tbool; tbool; tbool] (return [id "x"; id "x"; id "x"]))
					  =: (BoolT, TupleT [BoolT; BoolT; BoolT]);

	(* _::_, _::_ *)
	empty |- (func "f" [(aid "x" tint); (aid "y" tint)] [tint;tint] (return [(id "y"); (id "x")]))
						=: (TupleT [IntT; IntT], TupleT [IntT; IntT]);
	empty |- (func "f" [(aid "x" tint); (aid "y" tbool)] [tbool;tint] (return [(id "y"); (id "x")]))
						=: (TupleT [IntT; BoolT], TupleT [BoolT; IntT]);
	empty |- (func "f" [aid "x" tint; aunderscore tint; aunderscore tint] [tint; tint; tint]
					 (return [id "x"; id "x"; id "x"])) =: (TupleT [IntT; IntT; IntT], TupleT [IntT; IntT; IntT]);

	(* recursion *)
	empty |- (func "g" [aid "x" tint] [tint] (block [(asgn (id "x") (funccall "g" [id "x"])); (return [id "x"])]))
							=: (IntT, IntT);

	(* wrong return type *)
	empty =/= (func "f" [aid "x" tint] [tbool] (return [id "x"]));

	(* dup args *)
	empty =/= (func "has_dup" [(aid "x" tint); (aid "x" tint)] [tint] (return [id "x"]));

	(* unbound variable y *)
	empty =/= (func "f" [aid "x" tint] [tint] (return [id "y"]));

	(* does not actually return *)
	empty =/= (func "f" [] [tint] (block []));

	(* procedures *)

	(* [] *)
	empty |- (proc "f" [] (proccall "f" [])) =: (UnitT, UnitT);

	(* [x] *)
	empty |- (proc "f" [aid "x" tint] (proccall "f" [id "x"])) =: (IntT, UnitT);
	empty |- (proc "f" [aunderscore tint] (proccall "f" [int 3L])) =: (IntT, UnitT);

	(* _::_ *)
	empty |- (proc "f" [aid "x" tint; aid "y" tint] (proccall "f" [id "x"; id "x"]))
						=: (TupleT [IntT; IntT], UnitT);
	empty |- (proc "f" [aunderscore tint; aunderscore tint] (proccall "f" [int 3L; int 3L]))
						=: (TupleT [IntT; IntT], UnitT);

	(* dup args *)
	empty =/= (proc "f" [aid "x" tint; aid "x" tint] (proccall "f" []));

	(* unbound variable y *)
	empty =/= (proc "f" [] (proccall "y" []));

	(* non-empty context *)
	let f_binded = {empty with locals = Context.bind Context.empty "f" (Function (IntT, IntT))} in
	let g_binded = {empty with locals = Context.bind Context.empty "g" (Function (UnitT, UnitT))} in

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

let test_prog _ =
	let open Pos in
	let open TestProg in
	let avar_u = [] in
	let avar_i1 = [aid "x" tint] in
	let avar_i2 = [aid "x" tint; aid "y" tint] in
	let avar_i3 = [aid "x" tint; aid "y" tint; aid "z" tint] in
	let typ_i1 = [tint] in
	let typ_i2 = [tint; tint] in
	let typ_i3 = [tint; tint; tint] in
	let ret_i1 = return [int 3L] in
	let ret_i2 = return [int 3L; int 3L] in
	let ret_i3 = return [int 3L; int 3L; int 3L] in
	let emp = block [] in

	let f_u2i = func "f_u2i" avar_u typ_i1 ret_i1 in
	let f_u2ii = func "f_u2ii" avar_u typ_i2 ret_i2 in
	let f_u2iii = func "f_u2iii" avar_u typ_i3 ret_i3 in

	let f_i2i = func "f_i2i" avar_i1 typ_i1 ret_i1 in
	let f_i2ii = func "f_i2ii" avar_i1 typ_i2 ret_i2 in
	let f_i2iii = func "f_i2iii" avar_i1 typ_i3 ret_i3 in

	let f_ii2i = func "f_ii2i" avar_i2 typ_i1 ret_i1 in
	let f_ii2ii = func "f_ii2ii" avar_i2 typ_i2 ret_i2 in
	let f_ii2iii = func "f_ii2iii" avar_i2 typ_i3 ret_i3 in

	let f_iii2i = func "f_iii2i" avar_i3 typ_i1 ret_i1 in
	let f_iii2ii = func "f_iii2ii" avar_i3 typ_i2 ret_i2 in
	let f_iii2iii = func "f_iii2iii" avar_i3 typ_i3 ret_i3 in

	let p_u2u = proc "p_u2u" avar_u emp in
	let p_i2u = proc "p_i2u" avar_i1 emp in
	let p_ii2u = proc "p_ii2u" avar_i2 emp in
	let p_iii2u = proc "p_iii2u" avar_i3 emp in

	let fd_u2i = funcdecl "f_u2i" avar_u typ_i1 in
	let fd_u2ii = funcdecl "f_u2ii" avar_u typ_i2 in
	let fd_u2iii = funcdecl "f_u2iii" avar_u typ_i3 in

	let fd_i2i = funcdecl "f_i2i" avar_i1 typ_i1 in
	let fd_i2ii = funcdecl "f_i2ii" avar_i1 typ_i2 in
	let fd_i2iii = funcdecl "f_i2iii" avar_i1 typ_i3 in

	let fd_ii2i = funcdecl "f_ii2i" avar_i2 typ_i1 in
	let fd_ii2ii = funcdecl "f_ii2ii" avar_i2 typ_i2 in
	let fd_ii2iii = funcdecl "f_ii2iii" avar_i2 typ_i3 in

	let fd_iii2i = funcdecl "f_iii2i" avar_i3 typ_i1 in
	let fd_iii2ii = funcdecl "f_iii2ii" avar_i3 typ_i2 in
	let fd_iii2iii = funcdecl "f_iii2iii" avar_i3 typ_i3 in

	let pd_u2u = procdecl "p_u2u" avar_u in
	let pd_i2u = procdecl "p_i2u" avar_i1 in
	let pd_ii2u = procdecl "p_ii2u" avar_i2 in
	let pd_iii2u = procdecl "p_iii2u" avar_i3 in

	let diff_fd_u2i = funcdecl "diff_f_u2i" avar_u typ_i1 in
	let diff_fd_u2ii = funcdecl "diff_f_u2ii" avar_u typ_i2 in
	let diff_fd_u2iii = funcdecl "diff_f_u2iii" avar_u typ_i3 in

	let diff_fd_i2i = funcdecl "diff_f_i2i" avar_i1 typ_i1 in
	let diff_fd_i2ii = funcdecl "diff_f_i2ii" avar_i1 typ_i2 in
	let diff_fd_i2iii = funcdecl "diff_f_i2iii" avar_i1 typ_i3 in

	let diff_fd_ii2i = funcdecl "diff_f_ii2i" avar_i2 typ_i1 in
	let diff_fd_ii2ii = funcdecl "diff_f_ii2ii" avar_i2 typ_i2 in
	let diff_fd_ii2iii = funcdecl "diff_f_ii2iii" avar_i2 typ_i3 in

	let diff_fd_iii2i = funcdecl "diff_f_iii2i" avar_i3 typ_i1 in
	let diff_fd_iii2ii = funcdecl "diff_f_iii2ii" avar_i3 typ_i2 in
	let diff_fd_iii2iii = funcdecl "diff_f_iii2iii" avar_i3 typ_i3 in

	let diff_pd_u2u = procdecl "diff_p_u2u" avar_u in
	let diff_pd_i2u = procdecl "diff_p_i2u" avar_i1 in
	let diff_pd_ii2u = procdecl "diff_p_ii2u" avar_i2 in
	let diff_pd_iii2u = procdecl "diff_p_iii2u" avar_i3 in

	let wrong_fd_u2i = funcdecl "f_u2i" avar_u typ_i2 in
	let wrong_fd_u2ii = funcdecl "f_u2ii" avar_u typ_i1 in
	let wrong_fd_u2iii = funcdecl "f_u2iii" avar_u typ_i1 in

	let wrong_fd_i2i = funcdecl "f_i2i" avar_i1 typ_i2 in
	let wrong_fd_i2ii = funcdecl "f_i2ii" avar_i1 typ_i1 in
	let wrong_fd_i2iii = funcdecl "f_i2iii" avar_i1 typ_i1 in

	let wrong_fd_ii2i = funcdecl "f_ii2i" avar_i2 typ_i2 in
	let wrong_fd_ii2ii = funcdecl "f_ii2ii" avar_i2 typ_i1 in
	let wrong_fd_ii2iii = funcdecl "f_ii2iii" avar_i2 typ_i1 in

	let wrong_fd_iii2i = funcdecl "f_iii2i" avar_i3 typ_i2 in
	let wrong_fd_iii2ii = funcdecl "f_iii2ii" avar_i3 typ_i1 in
	let wrong_fd_iii2iii = funcdecl "f_iii2iii" avar_i3 typ_i1 in

	let wrong_pd_u2u = procdecl "p_u2u" avar_i1 in
	let wrong_pd_i2u = procdecl "p_i2u" avar_i2 in
	let wrong_pd_ii2u = procdecl "p_ii2u" avar_i1 in
	let wrong_pd_iii2u = procdecl "p_iii2u" avar_i1 in

	let call_fd_u2i = func "f" avar_u typ_i1 (return [funccall "f_u2i" []]) in

        let call_inter_prog = prog [] [] [] [call_fd_u2i] in

        let everything_prog = prog [] [] [] [f_u2i; f_u2ii; f_u2iii; f_i2i; f_i2ii; f_i2iii;
													 f_ii2i; f_ii2ii; f_ii2iii; f_iii2i; f_iii2ii; f_iii2iii;
													 p_u2u; p_i2u; p_ii2u; p_iii2u] in

        let everything_inter = interface [] [] [fd_u2i; fd_u2ii; fd_u2iii; fd_i2i; fd_i2ii; fd_i2iii;
													 fd_ii2i; fd_ii2ii; fd_ii2iii; fd_iii2i; fd_iii2ii; fd_iii2iii;
													 pd_u2u; pd_i2u; pd_ii2u; pd_iii2u] in

        let diff_everything_inter = interface []
                                              []
                                              [diff_fd_u2i; diff_fd_u2ii; diff_fd_u2iii; diff_fd_i2i;
																				 diff_fd_i2ii; diff_fd_i2iii; diff_fd_ii2i; diff_fd_ii2ii;
																				 diff_fd_ii2iii; diff_fd_iii2i; diff_fd_iii2ii; diff_fd_iii2iii;
													 							 diff_pd_u2u; diff_pd_i2u; diff_pd_ii2u; diff_pd_iii2u] in

        let wrong_everything_inter = interface []
                                               []
                                               [wrong_fd_u2i; wrong_fd_u2ii; wrong_fd_u2iii; wrong_fd_i2i;
																				 wrong_fd_i2ii; wrong_fd_i2iii; wrong_fd_ii2i; wrong_fd_ii2ii;
																				 wrong_fd_ii2iii; wrong_fd_iii2i; wrong_fd_iii2ii; wrong_fd_iii2iii;
													 							 wrong_pd_u2u; wrong_pd_i2u; wrong_pd_ii2u; wrong_pd_iii2u] in

	let only_prog = fullprog "fakeprogname" everything_prog [] in
	() =: only_prog;

	let alldiff_fullprog = fullprog "fakeprogname" everything_prog [diff_everything_inter] in
	() =: alldiff_fullprog;

	let allsame_fullprog = fullprog "fakeprogname" everything_prog [everything_inter] in
	() =: allsame_fullprog;

	let mult_fullprog = fullprog "fakeprogname" everything_prog [everything_inter; everything_inter; diff_everything_inter] in
	() =: mult_fullprog;

	let wrong_fullprog = fullprog "fakeprogname" everything_prog [everything_inter; diff_everything_inter; wrong_everything_inter]
	in
	() =/= wrong_fullprog;

	let call_inter_fullprog = fullprog "fakeprogname" call_inter_prog [everything_inter] in
	() =: call_inter_fullprog;

	()

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* ! DON'T FORGET TO ADD YOUR TESTS HERE                                     ! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let main () =
    "suite" >::: [
        "test_expr" >:: test_expr;
        "test_stmt" >:: test_stmt;
		"test_callable" >:: test_callable;
		"test_prog" >:: test_prog;
    ] |> run_test_tt_main

let _ = main ()
