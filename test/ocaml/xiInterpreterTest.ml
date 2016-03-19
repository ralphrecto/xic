open Core.Std
open OUnit
open TestUtil
open Xi_interpreter

(* Custom equalities *)
module ValueEq = struct
  let (===) (a: value) (b: value) : unit =
    assert_equal ~printer:string_of_value a b
end

module ConfigEq = struct
  type config = context * value option

  let string_of_id_option (x: id option) =
    Option.value x ~default:"_"

  let string_of_value_option (v: value option) =
    Option.(value (v >>| string_of_value) ~default:"⊥")

  let string_of_store (s: store) =
    match s with
    | Value v -> string_of_value v
    | Function (args, _) ->
        let args_strings = List.map ~f:string_of_id_option args in
        sprintf "(%s) {...}" (Util.commas args_strings)

  let string_of_store_option (s: store option) =
    Option.(value (s >>| string_of_store) ~default:"⊥")

  let string_of_context (c: context) =
    String.Map.to_alist c
    |> List.map ~f:(fun (x, v) -> sprintf "%s->%s" x (string_of_store_option v))
    |> Util.commas
    |> sprintf "{%s}"

  let string_of_config ((c, v): config) =
    sprintf "(%s, %s)" (string_of_context c) (string_of_value_option v)

  let (===) (a: config) (b: config) : unit =
    let cmp (c1, v1) (c2, v2) =
      String.Map.equal (=) c1 c2 && v1 = v2
    in
    assert_equal ~cmp ~printer:string_of_config a b
end

(* Context helpers *)
let empty = String.Map.empty

let val_ v = Value v
let fun_ args s = Function (args, s)

let gam (xs: (string * store option) list) : context =
  String.Map.of_alist_exn xs

let vals (vs: (string * value) list) : context =
  gam (List.map vs ~f:(fun (v, t) -> (v, Some (val_ t))))

let funcs (fs: (string * (string option) list * Typecheck.stmt) list) : context =
  gam (List.map fs ~f:(fun (f, args, s) -> (f, Some (fun_ args s))))

let test_eval_expr () =
  let open ValueEq in
  let open Typecheck.Abbreviations in

  Int 1L === eval_expr empty (one);
  Int 2L === eval_expr empty (two);

  Int 1L === eval_expr empty (tru);
  Int 0L === eval_expr empty (fls);

  Array (ref [Int 102L;Int 111L;Int 111L]) === eval_expr empty (string "foo");

  Int 97L === eval_expr empty (char 'a');
  Int 98L === eval_expr empty (char 'b');

  Array (ref []) === eval_expr empty (arr[]);
  Array (ref [Int 1L]) === eval_expr empty (arr[one]);
  Array (ref [Int 1L;Int 2L]) === eval_expr empty (arr[one;two]);
  Array (ref [Int 2L;Int 1L]) === eval_expr empty (arr[two;one]);

  Int 1L === eval_expr empty (three -   two);
  Int 6L === eval_expr empty (three *   two);
  Int 0L === eval_expr empty (three *>> two);
  Int 1L === eval_expr empty (three /   two);
  Int 1L === eval_expr empty (three %   two);
  Int 5L === eval_expr empty (three +   two);
  Int 0L === eval_expr empty (three <   two);
  Int 0L === eval_expr empty (three <=  two);
  Int 1L === eval_expr empty (three >=  two);
  Int 1L === eval_expr empty (three >   two);
  Int 0L === eval_expr empty (three ==  two);
  Int 1L === eval_expr empty (three !=  two);
  Int 0L === eval_expr empty (tru   &   fls);
  Int 1L === eval_expr empty (tru   ||  fls);
  Int 0L === eval_expr empty (tru   ==  fls);
  Int 1L === eval_expr empty (tru   !=  fls);

  Int 1L === eval_expr empty (!fls);
  Int 0L === eval_expr empty (!tru);
  Int (-1L) === eval_expr empty (~~one);
  Int (-2L) === eval_expr empty (~~two);

  Array (ref []) === eval_expr empty (arr[] + arr[]);
  Array (ref [Int 1L]) === eval_expr empty (arr[one] + arr[]);
  Array (ref [Int 1L]) === eval_expr empty (arr[] + arr[one]);
  Array (ref [Int 1L;Int 2L]) === eval_expr empty (arr[one] + arr[two]);
  Array (ref [Int 1L;Int 2L;Int 3L]) === eval_expr empty (arr[one;two] + arr[three]);
  Array (ref [Int 1L;Int 2L;Int 3L]) === eval_expr empty (arr[one] + arr[two;three]);

  Int 0L === eval_expr empty (index (arr[zero;one;two;three;four;five]) zero);
  Int 1L === eval_expr empty (index (arr[zero;one;two;three;four;five]) one);
  Int 2L === eval_expr empty (index (arr[zero;one;two;three;four;five]) two);
  Int 3L === eval_expr empty (index (arr[zero;one;two;three;four;five]) three);
  Int 4L === eval_expr empty (index (arr[zero;one;two;three;four;five]) four);
  Int 5L === eval_expr empty (index (arr[zero;one;two;three;four;five]) five);
  Int 1L === eval_expr empty (index (index (arr[arr[one]]) zero) zero);
  Int 1L === eval_expr empty (index (index (index (arr[arr[arr[one]]]) zero) zero) zero);

  Int 0L === eval_expr empty (length (arr[]));
  Int 1L === eval_expr empty (length (arr[one]));
  Int 2L === eval_expr empty (length (arr[one;one]));
  Int 3L === eval_expr empty (length (arr[one;one;two]));

  let c = vals ["x",Int 1L; "y",Array (ref [Int 1L; Int 2L])] in
  Int 1L === eval_expr c (id "x");
  Array (ref [Int 1L; Int 2L]) === eval_expr c (id "y");

  let c = funcs [
    "f1", [], return [one];
    "f2", [Some "x"], return [one];
    "f3", [None], return [one];
    "f4", [Some "x"], return [id "x"];
    "f5", [Some "x"; Some "y"], return [id "x" + id "y"];
    "f6", [Some "x"; Some "y"], return [id "x" + id "y"; id "x"; zero];
  ] in
  Int 1L === eval_expr c (funccall "f1" []);
  Int 1L === eval_expr c (funccall "f2" [one]);
  Int 1L === eval_expr c (funccall "f3" [one]);
  Int 2L === eval_expr c (funccall "f4" [two]);
  Int 5L === eval_expr c (funccall "f5" [two;three]);
  Tuple [Int 5L;Int 2L;Int 0L] === eval_expr c (funccall "f6" [two;three]);
  Array (ref [Int 49L]) === eval_expr c (funccall "unparseInt" [one]);
  Int 1L === eval_expr c (funccall "parseInt" [arr[int 49L]]);

  ()

let test_eval_stmt () =
  let open ConfigEq in
  let open Typecheck.Abbreviations in

  (* decl *)
  (gam["x",None], None) ===
    eval_stmt empty (decl [avar (aid "x" tint)]);
  (gam["x",None;"y",None], None) ===
    eval_stmt empty (decl [avar (aid "x" tint); avar (aid "y" tint)]);

  (* declasgn *)
  let merge c1 c2 =
    String.Map.(of_alist_exn (to_alist c1 @ to_alist c2))
  in
  let fs = funcs [
    "f1", [], return [one];
    "f2", [Some "x"], return [one];
    "f3", [None], return [one];
    "f4", [Some "x"], return [id "x"];
    "f5", [Some "x"; Some "y"], return [id "x" + id "y"];
    "f6", [Some "x"; Some "y"], return [id "x" + id "y"; id "x"; zero];
    "f7", [], return [one; two];
  ] in
  let c1 = merge fs (vals ["x",Int 1L;]) in
  let c2 = merge fs (vals ["y",Int 2L;]) in
  let c3 = merge fs (vals ["x",Int 1L; "y",Int 2L]) in
  (c1, None) === eval_stmt fs (declasgn [avar (aid "x" tint)] one);
  (c2, None) === eval_stmt fs (declasgn [avar (aid "y" tint)] two);
  (c1, None) === eval_stmt fs
    (declasgn [avar (aid "x" tint); underscore] (funccall "f7" []));
  (c2, None) === eval_stmt fs
    (declasgn [underscore; avar (aid "y" tint)] (funccall "f7" []));
  (c3, None) === eval_stmt fs
    (declasgn [avar (aid "x" tint); avar (aid "y" tint)] (funccall "f7" []));

  (* return *)
  (empty, None) === eval_stmt empty (return []);
  (empty, Some (Int 1L)) === eval_stmt empty (return [one]);
  (empty, Some (Int 2L)) === eval_stmt empty (return [two]);
  (empty, Some (Tuple [Int 1L;Int 2L])) === eval_stmt empty (return [one;two]);
  (empty, Some (Tuple [Int 2L;Int 1L])) === eval_stmt empty (return [two;one]);
  (empty, Some (Tuple [Int 1L;Int 2L;Int 3L])) ===
    eval_stmt empty (return [one;two;three]);

  (* asgn *)
  let c  = vals ["x",Int 1L; "y",Array (ref [Int 1L; Int 2L])] in
  let c1 = vals ["x",Int 2L; "y",Array (ref [Int 1L; Int 2L])] in
  let c2 = vals ["x",Int 3L; "y",Array (ref [Int 1L; Int 2L])] in
  let c3 = vals ["x",Int 1L; "y",Array (ref [Int 2L; Int 1L])] in
  (c,  None) === eval_stmt c (asgn (id "x") one);
  (c1, None) === eval_stmt c (asgn (id "x") two);
  (c2, None) === eval_stmt c (asgn (id "x") three);
  (c3, None) === eval_stmt c (asgn (id "y") (arr[two;one]));

  (* if *)
  (empty, Some (Int 1L)) === eval_stmt empty (if_ tru (return [one]));
  (empty, None) === eval_stmt empty (if_ fls (return [one]));

  (* if else *)
  (empty, Some (Int 1L)) === eval_stmt empty (ifelse tru (return [one]) (return [two]));
  (empty, Some (Int 2L)) === eval_stmt empty (ifelse fls (return [one]) (return [two]));

  (* while *)
  (empty, Some (Int 1L)) === eval_stmt empty (while_ tru (block [return [one]]));
  (empty, None) === eval_stmt empty (while_ fls (block [return [one]]));

  (* proc calls *)
  let c = funcs [
    "f1", [], block [];
    "f2", [Some "x"], block [];
    "f3", [None], block [];
    "f4", [Some "x"], block [];
    "f5", [Some "x"; Some "y"], block [];
    "f6", [None; Some "y"], block [];
    "f7", [Some "x"; None], block [];
    "f8", [None; None], block [];
  ] in
  (c, None) === eval_stmt c (proccall "f1" []);
  (c, None) === eval_stmt c (proccall "f2" [one]);
  (c, None) === eval_stmt c (proccall "f3" [one]);
  (c, None) === eval_stmt c (proccall "f4" [two]);
  (c, None) === eval_stmt c (proccall "f5" [two;three]);
  (c, None) === eval_stmt c (proccall "f6" [two;three]);
  (c, None) === eval_stmt c (proccall "f7" [two;three]);
  (c, None) === eval_stmt c (proccall "f8" [two;three]);

  (* complicated *)
  let x, y, z, _ = id "x", id "y", id "z", underscore in
  let xint = avar (aid "x" tint) in
  let yint = avar (aid "y" tint) in
  let zint = avar (aid "z" tint) in
  let uint = avar (aunderscore tint) in

  let uno = Some (Int 1L) in
  let dos = Some (Int 2L) in
  let both = Some (Tuple [Int 1L; Int 2L]) in

  (empty, uno) === eval_stmt empty (block [
    decl [xint];
    x <-- one;
    return [x];
  ]);
  (empty, dos) === eval_stmt empty (block [
    decl [yint];
    y <-- two;
    return [y];
  ]);
  (empty, uno) === eval_stmt empty (block [
    decl [xint; uint];
    x <-- one;
    return [x];
  ]);
  (empty, uno) === eval_stmt empty (block [
    decl [uint; xint; uint];
    x <-- one;
    decl [uint];
    return [x];
  ]);
  (empty, uno) === eval_stmt empty (block [
    decl [xint];
    x <-- one;
    x <-- two;
    x <-- one;
    return [x]
  ]);
  (empty, uno) === eval_stmt empty (block [
    decl [xint];
    x <-- one;
    x <-- two;
    x <-- one;
    return [x];
  ]);
  (empty, both) === eval_stmt empty (block [
    [xint] := two;
    [yint] := one;
    if_ tru (block [
      x <-- one;
      y <-- two;
      [zint] := three;
    ]);
    [zint] := three;
    return [x; y]
  ]);
  (empty, both) === eval_stmt empty (block [
    [xint] := two;
    [yint] := one;
    ifelse (x == y) (block [
      return [z];
    ]) (block [
      x <-- one;
      y <-- two;
    ]);
    return [x; y]
  ]);
  (empty, both) === eval_stmt empty (block [
    [xint] := five;
    [yint] := two;
    while_ (x > one) (block [
      x <-- x - one;
      y <-- y + one;
    ]);
    y <-- two;
    return [x; y]
  ]);

  let f = funcs["one_first",[Some "x"],asgn (index x zero) one] in
  let xintarr = avar (aid "x" (tarray tint None)) in
  let yintarr = avar (aid "y" (tarray tint None)) in
  (empty, uno) === eval_stmt empty (block [
    [xintarr] := arr[zero];
    index x zero <-- one;
    return [index x zero]
  ]);
  (f, uno) === eval_stmt f (block [
    [xintarr] := arr[zero];
    proccall "one_first" [x];
    return [index x zero]
  ]);
  (empty, Some (Int 0L)) === eval_stmt empty (block [
    [xintarr] := arr[one;two;three];
    [yintarr] := arr[one;two;three];
    return [x == y]
  ]);
  (empty, Some (Int 1L)) === eval_stmt empty (block [
    [xintarr] := arr[one;two;three];
    [yintarr] := x;
    return [x == y]
  ]);

  ()

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* ! DON'T FORGET TO ADD YOUR TESTS HERE                                     ! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let main () =
    "suite" >::: [
      "test_eval_expr" >:: test_eval_expr;
      "test_eval_stmt" >:: test_eval_stmt;
    ] |> run_test_tt_main

let _ = main ()
