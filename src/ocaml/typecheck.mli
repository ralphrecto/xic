open Core.Std
open Async.Std

(******************************************************************************)
(* Error                                                                      *)
(******************************************************************************)
module Error: sig
  type t = Pos.pos * string
  type 'a result = ('a, t) Result.t
end

(******************************************************************************)
(* Ast                                                                        *)
(******************************************************************************)
module Expr: sig
  type t =
    | IntT
    | BoolT
    | UnitT
    | ArrayT of t
    | TupleT of t list (* len >= 2 *)
    | EmptyArray
    | NullT
    | KlassT of string
    [@@deriving sexp, compare]

  type subtyping = t -> t -> bool

  val to_string: t -> string
  val of_typ: Pos.typ -> t

  val array_subtyping : subtyping
  val subtype : subtyping

  (* takes a map from class name -> superclass name *)
  val make_subtype_rel : string String.Map.t -> subtyping

  val comparable : subtyping -> t -> t -> bool

  val type_max : subtyping -> Pos.pos -> t -> t -> t Error.result

  (* `eqs p xs ys num type` checks that
   *
   *     (1) len(xs) == len(ys), and
   *     (2) for all xi and yi, xi >= yi.
   *
   * If (1) fails, `Error (p, num)` is returned. If (2) fails, `Error (p,
   * type)` is returned. *)
  val eqs: subtyping -> Pos.pos -> t list -> t list -> string -> string -> unit Error.result
end

module Stmt: sig
  type t =
    | One  (* aka unit *)
    | Zero (* aka void *)
    [@@deriving sexp]

  (* least upper bound *)
  val lub: t -> t -> t
end

module T: sig
  type p = unit             [@@deriving sexp]
  type u = unit             [@@deriving sexp]
  type g = unit             [@@deriving sexp] (*TODO*)
  type k = unit             [@@deriving sexp] (*TODO*)
  type c = Expr.t * Expr.t  [@@deriving sexp]
  type i = unit             [@@deriving sexp]
  type a = Expr.t           [@@deriving sexp]
  type v = Expr.t           [@@deriving sexp]
  type s = Stmt.t           [@@deriving sexp]
  type e = Expr.t           [@@deriving sexp]
  type t = Expr.t           [@@deriving sexp]
end
include (module type of Ast.Make(T))

module D: sig
  include (module type of T)
  val dummy_p: p
  val dummy_u: u
  val dummy_g: g
  val dummy_k: k
  val dummy_c: c
  val dummy_i: i
  val dummy_a: a
  val dummy_v: v
  val dummy_s: s
  val dummy_e: e
  val dummy_t: t
end

module Abbreviations: (module type of Ast.Abbreviate(D))

val varsofavar : raw_avar -> string option
val varsofvar  : raw_var  -> string option
val varsofvars : var list -> string list

val is_underscore  : raw_var  -> bool
val has_underscore : var list -> bool

val typeofavar    : Pos.raw_avar -> Expr.t
val postypeofavar : Pos.raw_avar -> Pos.typ
val typesofvars   : Pos.var list -> Pos.typ list

(******************************************************************************)
(* Contexts                                                                   *)
(******************************************************************************)
module Sigma: sig
  type t =
    | Var      of Expr.t
    | Function of Expr.t * Expr.t
    [@@deriving sexp]
end

module KlassM: sig
  (*
   * class B {
   *     x: int
   *     y: int[]
   *     foo(z: int) : bool
   *     bar()
   * }
   *
   * class A extends B {
   *     b: B
   *     foo(z: int) : bool
   *     baz(c: bool[])
   * }
   *
   * {
   *     name: "B";
   *     super: None;
   *     fields: [(x, int); (y, int)];
   *     methods: [foo(z: int) : bool; bar()];
   *     overrides: [];
   * }
   *
   * {
   *     name: "A";
   *     super: Some "B";
   *     fields: [(b, B)];
   *     methods: [baz(c: bool[])];
   *     overrides: [foo(z: int) : bool];
   * }
   *)
  type t = {
    name      : string;
    super     : string option;
    fields    : (string * Pos.typ) list;
    methods   : Pos.callable_decl list;
    overrides : Pos.callable_decl list;
  } [@@deriving sexp]
end

type context = Sigma.t String.Map.t

module Context: sig
  include (module type of String.Map)

  (* `var p c x` tries to find a binding for `x` of the form `Var e`. If no
   * such binding is found, an error at position `p` is returned instead. *)
  val var:  Pos.pos -> context -> string -> Expr.t Error.result

  (* `func p c x` tries to find a binding for `x` of the form `Function (a,
   * b)`. If no such binding is found, an error at position `p` is returned
   * instead. *)
  val func: Pos.pos -> context -> string -> (Expr.t * Expr.t) Error.result

  (* Abbreviation for Context.add c ~key:k ~data:d. *)
  val bind: context -> string -> Sigma.t -> context

  (* For every annotated variable `x:t` in `vs`, `bind_all c vs` binds `x` to
   * `t`. underscores and annotated underscores are ignored. *)
  val bind_all_vars: context -> var list -> context

  (* Same as bind_all_vars, but errors out if there are any underscores or
   * annotated underscores. *)
  val bind_all_pos_vars: context -> Pos.var list -> context

  val bind_all_avars: context -> avar list -> context
end

type contexts = {
  locals        : context;               (* Γ *)
  delta_m       : KlassM.t String.Map.t; (* Δ_m *)
  delta_i       : KlassM.t String.Map.t; (* Δ_i *)
  class_context : string option;         (* γ *)
  inloop        : bool;                  (* λ *)

  (* Set of all unmangled global identifiers. *)
  globals : String.Set.t;

  (* List of globals in the order in which they should be initialized. *)
  typed_globals : global list;

  (* Subtype relation respecting delta_m and delta_i. *)
  subtype : Expr.t -> Expr.t -> bool;
}

val empty_contexts : contexts

type typecheck_info = {
  prog  : full_prog;
  ctxts : contexts;
}

(* `methods m i c` returns a list of the methods in c's class heirarchy in the
 * order in which they are declared. For example, consider this heirarchy:
 *
 *     class A { a() b() }
 *     class B { c() a() d() }
 *     class C { }
 *     class D { a() e() }
 *
 * methods m i "D" is [a, b, c, d, e] *)
val methods: delta_m:KlassM.t String.Map.t ->
             delta_i:KlassM.t String.Map.t ->
             string ->
             string list

(******************************************************************************)
(* Helpers                                                                    *)
(******************************************************************************)
(**
 * `class_graph ks` constructs the class hierarchy for the classes in ks. More
 * precisely, it constructs a graph with one vertex for each k in ks and an
 * edge from vertex u to vertex v if v extends u. If ks are well-typed, then
 * this graph will also be a tree. It is an invariant that no two classes in ks
 * have the same name!
 *
 * For example, the classes in this code:
 *
 *     class A {}
 *     class B extends A {}
 *     class C extends A {}
 *     class D extends B {}
 *     class E {}
 *
 * will generate the following tree with arrows going downwards:
 *
 *       A     E
 *      / \
 *     B   C
 *     |
 *     D
 *
 * Whereas the classes in this code:
 *
 *     class A extends B {}
 *     class B extends C {}
 *     class C extends D {}
 *     class D extends A {}
 *
 * will generate this graph:
 *
 *     A <--- B
 *     |      ▲
 *     ▼      |
 *     D ---> C
 *
 * The class graph is useful for a couple of things. For example, it can be
 * checked for cycles or it can be topologically sorted.
 *)
module KlassVertex : sig
  type t = Pos.klass
  val compare : t -> t -> int
  val hash    : t -> int
  val equal   : t -> t -> bool
end
module KlassGraph : module type of Graph.Persistent.Digraph.Concrete(KlassVertex)
val class_graph : Pos.klass list -> KlassGraph.t

(**
 * `global_graph gs` constructs a global dependency graph. More precisely, it
 * constructs a graph with one vertex for each g in gs and an edge from vertex
 * u to vertex v if v depends on u. If gs are well-typed, then this graph will
 * also be acyclic. It is an invariant that no two globals in gs have the same
 * name!
 *
 * For example, the globals in this code:
 *
 *     A: int
 *     B: int = A
 *     C: int = A
 *     D: int = B
 *     E: int
 *
 * will generate the following tree with arrows going downwards:
 *
 *       A     E
 *      / \
 *     B   C
 *     |
 *     D
 *
 * Whereas the globals in this code:
 *
 *     A: int = B
 *     B: int = C
 *     C: int = D
 *     D: int = A
 *
 * will generate this graph:
 *
 *     A <--- B
 *     |      ▲
 *     ▼      |
 *     D ---> C
 *)
module GlobalVertex : sig
  type t = Pos.global
  val compare : t -> t -> int
  val hash    : t -> int
  val equal   : t -> t -> bool
end
module GlobalGraph : module type of Graph.Persistent.Digraph.Concrete(GlobalVertex)
val global_graph : Pos.global list -> GlobalGraph.t

(******************************************************************************)
(* typechecking                                                               *)
(******************************************************************************)
val expr_typecheck: contexts -> Pos.expr -> expr Error.result
val typ_typecheck: contexts -> Pos.typ -> typ Error.result
val avar_typecheck: contexts -> Pos.avar -> avar Error.result
val var_typecheck: contexts -> Pos.var -> var Error.result
val stmt_typecheck: contexts -> Expr.t -> Pos.stmt -> stmt Error.result
val global_typecheck: contexts -> Pos.global -> global Error.result
val func_decl_typecheck: contexts -> Pos.callable_decl -> contexts Error.result
val func_typecheck: contexts -> Pos.callable -> contexts Error.result
val callable_decl_typecheck : Pos.callable_decl -> callable_decl Error.result
val interface_typecheck : Pos.interface -> interface Error.result
val fst_func_pass: Pos.callable list -> Pos.interface list -> contexts Error.result
val snd_func_pass: contexts -> Pos.callable -> callable Error.result

(* TODO: document the order in which passes are done and what each pass does. *)

(**
 * The global pass checks that the following conditions hold:
 *   (1) There are no underscores
 *   (2) There are no duplicate global names
 *   (3) No global is named "this"
 *   (4) If a global is uninit or initialized to constants or to other globals
 *   (5) The global dependency graph is acyclic
 *   (6) All the types mentioned by the globals are ok
 * If all these conditions are met, a new contexts is returned in which
 *   (a) gamma is populated with the globals
 *   (b) globals is populated with the set of all global identifiers
 *   (c) typed_globals is populated with a list of typechecked globals in the
 *       order they should be initialized. Moreover, each decl and declassgn is
 *       flattened into a singleton list.
 *)
val global_pass : contexts -> Pos.global list -> contexts Error.result
val fst_klass_pass : contexts -> Pos.klass list -> contexts Error.result
val prog_typecheck: Pos.full_prog -> typecheck_info Error.result
