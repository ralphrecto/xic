open Core.Std

(* double_underscore "foo"      = "foo"
 * double_underscore "foo_bar"  = "foo__bar"
 * double_underscore "foo__bar" = "foo____bar" *)
val double_underscore : string -> string

(* let f = abi_type_name in
 *
 * f _     IntT       = "i"
 * f _     BoolT      = "b"
 * f true  UnitT      = ""
 * f false UnitT      = "p"
 * f _     (ArrayT t) = "a" ^ (f _ t)
 * f _     (KlassT c) = o ^ len(c) ^ c
 * f _     EmptyArray = undefined
 * f _     NullT      = undefined
 * f _     (TupleT [t1; ...; tn]) = "n" ^ f _ t1 ^ ... ^ f _ tn *)
val abi_type_name : bool -> Typecheck.Expr.t -> string

val abi_callable_decl_name  : Typecheck.callable_decl      -> string
val abi_callable_name       : Typecheck.callable           -> string

(* id name -> ABI compliant name *)
val abi_callable_decl_names : Typecheck.callable_decl list -> string String.Map.t
val abi_callable_names      : Typecheck.callable list      -> string String.Map.t

(* ABI compliant name -> id name *)
val mangled_to_name         : Typecheck.callable list      -> string String.Map.t
