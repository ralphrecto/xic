open Core.Std
open Async.Std

(* init [] = []
 * init [a] = []
 * init [a, b] = [a]
 * init [a, b, ..., y, z] = [a, b, ..., y] *)
val init: 'a list -> 'a list

(* String.concat ~sep:"\n" *)
val join: string list -> string

(* String.concat ~sep:"," *)
val commas: string list -> string

(* pairs [] = []
 * pairs [1] = []
 * pairs [1;2] = [(1,2)]
 * pairs [1;2;3] = [(1,2),(2,3)] *)
val pairs: 'a list -> ('a * 'a) list

(* all_eq xs ys <==> (a in xs \iff a \in ys) *)
val all_eq: 'a list -> 'a list -> bool

(* Increment and return the old value of an int ref. *)
val get_and_incr: int ref -> int

(* int_of_string "1" = Some 1
 * int_of_string "2" = Some 2
 * int_of_string "foo" = None *)
val int_of_string: string -> int option

(* ordered_dedup [1;2;3] = [1;2;3]
 * ordered_dedup [1;2;1;1;3;2;2;3;3] = [1;2;3] *)
val ordered_dedup: 'a list -> 'a list

(* time thunk returns the time taken to evaluate thunk *)
val time: (unit -> 'a) -> (Time.Span.t * 'a)
val time_def: 'a Deferred.t -> (Time.Span.t * 'a) Deferred.t

(* Pretty printing. When the short argument is true, data structures are
 * printed more concisely. short defaults false. *)
val string_of_list : ?short:bool -> 'a list -> f:('a -> string) -> string
val string_of_set  : ?short:bool -> ('a, _) Set.t -> f:('a -> string) -> string
val string_of_map  : ?short:bool -> ('k, 'v, _) Map.t ->
                     k:('k -> string) -> v:('v -> string) -> string

(* Combines two maps with disjoint keys. *)
val disjoint_merge : ('k, 'v, 'cmp) Map.t ->
                     ('k, 'v, 'cmp) Map.t ->
                     ('k, 'v, 'cmp) Map.t

(* index ~eq xs x returns the 0-based index of x in xs using eq. *)
val index : ?eq:('a -> 'a -> bool) -> 'a list -> 'a -> int option
