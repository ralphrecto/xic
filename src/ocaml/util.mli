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
