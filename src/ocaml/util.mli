(* init [] = []
 * init [a] = []
 * init [a, b] = [a]
 * init [a, b, ..., y, z] = [a, b, ..., y] *)
val init: 'a list -> 'a list

(* String.concat ~sep:"\n" *)
val join: string list -> string

(* String.concat ~sep:"," *)
val commas: string list -> string
