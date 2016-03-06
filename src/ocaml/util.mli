(* init [] = []
 * init [a] = []
 * init [a, b] = [a]
 * init [a, b, ..., y, z] = [a, b, ..., y] *)
val init: 'a list -> 'a list

(* commas [] = ""
 * commas ["a"] = "a"
 * commas ["a", "b"] = "a","b" *)
val commas: string list -> string
