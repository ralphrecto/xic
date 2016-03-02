(* init [] = []
 * init [a] = []
 * init [a, b] = [a]
 * init [a, b, ..., y, z] = [a, b, ..., y] *)
val init: 'a list -> 'a list
