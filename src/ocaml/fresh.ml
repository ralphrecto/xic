open Core.Std

module type Name = sig
  val name: string
end

module type S = sig
  val gen   : int  -> string
  val fresh : unit -> string
  val reset : unit -> unit
end

module Make(N: Name) = struct
  let n = ref 0
  let gen n = N.name ^ (string_of_int n)
  let fresh () = gen (Util.get_and_incr n)
  let reset () = n := 0
end
