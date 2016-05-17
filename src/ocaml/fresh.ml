open Core.Std

module type Name = sig
  val name: string
end

module type S = sig
  val fresh   : unit   -> string
  val reset   : unit   -> unit
  val gen     : int    -> string
  val get     : string -> int option
  val gen_str : string -> string
  val get_str : string -> string option
end

module Make(N: Name) = struct
  let n = ref 0

  let gen n =
    N.name ^ (string_of_int n)

  let get s =
    if String.is_prefix s ~prefix:N.name then
      Util.int_of_string (String.drop_prefix s (String.length N.name))
    else None

  let gen_str s =
    N.name ^ s

  let get_str s =
    if String.is_prefix s ~prefix:N.name then
      Some (String.drop_prefix s (String.length N.name))
    else
      None

  let fresh () = gen (Util.get_and_incr n)
  let reset () = n := 0
end

(* Tiling fresh modules *)
module FreshReg    = Make(struct let name = "_asmreg" end)
module FreshAsmRet = Make(struct let name = "_asmret" end)
module FreshLabel  = Make(struct let name = "_asmlabel" end)
