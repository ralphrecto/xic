open Core.Std

(* In many parts of our compiler, we need to generate fresh names. For example,
 * when we generate IR from an AST or when we perform block reordering, we need
 * to create fresh labels. Similarly, when we generate assembly, we need fresh
 * register names. This Fresh module helps automate the generation of fresh
 * names.
 *
 * Given a module N of of type Name, the module Fresh.Make(N) will generate
 * fresh names N.name0, N.name1, N.name2, N.name3, etc. More concretely, the
 * following code
 *
 *     module RegName = struct
 *       let name = "asm_reg"
 *     end
 *
 *     module FreshReg = Fresh.Make(RegName)
 *
 *     print_endline (FreshReg.fresh ());
 *     print_endline (FreshReg.fresh ());
 *     print_endline (FreshReg.fresh ());
 *     print_endline (FreshReg.fresh ())
 *
 * will print asm_reg0, asm_reg1, asm_reg2, and asm_reg3.
 *
 * Names can be generated explicitly using the gen function. For example,
 * `FreshReg.gen 42` returns `asm_reg42`. Moreover, the number suffix of fresh
 * names can be reset with reset.
 *
 *     print_endline (FreshReg.fresh ()); (* prints asm_reg0 *)
 *     print_endline (FreshReg.fresh ()); (* prints asm_reg1 *)
 *     FreshReg.reset ();
 *     print_endline (FreshReg.fresh ())  (* prints asm_reg0 *)
 *
 * The integer suffix of a fresh name can also be parsed with get.
 *
 *     FreshReg.get "asm_reg0"    = Some 0
 *     FreshReg.get "asm_reg1"    = Some 1
 *     FreshReg.get "foo1"        = None
 *     FreshReg.get "asm_regoops" = None
 *)
module type Name = sig
  val name: string
end

module type S = sig
  val fresh         : unit   -> string
  val reset         : unit   -> unit
  val gen           : int    -> string
  val get           : string -> int option
  val gen_str       : string -> string
  val get_str       : string -> string option
  val gen_fresh_str : string -> string
  val get_fresh_str : string -> string option
end

module Make(N: Name): S

(* Tiling fresh modules *)
module FreshReg    : S
module FreshAsmRet : S
module FreshLabel  : S
