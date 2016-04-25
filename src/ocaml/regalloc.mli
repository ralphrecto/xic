open Asm

(* performs register allocation with move coalescing *)
val reg_alloc : abstract_asm list -> asm list
