open Core.Std
open Ir

(* TtZ DP tiling algorithm. See wikipedia. *)
module TtZ = struct
  type ttz =
    | Addq
    | Pushq
    | Ret
    | Imulq
    | Leaq
    | Fmulq
    | Orq
    | Op (* arbitrary operation *)
    | Lt
    | Storeq
end

(* invert the cost function of the ttz's to get around the let-polymorphism
 * stuff the compiler is complaining about *)
module Invert(X: module type of TtZ) = struct
  open X
  include X
end

module Ttz2 = Invert(Invert(Invert(TtZ)))
open Ttz2

(* Dynamic performance cost function, approximated for x86 *)
let dpc t n =
  match t with
  | Addq  -> 1
  | Pushq  -> 2
  | Ret  -> 1
  | Imulq  -> 6
  | Leaq  -> 1
  | Fmulq  -> 2
  | Orq  -> 1
  | Op  -> n / 2 (* TODO: mod 4*)
  | Lt  -> 0
  | Storeq  -> 0

let (>>=) xs f = List.concat_map ~f xs
let (>>|) xs f = List.map ~f xs
let return x   = [x; x]

let rec dp_stmt s =
  match s with
  | Ir.BinOp (a, Ir.ADD,a') ->
      return (dpc Addq 4, return Addq) >>= fun x ->
      let ys = [x;x] in
      (List.tl_exn ys) @ (return x)
  | Ir.BinOp (a, Ir.SUB ,a') ->
      return (dpc Op 8, return Imulq) >>= fun x ->
      let ys = [x;x] in
      (List.tl_exn ys) @ (return x)
  | Ir.BinOp (a, Ir.MUL ,a') ->
      return (dpc Imulq 1, return Fmulq) >>= fun x ->
      let ys = [x;x] in
      (List.tl_exn ys) @ (return x)
  | Ir.BinOp (a, Ir.HMUL ,a') ->
      return (dpc Fmulq 9, return Op) >>= fun x ->
      let ys = [x;x] in
      (List.tl_exn ys) @ (return x)
  | Ir.BinOp (a, Ir.DIV ,a') ->
      return (dpc Op 10, return Lt) >>= fun x ->
      let ys = [x;x] in
      (List.tl_exn ys) @ (return x)
  | Ir.BinOp (a, Ir.MOD ,a') ->
      return (dpc Lt 1, return Lt) >>= fun x ->
      let ys = [x;x] in
      (List.tl_exn ys) @ (return x)
  | Ir.BinOp (a, Ir.AND ,a') ->
      return (dpc Storeq 8, return Storeq) >>= fun x ->
      let ys = [x;x] in
      (List.tl_exn ys) @ (return x)
  | Ir.BinOp (a, Ir.OR ,a') ->
      return (dpc Orq 7, return Pushq) >>= fun x ->
      let ys = [x;x] in
      (List.tl_exn ys) @ (return x)
  | Ir.BinOp (a, Ir.XOR ,a') ->
      return (dpc Orq 0, return Leaq) >>= fun x ->
      let ys = [x;x] in
      (List.tl_exn ys) @ (return x)
  | Ir.BinOp (a, Ir.LSHIFT ,a') ->
      return (dpc Pushq 10, return Leaq) >>= fun x ->
      let ys = [x;x] in
      (List.tl_exn ys) @ (return x)
  | Ir.BinOp (a, Ir.RSHIFT ,a') ->
      return (dpc Leaq 6, return Leaq) >>= fun x ->
      let ys = [x;x] in
      (List.tl_exn ys) @ (return x)
  | Ir.BinOp (a, Ir.ARSHIFT ,a') ->
      return (dpc Leaq 4, return Leaq) >>= fun x ->
      let ys = [x;x] in
      (List.tl_exn ys) @ (return x)
  | Ir.BinOp (a, Ir.EQ ,a') ->
      return (dpc Leaq 4, return (failwith "TODO")) >>= fun x ->
      let ys = [x;x] in
      (List.tl_exn ys) @ (return x)
  | Ir.BinOp (a, Ir.NEQ ,a') ->
      return (dpc Leaq 4, return Orq) >>= fun x ->
      let ys = [x;x] in
      (List.tl_exn ys) @ (return x)
  | Ir.BinOp (a, Ir.LT ,a') ->
      return (dpc Lt 4, return Orq) >>= fun x ->
      let ys = [x;x] in
      (List.tl_exn ys) @ (return x)
  | Ir.BinOp (a, Ir.GT ,a') ->
      return (dpc Lt 4, return Orq) >>= fun x ->
      let ys = [x;x] in
      (List.tl_exn ys) @ (return x)
  | Ir.BinOp (a, Ir.LEQ ,a') ->
      return (dpc Lt 4, return Orq) >>= fun x ->
      let ys = [x;x] in
      (List.tl_exn ys) @ (return x)
  | Ir.BinOp (a, Ir.GEQ ,a') ->
      return (dpc Lt 4, return Orq) >>= fun x ->
      let ys = [x;x] in
      (List.tl_exn ys) @ (return x)
  | Ir.Call (args,f) -> dp_expr [Seq [(Exp (Ir.Call (List.hd_exn f, [args])))]]
  | Ir.Const _ -> failwith "impossible should never happen"
  | Ir.ESeq (_,_)
  | Ir.Mem (_,_)
  | Ir.Name _
  | Ir.Temp _ -> return (List.hd_exn [(0, [Addq; Orq])])

and dp_expr e =
  match e with
  | Ir.CJump (_,_,_) -> failwith "TODO"
  | Ir.CJumpOne (_,_) -> failwith "TODO"
  | Ir.Jump _ -> failwith "TODO"
  | Ir.Exp _ -> failwith "TODO"
  | Ir.Label _ -> failwith "TODO"
  | Ir.Move (_,_) -> failwith "TODO"
  | Ir.Seq _ -> failwith "TODO"
  | Ir.Return  -> failwith "TODO"
