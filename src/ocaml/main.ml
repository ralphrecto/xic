module StdString = String
open Core.Std
open Async.Std
open Typecheck
open Ast_optimization
open Ir
open Ir_util
open Ir_generation
open Ir_printer
open Printf
open Xi_interpreter

module TodoRemoveThis_ItsOnlyUsedToBuildAsm = Asm
module TodoRemoveThis_ItsOnlyUsedToBuildTiling = Tiling
module TodoRemoveThis_ItsOnlyUsedToBuildFresh = Fresh

type flags = {
  typecheck:  bool;
  tcdebug:    bool;
  irgen:      bool;
  ast_cfold:  bool;
  nothing:    bool;
  lower:      bool;
  ir_cfold:   bool;
  blkreorder: bool;
  outputs:    string list;
} [@@deriving sexp]

let resmap ~f =
  List.map ~f:(function
      | Ok x -> Ok (f x)
      | Error e-> Error e)

let do_if (b: bool) (f: 'a -> 'a) (x: 'a) : 'a =
  if b then f x else x

let format_err_msg ((row, col), msg) =
  let row_s = string_of_int row in
  let col_s = string_of_int col in
  "ERROR:::" ^ row_s ^ ":::" ^ col_s ^ ":::" ^ msg

let write (f: 'a -> string) (out: string) (tc: 'a Error.result) =
  let contents =
    match tc with
    | Ok el -> f el 
    | Error e -> format_error_msg e in
  Writer.save out ~contents

let get_callable_decls (FullProg (_, (_, interfaces))) = 
 List.fold_left
   ~f:(fun acc (_, Interface clist) -> clist @ acc)
   ~init:[] interfaces

let ( $ ) = Fn.compose

(* actual compiler options *)
let typecheck (ast: string) : Typecheck.full_prog Error.result = 
  ast
    |> StdString.trim
    |> Sexp.of_string
    |> Pos.full_prof_of_sexp
    |> prog_typecheck

let ir_gen (ast: string) : Ir.comp_unit Error.result = 
  let f = 
    let g1 = ir_gen $ ast_constant_folding in
    let g2 = ir_constant_folding $ g1 in
    let g3 = lower_comp_unit $ g2 in
    block_reorder_comp_unit $ g3 in
  Result.map (typecheck ast) ~f

let ir_gen_no_opt (ast: string) : Ir.comp_unit Error.result = 
  let f = block_reorder_comp_unit $ (lower_comp_unit $ gen_comp_unit) in
  Result.map (typecheck ast) ~f

(* debugging paths *)
let debug_ast_cfold (ast: string) : Typecheck.full_prog Error.result =
  Result.map (typecheck ast) ~f:ast_constant_folding

let debug_ir_gen (ast: string) : Ir.comp_unit Error.result =
  Result.map (ast_cfold ast) ~f:gen_comp_unit

let debug_ir_cfold (ast: string): Ir.comp_unit Error.result =
  Result.map (ir_gen ast) ~f:ir_constant_folding

let debug_ir_lower (ast: string): Ir.comp_unit Error.result =
  Result.map (ir_cfold ast) ~f:lower_comp_unit

let debug_ir_blkreorder (ast: string): Ir.comp_unit Error.result =
  Result.map (ir_lower ast) ~f:block_reorder_comp_unit

let main flags asts () : unit Deferred.t =
  let help (to_str: 'a -> string) (astf: string -> 'a Error.result) = 
    List.zip_exn outs (List.map ~f:astf asts) |>
    Deferred.List.iter ~f:(fun (out, elt) -> write to_str out elt) in

  if flags.typecheck then
    help (fun _ -> "Valid Xi Program") typecheck
  else if flags.tcdebug then
    let to_str (FullProg (prog, _): Typecheck.full_prog) : string = 
      prog |> Typecheck.sexp_of_prog |> Sexp.to_string in
    help to_str typecheck
  else if flags.ir_gen && flags.no_opt then
    help sexp_of_comp_unit ir_gen_no_opt
  else if flags.ir_gen then
    help sexp_of_comp_unit ir_gen
  else if flags.debug-ast-cfold then
    help sexp_of_comp_unit debug_ast_cfold
  else if flags.debug-ir-gen then
    help sexp_of_comp_unit debug_ast_cfold
  else if flags.debug-ir-lower then
    help sexp_of_comp_unit debug_ir_lower
  else if flags.debug-ir-blkreorder then
    help sexp_of_comp_unit debug_ir_blkreorder
  else return ()

let () =
  Command.async
    ~summary:"Xi Compiler"
    Command.Spec.(
      empty
      +> flag "--typecheck"  no_arg ~doc:""
      +> flag "--tcdebug"    no_arg ~doc:""
      +> flag "--irgen"      no_arg ~doc:""
      +> flag "--ast-cfold"  no_arg ~doc:""
      +> flag "--nothing"    no_arg ~doc:""
      +> flag "--lower"      no_arg ~doc:""
      +> flag "--ir-cfold"   no_arg ~doc:""
      +> flag "--blkreorder" no_arg ~doc:""
      +> flag "--outputs"    (listed string) ~doc:""
      +> anon (sequence ("asts" %: string))
    )
    (fun tc tcd irg afold nothing ifold l b os asts ->
       let flags = {
         typecheck  = tc;
         tcdebug    = tcd;
         irgen      = irg;
         ast_cfold  = afold;
         nothing    = nothing;
         lower      = l;
         ir_cfold   = ifold;
         blkreorder = b;
         outputs    = os;
       } in
       main flags asts)
  |> Command.run
