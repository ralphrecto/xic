module StdString = String
open Core.Std
open Async.Std
open Typecheck
open Ast_optimization
open Ir
open Ir_util
open Ir_generation
open Ir_printer
open Asm
open Tiling
open Printf
open Xi_interpreter

module TodoRemoveThis_ItsOnlyUsedToBuildAsm = Asm
module TodoRemoveThis_ItsOnlyUsedToBuildTiling = Tiling
module TodoRemoveThis_ItsOnlyUsedToBuildFresh = Fresh

type flags = {
  typecheck:      bool;
  tcdebug:        bool;
  irgen:          bool;
  irgen_no_opt:   bool;
  ast_cfold:      bool;
  nothing:        bool;
  lower:          bool;
  ir_cfold:       bool;
  blkreorder:     bool;
  outputs:        string list;
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

let get_callable_decls ((FullProg (_, _, interfaces)): Pos.full_prog) = 
 List.fold_left
   ~f:(fun acc (_, Interface clist) -> clist @ acc)
   ~init:[] interfaces

let ( $ ) = Fn.compose

(* actual compiler options *)
let typecheck (ast: string) : Typecheck.full_prog Error.result = 
  ast
    |> StdString.trim
    |> Sexp.of_string
    |> Pos.full_prog_of_sexp
    |> prog_typecheck

let ir_gen (ast: string) : Ir.comp_unit Error.result = 
  let f = 
    let g1 = gen_comp_unit $ ast_constant_folding in
    let g2 = ir_constant_folding $ g1 in
    let g3 = lower_comp_unit $ g2 in
    block_reorder_comp_unit $ g3 in
  Result.map (typecheck ast) ~f

let ir_gen_no_opt (ast: string) : Ir.comp_unit Error.result = 
  let f = block_reorder_comp_unit $ (lower_comp_unit $ gen_comp_unit) in
  Result.map (typecheck ast) ~f

let asm_gen_opt (ast: string) : Asm.asm_prog Error.result =
  let f = 
    let g1 = gen_comp_unit $ ast_constant_folding in
    let g2 = ir_constant_folding $ g1 in
    let g3 = lower_comp_unit $ g2 in
    block_reorder_comp_unit $ g3 in
  Result.bind (typecheck ast) begin fun fullprog -> 
    let comp_unit = f fullprog in  
    Ok (asm_gen fullprog comp_unit)
  end

let asm_gen_no_opt (ast: string) : Asm.asm_prog Error.result =
  let f = block_reorder_comp_unit $ (lower_comp_unit $ gen_comp_unit) in
  Result.bind (typecheck ast) begin fun fullprog -> 
    let comp_unit = f fullprog in  
    Ok (asm_gen fullprog comp_unit)
  end

(* debugging paths *)
let debug_ast_cfold (ast: string) : Typecheck.full_prog Error.result =
  Result.map (typecheck ast) ~f:ast_constant_folding

let debug_ir_gen (ast: string) : Ir.comp_unit Error.result =
  Result.map (debug_ast_cfold ast) ~f:gen_comp_unit

let debug_ir_cfold (ast: string): Ir.comp_unit Error.result =
  Result.map (debug_ir_gen ast) ~f:ir_constant_folding

let debug_ir_lower (ast: string): Ir.comp_unit Error.result =
  Result.map (debug_ir_cfold ast) ~f:lower_comp_unit

let debug_ir_blkreorder (ast: string): Ir.comp_unit Error.result =
  Result.map (debug_ir_lower ast) ~f:block_reorder_comp_unit

let asts_to_strs (tcf: string -> 'a Error.result) (strf: 'a -> string) (asts: string list) =
  let f ast =
    match tcf ast with
    | Ok el -> strf el 
    | Error e -> format_err_msg e in
  List.map ~f asts

let writes (outs: string list) (content_list: string list) = 
  let zipped = List.zip_exn outs content_list in
  Deferred.List.iter ~f:(fun (out, contents) -> Writer.save out ~contents) zipped

let main flags asts () : unit Deferred.t =
  let typed_strf (FullProg (_, prog, _): Typecheck.full_prog) : string = 
    prog |> Typecheck.sexp_of_prog |> Sexp.to_string in
  let ir_strf = sexp_of_comp_unit in

  if flags.typecheck then
    let strf = fun _ -> "Valid Xi Program" in
    let contents = asts_to_strs typecheck strf asts in
    writes flags.outputs contents
  else if flags.tcdebug then
    let contents = asts_to_strs typecheck typed_strf asts in
    writes flags.outputs contents
  else if flags.irgen_no_opt then
    let contents = asts_to_strs ir_gen_no_opt ir_strf asts in
    writes flags.outputs contents
  else if flags.irgen then
    let contents = asts_to_strs ir_gen ir_strf asts in
    writes flags.outputs contents
  else if flags.ast_cfold then
    let contents = asts_to_strs debug_ast_cfold typed_strf asts in
    writes flags.outputs contents
  else if flags.irgen then
    let contents = asts_to_strs debug_ir_gen ir_strf asts in
    writes flags.outputs contents
  else if flags.lower then
    let contents = asts_to_strs debug_ir_lower ir_strf asts in
    writes flags.outputs contents
  else if flags.blkreorder then
    let contents = asts_to_strs debug_ir_blkreorder ir_strf asts in
    writes flags.outputs contents
  else
    let contents = asts_to_strs asm_gen_opt string_of_asms asts in
    writes flags.outputs contents

let () =
  Command.async
    ~summary:"Xi Compiler"
    Command.Spec.(
      empty
      +> flag "--typecheck"      no_arg ~doc:""
      +> flag "--tcdebug"        no_arg ~doc:""
      +> flag "--irgen"          no_arg ~doc:""
      +> flag "--irgen_no_opt"   no_arg ~doc:""
      +> flag "--ast-cfold"      no_arg ~doc:""
      +> flag "--nothing"        no_arg ~doc:""
      +> flag "--lower"          no_arg ~doc:""
      +> flag "--ir-cfold"       no_arg ~doc:""
      +> flag "--blkreorder"     no_arg ~doc:""
      +> flag "--outputs"    (listed string) ~doc:""
      +> anon (sequence ("asts" %: string))
    )
    (fun tc tcd irg irg_no afold nothing ifold l b os asts ->
       let flags = {
         typecheck      =  tc;
         tcdebug        =  tcd;
         irgen          =  irg;
         irgen_no_opt   =  irg_no;
         ast_cfold      =  afold;
         nothing        =  nothing;
         lower          =  l;
         ir_cfold       =  ifold;
         blkreorder     =  b;
         outputs        =  os;
       } in
       main flags asts)
  |> Command.run
