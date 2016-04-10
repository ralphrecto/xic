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

let format_err_output row col msg =
  row ^ ":" ^ col ^ " error: " ^ msg

let format_err_print filename line col msg = 
  "Semantic error at " ^ filename ^ ":" ^ line ^ ":" ^ col ^ ": " ^ msg

let get_callable_decls ((FullProg (_, _, interfaces)): Pos.full_prog) = 
 List.fold_left
   ~f:(fun acc (_, Interface clist) -> clist @ acc)
   ~init:[] interfaces

let ( $ ) = Fn.compose

(* actual compiler options *)
let typecheck (ast: Pos.full_prog) : Typecheck.full_prog Error.result = 
    prog_typecheck ast

let ir_gen (ast: Pos.full_prog) : Ir.comp_unit Error.result = 
  let f = 
    let g1 = gen_comp_unit $ ast_constant_folding in
    let g2 = ir_constant_folding $ g1 in
    let g3 = lower_comp_unit $ g2 in
    block_reorder_comp_unit $ g3 in
  Result.map (typecheck ast) ~f

let ir_gen_no_opt (ast: Pos.full_prog) : Ir.comp_unit Error.result = 
  let f = block_reorder_comp_unit $ (lower_comp_unit $ gen_comp_unit) in
  Result.map (typecheck ast) ~f

let asm_gen_opt (ast: Pos.full_prog) : Asm.asm_prog Error.result =
  let f = 
    let g1 = gen_comp_unit $ ast_constant_folding in
    let g2 = ir_constant_folding $ g1 in
    let g3 = lower_comp_unit $ g2 in
    block_reorder_comp_unit $ g3 in
  Result.bind (typecheck ast) begin fun fullprog -> 
    let comp_unit = f fullprog in  
    Ok (asm_gen fullprog comp_unit)
  end

let asm_gen_no_opt (ast: Pos.full_prog) : Asm.asm_prog Error.result =
  let f = block_reorder_comp_unit $ (lower_comp_unit $ gen_comp_unit) in
  Result.bind (typecheck ast) begin fun fullprog -> 
    let comp_unit = f fullprog in  
    Ok (asm_gen fullprog comp_unit)
  end

(* debugging paths *)
let debug_ast_cfold (ast: Pos.full_prog) : Typecheck.full_prog Error.result =
  Result.map (typecheck ast) ~f:ast_constant_folding

let debug_ir_gen (ast: Pos.full_prog) : Ir.comp_unit Error.result =
  Result.map (debug_ast_cfold ast) ~f:gen_comp_unit

let debug_ir_cfold (ast: Pos.full_prog): Ir.comp_unit Error.result =
  Result.map (debug_ir_gen ast) ~f:ir_constant_folding

let debug_ir_lower (ast: Pos.full_prog): Ir.comp_unit Error.result =
  Result.map (debug_ir_cfold ast) ~f:lower_comp_unit

let debug_ir_blkreorder (ast: Pos.full_prog): Ir.comp_unit Error.result =
  Result.map (debug_ir_lower ast) ~f:block_reorder_comp_unit

let asts_to_strs
  (tcf: Pos.full_prog -> 'a Error.result)
  (strf: 'a -> string)
  (asts: Pos.full_prog list) =
  let f (Ast.S.FullProg (name, prog, interfaces)) =
    match tcf (Ast.S.FullProg (name, prog, interfaces)) with
    | Ok el -> strf el 
    | Error ((row, col), msg) ->
        let row_s = string_of_int row in
        let col_s = string_of_int col in
        Print.print_endline (format_err_print name row_s col_s msg);
        format_err_output row_s col_s msg in
  List.map ~f asts

let writes (outs: string list) (content_list: string list) = 
  let zipped = List.zip_exn outs content_list in
  Deferred.List.iter ~f:(fun (out, contents) -> Writer.save out ~contents) zipped

let get_asts (asts: string list) : Pos.full_prog list = 
  let f = Pos.full_prog_of_sexp $ Sexp.of_string $ StdString.trim in
  List.map ~f asts

let main flags ast_strs () : unit Deferred.t =
  (* parse the asts *)
  let asts = get_asts ast_strs in 

  (* functions to turn representations into strings *)
  let typed_strf (FullProg (_, prog, _): Typecheck.full_prog) : string = 
    prog |> Typecheck.sexp_of_prog |> Sexp.to_string in
  let ir_strf = sexp_of_comp_unit in

  (* dispatch logic *)
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
