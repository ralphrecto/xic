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

module TodoRemoveThis_ItsOnlyUsedToBuildCfg = Cfg
module TodoRemoveThis_ItsOnlyUsedToBuildDataflow = Dataflow
module TodoRemoveThis_ItsOnlyUsedToBuildRegalloc = Regalloc
module TodoRemoveThis_ItsOnlyUsedToBuildTranslate = Translate
module TodoRemoveThis_ItsOnlyUsedToBuildPre = Pre
module TodoRemoveThis_ItsOnlyUsedToBuildCcp = Ccp

type flags = {
  no_opt:         bool;
  typecheck:      bool;
  tcdebug:        bool;
  ast_cfold:      bool;
  basicir:        bool;
  ir_acfold:      bool;
  ir_cfold:       bool;
  lower:          bool;
  blkreorder:     bool;
  irgen:          bool;
  asmchomp:       bool;
  asmdebug:       bool;
  astfiles:       string list;
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
    block_reorder_comp_unit $
    lower_comp_unit $
    ir_constant_folding $
    gen_comp_unit $
    ast_constant_folding in
  Result.map (typecheck ast) ~f

let ir_gen_no_opt (ast: Pos.full_prog) : Ir.comp_unit Error.result =
  let f = block_reorder_comp_unit $ lower_comp_unit $ gen_comp_unit in
  Result.map (typecheck ast) ~f

let asm_gen_opt
  (debug: bool)
  (chomp: bool)
  (ast: Pos.full_prog) : Asm.asm_prog Error.result =
  let f =
    block_reorder_comp_unit $
    lower_comp_unit $
    ir_constant_folding $
    gen_comp_unit $
    ast_constant_folding in
  Result.bind (typecheck ast) begin fun fullprog ->
    let comp_unit = f fullprog in
    let asm_eat = if chomp then asm_chomp else asm_munch in
    Ok (asm_eat ~debug:debug fullprog comp_unit)
  end

let asm_gen_no_opt
  (debug: bool)
  (chomp: bool)
  (ast: Pos.full_prog) : Asm.asm_prog Error.result =
  let f = block_reorder_comp_unit $ lower_comp_unit $ gen_comp_unit in
  Result.bind (typecheck ast) begin fun fullprog ->
    let comp_unit = f fullprog in
    let asm_eat = if chomp then asm_chomp else asm_munch in
    Ok (asm_eat ~debug:debug fullprog comp_unit)
  end

(* debugging paths *)
let debug_ast_cfold (ast: Pos.full_prog) : Typecheck.full_prog Error.result =
  Result.map (typecheck ast) ~f:ast_constant_folding

let debug_ir_astcfold (ast: Pos.full_prog) : Ir.comp_unit Error.result =
  Result.map (debug_ast_cfold ast) ~f:gen_comp_unit

let debug_ir_cfold (ast: Pos.full_prog): Ir.comp_unit Error.result =
  Result.map (debug_ir_astcfold ast) ~f:ir_constant_folding

let debug_ir_lower (ast: Pos.full_prog): Ir.comp_unit Error.result =
  Result.map (debug_ir_cfold ast) ~f:lower_comp_unit

let debug_ir_blkreorder (ast: Pos.full_prog): Ir.comp_unit Error.result =
  Result.map (debug_ir_lower ast) ~f:block_reorder_comp_unit

let debug_ir_basic (ast: Pos.full_prog): Ir.comp_unit Error.result =
  Result.map (typecheck ast) ~f:gen_comp_unit

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

let get_asts (astfiles: string list) : (Pos.full_prog list) Deferred.t =
  let f astfile = Reader.load_sexp_exn astfile Pos.full_prog_of_sexp in
  Deferred.List.map ~f astfiles

let main flags () : unit Deferred.t =
  (* parse the asts *)
  get_asts flags.astfiles >>= fun asts ->

  (* functions to turn representations into strings *)
  let typed_strf = fun _ -> "Valid Xi Program" in
  let typed_debug_strf (FullProg (_, prog, _): Typecheck.full_prog) : string =
    prog |> Typecheck.sexp_of_prog |> Sexp.to_string in
  let ir_strf = sexp_of_comp_unit in

  (* dispatch logic *)
  if flags.typecheck then
    let contents = asts_to_strs typecheck typed_strf asts in
    writes flags.astfiles contents
  else if flags.tcdebug then
    let contents = asts_to_strs typecheck typed_debug_strf asts in
    writes flags.astfiles contents
  else if flags.ast_cfold then
    let contents = asts_to_strs debug_ast_cfold typed_debug_strf asts in
    writes flags.astfiles contents
  else if flags.basicir then
    let contents = asts_to_strs debug_ir_basic ir_strf asts in
    writes flags.astfiles contents
  else if flags.ir_acfold then
    let contents = asts_to_strs debug_ir_astcfold ir_strf asts in
    writes flags.astfiles contents
  else if flags.ir_cfold then
    let contents = asts_to_strs debug_ir_cfold ir_strf asts in
    writes flags.astfiles contents
  else if flags.lower then
    let contents = asts_to_strs debug_ir_lower ir_strf asts in
    writes flags.astfiles contents
  else if flags.blkreorder then
    let contents = asts_to_strs debug_ir_blkreorder ir_strf asts in
    writes flags.astfiles contents
  else if flags.irgen && flags.no_opt then
    let contents = asts_to_strs ir_gen_no_opt ir_strf asts in
    writes flags.astfiles contents
  else if flags.irgen then
    let contents = asts_to_strs ir_gen ir_strf asts in
    writes flags.astfiles contents
  else if flags.no_opt then
    let asm_gen = asm_gen_no_opt flags.asmdebug flags.asmchomp in
    let contents = asts_to_strs asm_gen string_of_asms asts in
    writes flags.astfiles contents
  else
    let asm_gen = asm_gen_opt flags.asmdebug flags.asmchomp in
    let contents = asts_to_strs asm_gen string_of_asms asts in
    writes flags.astfiles contents

let () =
  Command.async
    ~summary:"Xi Compiler"
    Command.Spec.(
      empty
      +> flag "--no-opt"         no_arg ~doc:""
      +> flag "--typecheck"      no_arg ~doc:""
      +> flag "--tcdebug"        no_arg ~doc:""
      +> flag "--ast-cfold"      no_arg ~doc:""
      +> flag "--basicir"        no_arg ~doc:""
      +> flag "--ir-acfold"       no_arg ~doc:""
      +> flag "--ir-cfold"       no_arg ~doc:""
      +> flag "--lower"          no_arg ~doc:""
      +> flag "--blkreorder"     no_arg ~doc:""
      +> flag "--irgen"          no_arg ~doc:""
      +> flag "--asmchomp"       no_arg ~doc:""
      +> flag "--asmdebug"       no_arg ~doc:""
      +> flag "--astfiles"    (listed string) ~doc:""
    )
    (fun x00 x01 x02 x03 x04 x05 x06 x07 x08 x09 x10 x11 x12 ->
       let flags = {
          no_opt       = x00;
          typecheck    = x01;
          tcdebug      = x02;
          ast_cfold    = x03;
          basicir      = x04;
          ir_acfold    = x05;
          ir_cfold     = x06;
          lower        = x07;
          blkreorder   = x08;
          irgen        = x09;
          asmchomp     = x10;
          asmdebug     = x11;
          astfiles = x12;
       } in
       main flags)
  |> Command.run
