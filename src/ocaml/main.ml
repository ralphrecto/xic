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

let main flags asts () : unit Deferred.t =
  let open Ast.S in
  let typeds =
    asts
    |> List.map ~f:StdString.trim
    |> List.map ~f:Sexp.of_string
    |> List.map ~f:Pos.full_prog_of_sexp
    |> List.map ~f:(fun (FullProg (prog, interfaces)) ->
       match prog_typecheck (FullProg (prog, interfaces)) with
       | Ok p ->
         let callables =
           List.fold_left
             ~f:(fun acc (_, Interface clist) -> clist @ acc)
             ~init:[] interfaces in
         Ok (abi_callable_decl_names callables, p)
       | Error e -> Error e)
  in
  if flags.typecheck then begin
    Deferred.List.iter (List.zip_exn typeds flags.outputs) ~f:(fun (typed, out) ->
      match typed with
      | Ok (_, p) ->
          if flags.tcdebug then
            let sexp = Typecheck.sexp_of_prog p in
            Writer.save out ~contents:(Sexp.to_string sexp)
          else
            Writer.save out ~contents:"Valid Xi Program"
      | Error e -> return (print_endline (format_err_msg e)))
  end else if flags.irgen then
    Deferred.List.iter (List.zip_exn typeds flags.outputs) ~f:(fun (typed, out) ->
      let ir = begin
        let open Result in
        typed >>| fun (callnames, ast) ->
        let ast = do_if flags.ast_cfold ast_constant_folding ast in
        let ir = gen_comp_unit callnames ast in
        let ir = do_if flags.ir_cfold ir_constant_folding ir in
        let ir = do_if flags.lower lower_comp_unit ir in
        let ir = do_if flags.blkreorder block_reorder_comp_unit ir in
        sexp_of_comp_unit ir
      end in
      match ir with
      | Ok s -> Writer.save out ~contents:s
      | Error e -> return (print_endline (format_err_msg e))
    )
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
