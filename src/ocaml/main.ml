module StdString = String
open Core.Std
open Async.Std
open Typecheck
open Ast_optimization
open Ir
open Ir_generation
open Ir_printer
open Printf
open Xi_interpreter

type flags = {
  typecheck:  bool;           (* --typecheck  *)
  xirun: bool;               (* --xi-run     *)
  irgen:  bool;               (* --irgen      *)
  ast_cfold: bool;            (* --ast-cfold  *)
  ir_cfold: bool;             (* --ir-cfold   *)
  lower: bool;                (* --lower      *)
  blkreorder: bool;           (* --blkreorder *)
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
  List.iter ~f:print_endline asts;
  let typechecked = asts
    |> List.map ~f:StdString.trim 
    |> List.map ~f:Sexp.of_string
    |> List.map ~f:Pos.full_prog_of_sexp
    |> List.map ~f:prog_typecheck in
  if flags.typecheck then
    typechecked
      |> List.map ~f:(function
        | Ok _ -> "Valid Xi Program"
        | Error e -> format_err_msg e)
      |> List.iter ~f:print_endline
      |> return
  else if flags.xirun then
    typechecked
      |> do_if flags.ast_cfold (resmap ~f:ast_constant_folding)
      |> resmap ~f:(eval_prog String.Map.empty)
      |> resmap ~f:get_main_val
      |> resmap ~f:(function Some v -> string_of_value v | None -> "no return")
      |> List.map ~f:(function Ok s -> s | Error e -> format_err_msg e)
      |> List.iter ~f:print_endline
      |> return
  else if flags.irgen then
    typechecked
      |> do_if flags.ast_cfold (resmap ~f:ast_constant_folding)
      |> resmap ~f:gen_comp_unit
      |> do_if flags.ir_cfold (resmap ~f:ir_constant_folding)
      |> do_if flags.lower (resmap ~f:lower_comp_unit)
      |> do_if flags.lower (resmap ~f:block_reorder_comp_unit)
      |> resmap ~f:sexp_of_comp_unit
      |> List.map ~f:(function Ok s -> s | Error e -> format_err_msg e)
      |> List.iter ~f:print_endline
      |> return
  else return ()


let () =
  Command.async
    ~summary:"Xi Compiler"
    Command.Spec.(
      empty
      +> flag "--typecheck" no_arg ~doc:""
      +> flag "--xirun" no_arg ~doc:""
      +> flag "--irgen" no_arg ~doc:""
      +> flag "--ast-cfold" no_arg ~doc:""
      +> flag "--ir-cfold" no_arg ~doc:""
      +> flag "--lower-cfold" no_arg ~doc:""
      +> flag "--blkreorder" no_arg ~doc:""
      +> anon (sequence ("asts" %: string))
    )
    (fun tc xir irg afold ifold l b asts ->
       let flags = {
         typecheck = tc;
         xirun = xir;
         irgen = irg;
         ast_cfold = afold;
         ir_cfold = ifold; 
         lower = l; 
         blkreorder = b; 
       } in
       main flags asts)
  |> Command.run
