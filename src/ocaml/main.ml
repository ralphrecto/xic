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
  irgen:  bool;               (* --irgen      *)
  optimize: bool;             (* --optimize   *)
} [@@deriving sexp]

let flatmap ~f =
  List.map ~f:(function
    | Ok x -> Ok (f x)
    | Error e-> Error e)

let do_if (b: bool) (f: 'a -> 'a) (x: 'a) : 'a =
  if b then f x else x

let format_err_msg ((row, col), msg) =
  let row_s = string_of_int row in
  let col_s = string_of_int col in
  row_s ^ ":" ^ col_s ^ " error:" ^ msg

let main flags asts () : unit Deferred.t =
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
  else if flags.irgen then
    typechecked
      |> flatmap ~f:(do_if flags.optimize ast_constant_folding)
      |> flatmap ~f:gen_comp_unit
      |> flatmap ~f:(do_if flags.optimize ir_constant_folding)
      |> flatmap ~f:lower_comp_unit
      |> flatmap ~f:block_reorder_comp_unit
      |> flatmap ~f:sexp_of_comp_unit
      |> List.map ~f:(function Ok s -> s | Error e -> format_err_msg e)
      |> List.iter ~f:print_endline
      |> return
  else return ()

let () =
  Command.async
    ~summary:"Xi Compiler"
    Command.Spec.(
      empty
      +> flag "--typecheck" (optional_with_default true bool) ~doc:""
      +> flag "--irgen" (optional_with_default false bool) ~doc:""
      +> flag "--optimize" (optional_with_default false bool) ~doc:""
      +> anon (sequence ("asts" %: string))
    )
    (fun t i o asts ->
       let flags = {
         typecheck = t;
         irgen = i;
         optimize = o;
       } in
       main flags asts)
  |> Command.run
