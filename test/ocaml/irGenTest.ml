module StdString = String
open Core.Std
open Async.Std
open Async_unix
open Typecheck
open Ir
open Ir_generation
open TestUtil
open String

let flatmap ~f =
  List.map ~f:(function
    | Some x -> f x
    | None -> None)

let optionize (f: 'a -> 'b) =
  fun x -> Some (f x)

let main programs () =
  (*
  programs
    |> List.map ~f:StdString.trim
    |> List.map ~f:Sexp.of_string
    |> List.map ~f:Pos.full_prog_of_sexp
    |> List.map ~f:prog_typecheck
    |> List.map ~f:(function Result.Ok x -> Some x | Result.Error _ -> None)
    |> flatmap ~f:(optionize gen_comp_unit)
    |> flatmap ~f:(optionize Ir_printer.sexp_of_comp_unit)
    |> List.map ~f:(function Some s -> s | None -> "gen error")
    |> List.iter ~f:Async_print.print_endline
    *)
  return ()

let () =
  Command.async
    ~summary:"IrGen tests"
    Command.Spec.(
      empty
      +> anon (sequence ("program" %: string))
    )
    (fun xi_sources -> main xi_sources)
  |> Command.run
