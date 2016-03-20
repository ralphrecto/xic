open Core.Std
open Async.Std

let main typed_ast_filename () =
  Reader.load_sexp_exn typed_ast_filename Typecheck.prog_of_sexp >>| Xi_interpreter.eval

let () =
  Command.async
    ~summary:"Xi Interpreter"
    Command.Spec.(empty +> anon ("typed_ast" %: file))
    main
  |> Command.run
