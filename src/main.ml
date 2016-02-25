open Core.Std
open Async.Std

let main filenames () : unit Deferred.t =
  Deferred.List.iter filenames ~f:(fun filename ->
    Reader.load_sexp_exn filename Pos.prog_of_sexp
    >>| Pos.sexp_of_prog
    >>| Sexp.to_string
    >>| print_endline
  )

let () =
  Command.async
    ~summary:""
    Command.Spec.(
      empty
      +> anon (sequence ("FILE" %: file))
    )
    main
  |> Command.run
