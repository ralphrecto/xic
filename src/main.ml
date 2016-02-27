open Core.Std
open Async.Std

type flags = {
  help:       bool;   (* --help      *)
  lex:        bool;   (* --lex       *)
  parse:      bool;   (* --parse     *)
  typecheck:  bool;   (* --typecheck *)
  sourcepath: string; (* -sourcepath *)
  libpath:    string; (* -libpath    *)
  outpath:    string; (* -D          *)
} [@@deriving sexp]

let main flags filenames () : unit Deferred.t =
  Typecheck.dummy ();
  print_endline (Sexp.to_string (sexp_of_flags flags));
  Deferred.List.iter filenames ~f:(fun filename ->
    Reader.load_sexp_exn filename Pos.prog_of_sexp
    >>| Pos.sexp_of_prog
    >>| Sexp.to_string
    >>| print_endline
  )

let () =
  Command.async
    ~summary:"Xi Compiler"
    Command.Spec.(
      empty
      +> flag "--help"      no_arg ~doc:""
      +> flag "--lex"       no_arg ~doc:""
      +> flag "--parse"     no_arg ~doc:""
      +> flag "--typecheck" no_arg ~doc:""
      +> flag "-sourcepath" (optional_with_default "." file) ~doc:""
      +> flag "-libpath"    (optional_with_default "." file) ~doc:""
      +> flag "-D"          (optional_with_default "." file) ~doc:""
      +> anon (sequence ("file" %: file))
    )
    (fun h l p t s lib o filenames ->
      let flags = {
        help = h;
        lex = l;
        parse =  p;
        typecheck = t;
        sourcepath = s;
        libpath = lib;
        outpath = o;
      } in
      main flags filenames)
  |> Command.run
