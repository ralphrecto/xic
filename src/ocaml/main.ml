open Core.Std
open Async.Std
open Typecheck
open Ir
open Ir_generation
open Ir_printer
open Printf

type flags = {
  help:       bool;           (* --help      *)
  lex:        bool;           (* --lex       *)
  parse:      bool;           (* --parse     *)
  typecheck:  bool;           (* --typecheck *)
  sourcepath: string option;  (* -sourcepath *)
  libpath:    string option;  (* -libpath    *)
  outpath:    string option;  (* -D          *)
} [@@deriving sexp]

let change_ext ext filename =
  filename
  |> Filename.split_extension
  |> fun (s,_) -> s ^ ext

(* errors if loading the sexp fails *)
let typecheck_file filename : (string * string, unit) Result.t Deferred.t = 
  Reader.load_sexp filename Pos.full_prog_of_sexp
  >>| function
  | Ok full_p -> begin
      match prog_typecheck full_p with
      | Ok _ -> Ok ("Valid Xi Program", filename)
      | Error ((row,col), msg) ->
        let row_s = string_of_int row in
        let col_s = string_of_int col in
        let stdout_msg =
          "Semantic error beginning at " ^
          row_s ^ ":" ^ col_s ^ ": " ^ msg in
        Print.print_endline stdout_msg;
        Ok (row_s ^ ":" ^ col_s ^ " error:" ^ msg, filename)
    end
  | Error _ -> Error ()

let main flags filenames () : unit Deferred.t =
  let rebase filename =
    let open Filename in
    let base = match flags.outpath with
      | Some s -> s
      | None -> dirname filename in
    concat base (basename filename) in

  if not flags.typecheck then return ()
  else filenames
       |> List.map ~f:(fun fn -> rebase fn |> change_ext ".typed")
       |> Deferred.List.map ~f:typecheck_file
    >>= Deferred.List.iter ~how:`Sequential ~f:(function
        | Ok (msg, file) ->
          let f writer =
            Writer.write_line writer msg |> return in
          Writer.with_file file ~f
        | Error _ -> return ()
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
      +> flag "-sourcepath" (optional file) ~doc:""
      +> flag "-libpath"    (optional file) ~doc:""
      +> flag "-D"          (optional file) ~doc:""
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
