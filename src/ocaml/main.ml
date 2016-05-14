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

(* ************************************************************************** *)
(* types                                                                      *)
(* ************************************************************************** *)
type flags = {
  (* mode *)
  typecheck      : bool;
  tcdebug        : bool;
  nolower        : bool;
  lower          : bool;
  irgen          : bool;
  optir_initial  : bool;
  optir_final    : bool;
  optcfg_initial : bool;
  optcfg_final   : bool;
  asmdebug       : bool;

  (* optimizations *)
  acf : bool;
  icf : bool;
  cp  : bool;
  pre : bool;
  is  : bool;
  reg : bool;

  (* args *)
  astfiles : string list;
} [@@deriving sexp]

type opts = {
  acf : Typecheck.full_prog -> Typecheck.full_prog;
  icf : Ir.comp_unit -> Ir.comp_unit;
  cp  : Ir.comp_unit -> Ir.comp_unit;
  pre : Ir.comp_unit -> Ir.comp_unit;
  is  : Tiling.eater;
  reg : Tiling.allocator;
}

let opts_of_flags ({acf; icf; cp; pre; is; reg; _}: flags) : opts =
  {
    acf = (if acf then Ast_optimization.ast_constant_folding else fun x -> x);
    icf = (if icf then Ir_generation.ir_constant_folding else fun x -> x);
    cp  = (if cp  then Ccp.ccp_comp_unit else fun x -> x);
    pre = (if pre then Pre.pre_comp_unit else fun x -> x);
    is  = (if is  then Tiling.asm_chomp else Tiling.asm_munch);
    reg = (if reg then Regalloc.reg_alloc else Tiling.register_allocate);
  }

(* ************************************************************************** *)
(* helpers                                                                    *)
(* ************************************************************************** *)
let ($) = Fn.compose

let format_err_output (row: int) (col: int) (msg: string) : string =
  sprintf "%d:%d error: %s" row col msg

let format_err_print (filename: string) (line: int) (col: int) (msg: string) : string =
  sprintf "Semantic error at %s:%d:%d: %s" filename line col msg

let change_ext ~(ext: string) (filename: string) =
  (Filename.chop_extension filename) ^ ext

let asts_to_strs
  (tcf: Pos.full_prog -> 'a Error.result)
  (strf: 'a -> string)
  (asts: Pos.full_prog list)
  : string list =
  let f (Ast.S.FullProg (name, _, _) as p) =
    match tcf p with
    | Ok el -> strf el
    | Error ((row, col), msg) -> (
        Print.print_endline (format_err_print name row col msg);
        format_err_output row col msg
    )
  in
  List.map ~f asts

let writes (outs: string list) (content_list: string list) =
  let zipped = List.zip_exn outs content_list in
  Deferred.List.iter ~f:(fun (out, contents) -> Writer.save out ~contents) zipped

let get_asts (astfiles: string list) : (Pos.full_prog list) Deferred.t =
  let f astfile = Reader.load_sexp_exn astfile Pos.full_prog_of_sexp in
  Deferred.List.map ~f astfiles

(* ************************************************************************** *)
(* modes                                                                      *)
(* ************************************************************************** *)
type 'a modef = opts -> Pos.full_prog -> 'a Error.result

let typecheck : Typecheck.full_prog modef = fun {acf; _} ast ->
  Result.(prog_typecheck ast >>| acf)

let nolower : Ir.comp_unit modef = fun ({icf; _} as opts) ast ->
  Result.(typecheck opts ast >>| (icf $ gen_comp_unit))

let lower : Ir.comp_unit modef = fun ({icf; cp; pre; _} as opts) ast ->
  Result.(typecheck opts ast >>| (pre $ cp $ icf $ lower_comp_unit $ gen_comp_unit))

let irgen : Ir.comp_unit modef = fun ({icf; _} as opts) ast ->
  Result.(
    typecheck opts ast >>| (
      icf $ block_reorder_comp_unit $ icf $ lower_comp_unit $ icf $ gen_comp_unit
    )
  )

let optir_initial : Ir.comp_unit modef = fun opts ast ->
  Result.(
    typecheck opts ast >>| (block_reorder_comp_unit $ lower_comp_unit $ gen_comp_unit)
  )

let optir_final : Ir.comp_unit modef = fun ({cp; pre; icf; _} as opts) ast ->
  Result.(
    typecheck opts ast >>| (
      cp $ pre $ icf $ block_reorder_comp_unit $ icf $ lower_comp_unit $ icf $
      gen_comp_unit
    )
  )

let asmgen : bool -> Asm.asm_prog modef = fun debug ({is; reg; _} as opts) ast ->
  Result.(
    typecheck opts ast >>= fun typed_prog ->
    irgen opts ast >>| fun ir_prog ->
    is ~debug reg typed_prog ir_prog
  )

(* ************************************************************************** *)
(* pretty printers                                                            *)
(* ************************************************************************** *)
let typed_strf (_: 'a) : string =
  "Valid Xi Program"

let typed_debug_strf (Ast.S.FullProg (_, prog, _): Typecheck.full_prog) : string =
  prog |> Typecheck.sexp_of_prog |> Sexp.to_string

let ir_strf : Ir.comp_unit -> string =
  sexp_of_comp_unit

let asm_strf : Asm.asm list -> string =
  string_of_asms

(* ************************************************************************** *)
(* main                                                                       *)
(* ************************************************************************** *)
let main opts flags () : unit Deferred.t =
  (* parse the asts *)
  get_asts flags.astfiles >>= fun asts ->

  (* dispatch logic *)
  let write contents = writes flags.astfiles contents in
  match () with
  | _ when flags.typecheck -> write (asts_to_strs (typecheck opts) typed_strf asts)
  | _ when flags.tcdebug   -> write (asts_to_strs (typecheck opts) typed_debug_strf asts)
  | _ when flags.nolower   -> write (asts_to_strs (nolower opts) ir_strf asts)
  | _ when flags.lower     -> write (asts_to_strs (lower opts) ir_strf asts)
  | _ when flags.irgen     -> write (asts_to_strs (irgen opts) ir_strf asts)
  | _ when flags.optir_initial || flags.optir_final ->
      let initials = List.map flags.astfiles ~f:(change_ext ~ext:"_initial.ir") in
      let finals = List.map flags.astfiles ~f:(change_ext ~ext:"_final.ir") in

      let initial =
        if flags.optir_initial
          then writes initials (asts_to_strs (optir_initial opts) ir_strf asts)
          else return ()
      in

      let final =
        if flags.optir_final
          then writes finals (asts_to_strs (optir_final opts) ir_strf asts)
          else return ()
      in

      initial >>= fun () ->
      final >>= fun () ->
      Deferred.List.iter flags.astfiles ~f:Unix.remove
  | _ when flags.optcfg_initial || flags.optcfg_final ->
      let cfg_phase names (phase: string) (filename: string) ((_, funcs): Ir.comp_unit) =
          let f (fname, (_, body, _)) =
            if fname <> "__concat" then
              match body with
              | Seq irs -> begin
                  let cfg = Cfg.IrCfg.create_cfg irs in
                  let dot = Cfg.IrCfg.to_dot cfg in
                  let ext = sprintf "_%s_%s.dot" (String.Map.find_exn names fname) phase in
                  let dot_file = change_ext ~ext filename in
                  Writer.save dot_file ~contents:dot
              end
              | _ -> failwith "optcfg: lowered ir should be a seq"
            else
              return ()
          in
          Deferred.List.iter ~f (String.Map.to_alist funcs)
      in

      let f (filename, (Ast.S.FullProg (name, _, _) as ast)) =
        match typecheck opts ast with
        | Ok (Ast.S.FullProg (_, (_, Ast.S.Prog (_, _, _, callables)), _)) ->
            let names = Ir_util.mangled_to_name callables in
            let ok = function
              | Ok o -> o
              | Error _ -> failwith "optcfg: results shouldn't have been Error"
            in
            let cfg_initial = cfg_phase names "initial" filename in
            let cfg_final = cfg_phase names "final" filename in

            let initial =
              if flags.optcfg_initial
                then cfg_initial (ok (optir_initial opts ast))
                else return ()
            in

            let final =
              if flags.optcfg_final
                then cfg_final (ok (optir_final opts ast))
                else return ()
            in

            initial >>= fun () ->
            final >>= fun () ->
            Unix.remove filename
        | Error ((row, col), msg) -> begin
            Print.print_endline (format_err_print name row col msg);
            Writer.save filename ~contents:(format_err_output row col msg)
        end
      in
      Deferred.List.iter ~f (List.zip_exn flags.astfiles asts)
  | _ when flags.asmdebug -> write (asts_to_strs (asmgen true opts) asm_strf asts)
  | _                     -> write (asts_to_strs (asmgen false opts) asm_strf asts)

let () =
  Command.async
    ~summary:"Xi Compiler"
    Command.Spec.(
      empty
      +> flag "--typecheck"      no_arg ~doc:""
      +> flag "--tcdebug"        no_arg ~doc:""
      +> flag "--nolower"        no_arg ~doc:""
      +> flag "--lower"          no_arg ~doc:""
      +> flag "--irgen"          no_arg ~doc:""
      +> flag "--optir-initial"  no_arg ~doc:""
      +> flag "--optir-final"    no_arg ~doc:""
      +> flag "--optcfg-initial" no_arg ~doc:""
      +> flag "--optcfg-final"   no_arg ~doc:""
      +> flag "--asmdebug"       no_arg ~doc:""
      +> flag "--acf"            no_arg ~doc:""
      +> flag "--icf"            no_arg ~doc:""
      +> flag "--cp"             no_arg ~doc:""
      +> flag "--pre"            no_arg ~doc:""
      +> flag "--is"             no_arg ~doc:""
      +> flag "--reg"            no_arg ~doc:""
      +> flag "--astfiles"   (listed string) ~doc:""
    )
    (fun x00 x01 x02 x03 x04 x05 x06 x07 x08 x09 x10 x11 x12 x13 x14 x15 x16 ->
       let flags = {
         typecheck      = x00;
         tcdebug        = x01;
         nolower        = x02;
         lower          = x03;
         irgen          = x04;
         optir_initial  = x05;
         optir_final    = x06;
         optcfg_initial = x07;
         optcfg_final   = x08;
         asmdebug       = x09;
         acf            = x10;
         icf            = x11;
         cp             = x12;
         pre            = x13;
         is             = x14;
         reg            = x15;
         astfiles       = x16;
       } in
       main (opts_of_flags flags) flags
    )
  |> Command.run
