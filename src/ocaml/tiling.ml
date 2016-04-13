open Core.Std
open Asm
open Func_context
open Ir_generation
open Typecheck

module FreshReg    = Fresh.Make(struct let name = "_asmreg" end)
module FreshAsmRet = Fresh.Make(struct let name = "_asmret" end)
module FreshLabel  = Fresh.Make(struct let name = "_asmlabel" end)
let ret_ptr_reg   = Fake "_asmretptr"

type ('input, 'output) with_fcontext =
  ?debug:bool -> func_context -> func_contexts -> 'input -> 'output
type ('input, 'output) without_fcontext =
  ?debug:bool -> func_contexts -> 'input -> 'output

let max_int32 = Int64.of_int32_exn (Int32.max_value)
let min_int32 = Int64.of_int32_exn (Int32.min_value)

let register_allocate ?(debug=false) asms =
  (* spill_env maps each fake name to an index, starting at 15, into the stack.
   * We start at 15 since the first 14 is reserved for callee save registers.
   * For example, if the fake name "foo" is mapped to n in spill_env, then Reg
   * (Fake "foo") will be spilled to -8n(%rbp). *)
  let spill_env =
    fakes_of_asms asms
    |> List.mapi ~f:(fun i asm -> (asm, i + 15))
    |> String.Map.of_alist_exn
  in

  (* Given an environment and name, return the memory address that fake is
   * spilled into. For example, `spill_address {"foo": 4}` "foo" = -32(%rbp)` *)
  let spill_address (spill_env: int String.Map.t) (fake: string) : reg operand =
    let i = String.Map.find_exn spill_env fake in
    let offset = Int64.of_int (-8 * i) in
    Mem (Base (Some offset, Rbp))
  in

  (* Recursively applies f to all the abstract_registers in asm. *)
  let abstract_reg_map (f: abstract_reg -> reg)  (asm: abstract_asm) =
    match asm with
    | Op (s, operands) ->
        Op (s, List.map operands ~f:(fun operand ->
          match operand with
          | Reg r -> Reg (f r)
          | Mem (Base (n, base)) -> Mem (Base (n, f base))
          | Mem (Off (n, off, scale)) -> Mem (Off (n, f off, scale))
          | Mem (BaseOff (n, base, off, scale)) -> Mem (BaseOff (n, f base, f off, scale))
          | Label l -> Label l
          | Const c -> Const c
        ))
    | Lab l -> Lab l
    | Directive (d, args) -> Directive (d, args)
    | Comment s -> Comment s
  in

  (* Certain real registers are output into abstract assembly. For example,
   * multiplication and division use rax and rdx. These registers shouldn't be
   * present in abstract assembly. *)
  let unused_regs = [R13; R14; R15] in

  (* Translate fake registers using the register environment and leave real
   * registers alone. *)
  let translate_reg (reg_env: reg String.Map.t) (r: abstract_reg) : reg =
    match r with
    | Fake s -> String.Map.find_exn reg_env s
    | Real r -> r
  in

  let allocate (env: int String.Map.t) (asm: abstract_asm) : asm list =
    let spill = spill_address env in
    match asm with
    | Op (_, operands) ->
      let fakes = fakes_of_operands operands in
      let unused_regs = List.take unused_regs (List.length fakes) in
      let fake_to_real = List.zip_exn fakes unused_regs in
      let reg_env = String.Map.of_alist_exn fake_to_real in
      let fake_to_op f = Reg (String.Map.find_exn reg_env f) in

      let pre = List.map fakes ~f:(fun fake -> movq (spill fake) (fake_to_op fake)) in
      let translation = [abstract_reg_map (translate_reg reg_env) asm] in
      let post = List.map fakes ~f:(fun fake -> movq (fake_to_op fake) (spill fake)) in
      pre @ translation @ post
    | Lab l -> [Lab l]
    | Directive (d, args) -> [Directive (d, args)]
    | Comment s -> [Comment s]
  in

  let allocated = List.concat_map ~f:(allocate spill_env) asms in
  if debug then
    let mapping = [] in
    let mapping = mapping @ [Comment "----- begin register mapping"] in
    let mapping = mapping @ (
      String.Map.to_alist spill_env
      |> List.sort ~cmp:(fun (_, i1) (_, i2) -> compare i1 i2)
      |> List.map ~f:(fun (s, i) -> Comment (sprintf "-%d(%%rbp): %s" (i * 8) s))
    ) in
    let mapping = mapping @ [Comment "----- end register mapping"] in
    mapping @ allocated
  else
    allocated

let binop_to_instr (op: Ir.binop_code) =
  match op with
  | Ir.ADD -> addq
  | Ir.SUB -> subq
  | Ir.AND -> andq
  | Ir.OR -> orq
  | Ir.XOR -> xorq
  | _ -> failwith "shouldn't happen -- binop_to_instr"

let binop_commutative (op: Ir.binop_code) =
  match op with
  | Ir.SUB -> false
  | Ir.ADD
  | Ir.AND
  | Ir.OR
  | Ir.XOR -> true
  | _ -> failwith "shouldn't happen -- binop_commutative"

let cmp_to_instr (op: Ir.binop_code) =
  match op with
  | Ir.EQ  -> asete
  | Ir.NEQ -> asetne
  | Ir.LT  -> asetl
  | Ir.GT  -> asetg
  | Ir.LEQ -> asetle
  | Ir.GEQ -> asetge
  | _ -> failwith "shouldn't happen -- cmp_to_instr"

let cmp_zero_to_instr (op: Ir.binop_code) =
  let open Ir in
  match op with
  | EQ  -> asetz
  | NEQ -> asetnz
  | LT  -> asets
  | GT  -> asetg
  | LEQ -> asetle
  | GEQ -> asetns
  | _ -> failwith "shouldn't happen -- cmp_zero_to_instr"

let shift_to_instr (op: Ir.binop_code) =
  let open Ir in
  match op with
  | LSHIFT -> shlq
  | RSHIFT -> shrq
  | ARSHIFT -> sarq
  | _ -> failwith "shouldn't happen -- shift_to_instr"

let cmp_to_jump_instr (op: Ir.binop_code) label =
  let open Ir in
  match op with
  | EQ -> je label
  | NEQ -> jne label
  | LT -> jl label
  | GT -> jg label
  | LEQ -> jle label
  | GEQ -> jge label
  | _ -> failwith "shouldn't happen cmp_to_jump_instr"

let cmp_zero_to_jump_instr (op: Ir.binop_code) label =
  let open Ir in
  match op with
  | EQ -> jz label
  | NEQ -> jnz label
  | LT -> js label
  | GT -> jg label
  | LEQ -> jle label
  | GEQ -> jns label
  | _ -> failwith "shouldn't happen cmp_zero_to_jump_instr"

let cmp_zero_jump op reg label =
  [test (Reg reg) (Reg reg); (cmp_zero_to_jump_instr op label)]

let non_imm_cmp_jump op reg1 reg2 label =
  [cmpq (Reg reg2) (Reg reg1); (cmp_to_jump_instr op label)]

let imm_cmp_jump op reg const label =
  [cmpq (Asm.Const const) (Reg reg); (cmp_to_jump_instr op label)]

let non_imm_cmp op reg1 reg2 dest =
  [cmpq (Reg reg2) (Reg reg1);
   (cmp_to_instr op) (Reg (Real Cl));
   movq (Reg (Real Rcx)) (Reg dest)]

let cmp_zero op reg1 dest =
  [test (Reg reg1) (Reg reg1);
  (cmp_zero_to_instr op) (Reg (Real Cl));
  movq (Reg (Real Rcx)) (Reg dest)]

let imm_cmp op const reg dest =
  [cmpq (Asm.Const const) (Reg reg);
  (cmp_to_instr op) (Reg (Real Cl));
  movq (Reg (Real Rcx)) (Reg dest)]

(* Java style shifting: mod RHS operand by word size *)
let non_imm_shift op reg1 reg2 dest =
  if reg1 = dest then
    [movq (Reg reg2) (Reg (Real Rcx)); (shift_to_instr op) (Reg (Real Cl)) (Reg reg1)]
  else
    [movq (Reg reg2) (Reg (Real Rcx)); (shift_to_instr op) (Reg (Real Cl)) (Reg reg1); movq (Reg reg1) (Reg dest)]

let imm_shift op const reg dest =
  if reg = dest then
    [(shift_to_instr op) (Asm.Const const) (Reg reg)]
  else
    [(shift_to_instr op) (Asm.Const const) (Reg reg); movq (Reg reg) (Reg dest)]

let imm_binop op const reg dest =
  if reg = dest then
    [(binop_to_instr op) (Asm.Const const) (Reg reg)]
  else
    [(binop_to_instr op) (Asm.Const const) (Reg reg); movq (Reg reg) (Reg dest)]

(* Note that `reg1 irop reg2` is compiled to `asmop reg2 reg1` not
 * `asmop reg1 reg2`. *)
let non_imm_binop op reg1 reg2 dest =
  if reg1 = dest then
    [(binop_to_instr op) (Reg reg2) (Reg reg1)]
  else if reg2 = dest && binop_commutative op then
    [(binop_to_instr op) (Reg reg1) (Reg reg2)]
  else
    [(binop_to_instr op) (Reg reg2) (Reg reg1); movq (Reg reg1) (Reg dest)]

(* register saving *)
let caller_saved_no_sp = [
  Rax;
  Rcx;
  Rdx;
  Rsi;
  Rdi;
  R8;
  R9;
  R10;
  R11;
]

let callee_saved_no_bp = [
  Rbx;
  R12;
  R13;
  R14;
  R15;
]

(* Returns the memory location in the stack of a callee/caller saved registers.
 * See the diagram in tiling.mli. *)
let mem_of_saved_reg r =
  match r with
  | Rax -> -( 1 * 8) $ (Real Rbp)
  | Rbx -> -( 2 * 8) $ (Real Rbp)
  | Rcx -> -( 3 * 8) $ (Real Rbp)
  | Rdx -> -( 4 * 8) $ (Real Rbp)
  | Rsi -> -( 5 * 8) $ (Real Rbp)
  | Rdi -> -( 6 * 8) $ (Real Rbp)
  | R8  -> -( 7 * 8) $ (Real Rbp)
  | R9  -> -( 8 * 8) $ (Real Rbp)
  | R10 -> -( 9 * 8) $ (Real Rbp)
  | R11 -> -(10 * 8) $ (Real Rbp)
  | R12 -> -(11 * 8) $ (Real Rbp)
  | R13 -> -(12 * 8) $ (Real Rbp)
  | R14 -> -(13 * 8) $ (Real Rbp)
  | R15 -> -(14 * 8) $ (Real Rbp)
  | Rbp -> failwith "we shouldn't be saving the base pointer"
  | Rsp -> failwith "we shouldn't be saving the stack pointer"
  | Cl  -> failwith "we shouldn't be saving %cl"

(* Wrap asm with string if debug is true. *)
let debug_wrap (debug: bool) (pre: string) (post: string) asms =
  if debug
    then [Comment pre] @ asms @ [Comment post]
    else asms

let section_wrap debug asms name =
  let pre = "----- start " ^ name ^ " -----" in
  let post = "----- end " ^ name ^ " -----" in
  debug_wrap debug pre post asms

let create_binop_instr (opcode: Ir.binop_code) reg1 asm1 reg2 asm2 dest =
  match opcode with
  | ADD | SUB | AND | OR | XOR ->
    begin
      match dest with
      | Some dest_reg -> (dest_reg, asm1 @ asm2 @ (non_imm_binop opcode reg1 reg2 dest_reg))
      | None -> (reg1, asm1 @ asm2 @ (non_imm_binop opcode reg1 reg2 reg1))
    end
  | LSHIFT | RSHIFT | ARSHIFT ->
    begin
      match dest with
      | Some dest_reg -> (dest_reg, asm1 @ asm2 @ (non_imm_shift opcode reg1 reg2 dest_reg))
      | None -> (reg1, asm1 @ asm2 @ (non_imm_shift opcode reg1 reg2 reg1))
    end
  | EQ | NEQ | LT | GT | LEQ | GEQ ->
    begin
      match dest with
      | Some dest_reg -> (dest_reg, asm1 @ asm2 @ (non_imm_cmp opcode reg1 reg2 dest_reg))
      | None -> (reg2, asm1 @ asm2 @ (non_imm_cmp opcode reg1 reg2 reg2))
    end
  | MUL | HMUL ->
    begin
      match dest with
      | Some dest_reg ->
        let r = if opcode = MUL then Real Rax else Real Rdx in
        let mul_asm = [movq (Reg reg2) (Reg (Real Rax)); imulq (Reg reg1); movq (Reg r) (Reg dest_reg)] in
        (dest_reg, asm1 @ asm2 @ mul_asm)
      | None ->
        let r = if opcode = MUL then Real Rax else Real Rdx in
        let mul_asm = [movq (Reg reg2) (Reg (Real Rax)); imulq (Reg reg1); movq (Reg r) (Reg reg2)] in
        (reg2, asm1 @ asm2 @ mul_asm)
    end
  | DIV | MOD ->
    begin
      match dest with
      | Some dest_reg ->
        let r = if opcode = DIV then Real Rax else Real Rdx in
        let div_asm = [movq (Reg reg1) (Reg (Real Rax)); idivq (Reg reg2); movq (Reg r) (Reg dest_reg)] in
        (dest_reg, asm1 @ asm2 @ div_asm)
      | None ->
        let r = if opcode = DIV then Real Rax else Real Rdx in
        let div_asm = [movq (Reg reg1) (Reg (Real Rax)); idivq (Reg reg2); movq (Reg r) (Reg reg2)] in
        (reg2, asm1 @ asm2 @ div_asm)
    end

let rec munch_expr
  ?(debug=false)
  (curr_ctx: func_context)
  (fcontexts: func_contexts)
  (e: Ir.expr) : Asm.fake * Asm.abstract_asm list =
  let munch_expr = munch_expr ~debug in
  begin
    match e with
    | Ir.BinOp (e1, opcode, e2) ->
      begin
        let (reg1, asm1) = munch_expr curr_ctx fcontexts e1 in
        let (reg2, asm2) = munch_expr curr_ctx fcontexts e2 in
        match opcode with
        | Ir.ADD | Ir.SUB | Ir.AND | Ir.OR | Ir.XOR ->
          (reg1, asm1 @ asm2 @ (non_imm_binop opcode (Fake reg1) (Fake reg2) (Fake reg1)))
        | Ir.LSHIFT | Ir.RSHIFT | Ir.ARSHIFT ->
          (reg1, asm1 @ asm2 @ (non_imm_shift opcode (Fake reg1) (Fake reg2) (Fake reg1)))
        | Ir.EQ | Ir.NEQ | Ir.LT | Ir.GT | Ir.LEQ | Ir.GEQ ->
          (reg2, asm1 @ asm2 @ (non_imm_cmp opcode (Fake reg1) (Fake reg2) (Fake reg2)))
        | Ir.MUL | Ir.HMUL ->
          let r = if opcode = Ir.MUL then Rax else Rdx in
          let mul_asm = [
            movq (Reg (Fake reg1)) (Reg (Real Rax));
            imulq (Reg (Fake reg2));
            movq (Reg (Real r)) (Reg (Fake reg2));
          ] in
          (reg2, asm1 @ asm2 @ mul_asm)
        | Ir.DIV | Ir.MOD ->
          let r = if opcode = Ir.DIV then Rax else Rdx in
          let div_asm = [
            xorq (Reg (Real Rdx)) (Reg (Real Rdx));
            movq (Reg (Fake reg1)) (Reg (Real Rax));
            idivq (Reg (Fake reg2));
            movq (Reg (Real r)) (Reg (Fake reg2))
          ] in
          (reg2, asm1 @ asm2 @ div_asm)
      end
    | Ir.Const c ->
        let new_tmp = FreshReg.fresh () in
        (new_tmp, [movq (Asm.Const c) (Reg (Fake new_tmp))])
    | Ir.Mem (e, _) ->
        let (e_reg, e_asm) = munch_expr curr_ctx fcontexts e in
        (e_reg, e_asm @ [movq (Mem (Base (None, (Fake e_reg)))) (Reg (Fake e_reg))])
    | Ir.Temp str -> begin
        let new_tmp = FreshReg.fresh () in
        match FreshRetReg.get str, FreshArgReg.get str with
        | Some i, None ->
            (* moving rets from callee return *)
            (FreshAsmRet.gen i, [])
        | None, Some i ->
            (* moving passed arguments as callee into vars *)
            let i' = if curr_ctx.num_rets > 2 then i + 1 else i in
            (new_tmp, [movq (Asm.callee_arg_op i') (Reg (Fake new_tmp))])
        | None, None -> (new_tmp, [movq (Reg (Fake str)) (Reg (Fake new_tmp))])
        | Some _, Some _ -> failwith "impossible: reg can't be ret and arg"
    end
    | Ir.Call (Ir.Name (fname), arglist) -> begin
        (* save caller-saved registers *)
        let saving_caller_asm = List.map caller_saved_no_sp ~f:(fun r ->
          movq (Reg (Real r)) (Mem (mem_of_saved_reg r))
        ) in

        (* evaluate arguments *)
        let f = munch_expr curr_ctx fcontexts in
        let (arg_regs, arg_asms) = List.unzip (List.map ~f arglist) in
        let arg_regs = List.map ~f:(fun r -> Fake r) arg_regs in

        (* prepare implicit 0th argument *)
        let (ret_ptr, ret_asm) =
          let callee_ctx = get_context fcontexts fname in
          if callee_ctx.num_rets > 2 then
            let new_tmp = Fake (FreshReg.fresh ()) in
            let asm = leaq (Mem ((8*(max (curr_ctx.max_args-6) 0))$(Real Rsp))) (Reg new_tmp) in
            ([new_tmp], [asm])
          else ([], []) in

        (* shuttle values into argument registers and stack *)
        let f i argsrc =
          let dest =
            match arg_reg i with
            | Some r -> Reg (Real r)
            | None -> Mem ((8 * (max (i-6) 0))$(Real Rsp))
          in
          movq (Reg argsrc) dest
        in
        let mov_asms = List.mapi ~f (ret_ptr @ arg_regs) in

        (* call function *)
        let call_asm = [call (Label fname)] in

        (* shuttle returns into fake registers *)
        let num_rets = (get_context fcontexts fname).num_rets in
        let save_rets_asms =
          List.map (List.range ~start:`inclusive ~stop:`exclusive  0 num_rets) ~f:(fun i ->
            let src = Asm.caller_ret_op ~max_args:curr_ctx.max_args ~i in
            movq src (Reg (Fake (FreshAsmRet.gen i)))
          )
        in

        (* restore caller-saved registers *)
        let restore_caller_asm = List.map caller_saved_no_sp ~f:(fun r ->
          movq (Mem (mem_of_saved_reg r)) (Reg (Real r))
        ) in

        (* sew it all together *)
        let asms = saving_caller_asm @
                   (List.concat arg_asms) @
                   ret_asm @
                   mov_asms @
                   call_asm @
                   save_rets_asms @
                   restore_caller_asm in
        (FreshAsmRet.gen 0, section_wrap debug asms ("calling " ^ fname))
    end
    | Ir.Name _ -> failwith "Name should never be munched by itself"
    | Ir.Call _ -> failwith "Call should always have a Name first"
    | Ir.ESeq _ -> failwith "ESeq shouldn't exist"
  end
  |> fun (r, asms) ->
    let pre  = (Ir.string_of_expr e) ^ " {" in
    let post = "}" in
    (r, debug_wrap debug pre post asms)

and munch_stmt
    ?(debug=false)
    (curr_ctx: func_context)
    (fcontexts: func_contexts)
    (s: Ir.stmt) =
  let munch_expr = munch_expr ~debug in
  let munch_stmt = munch_stmt ~debug in
  begin
    match s with
    | Ir.CJumpOne (e, tru) -> begin
        let (r, asms) = munch_expr curr_ctx fcontexts e in
        let jump_lst = [
          cmpq (Const 0L) (Reg (Fake r));
          jnz (Asm.Label tru);
        ] in
        asms @ jump_lst
    end
    | Ir.Jump (Ir.Name s) -> [jmp (Asm.Label s)]
    | Ir.Exp e -> snd (munch_expr curr_ctx fcontexts e)
    | Ir.Label l -> [label_op l]
    | Ir.Move (Ir.Temp n, e) -> begin
      let dest =
        (* moving return values to _RETi before returning *)
        match FreshRetReg.get n with
        | Some i -> Asm.callee_ret_op ret_ptr_reg i
        | None -> Reg (Fake n)
      in
      let (e_reg, e_lst) = munch_expr curr_ctx fcontexts e in
      e_lst @ [movq (Reg (Fake e_reg)) dest]
    end
    | Ir.Move (Ir.Mem (e1, _), e2) ->
      let (e1_reg, e1_lst) = munch_expr curr_ctx fcontexts e1 in
      let (e2_reg, e2_lst) = munch_expr curr_ctx fcontexts e2 in
      e1_lst @ e2_lst @ [movq (Reg (Fake e2_reg)) (Mem (Base (None, (Fake e1_reg))))]
    | Ir.Seq s_list -> List.concat_map ~f:(munch_stmt curr_ctx fcontexts) s_list
    | Ir.Return ->
        (* restore callee-saved registers *)
        let restore_callee_asm = List.map callee_saved_no_bp ~f:(fun r ->
          movq (Mem (mem_of_saved_reg r)) (Reg (Real r))
        ) in
        let asms = restore_callee_asm @ [leave; ret] in
        section_wrap debug asms "returning"
    | Ir.Move _ -> failwith "Move has a non TEMP lhs"
    | Ir.Jump _ -> failwith "jump to a non label shouldn't exist"
    | Ir.CJump _ -> failwith "cjump shouldn't exist"
  end
  |> fun asms ->
    let pre  = (Ir.string_of_stmt s) ^ " {" in
    let post = "}" in
    debug_wrap debug pre post asms

let eat_func_decl
    (eat_stmt: (Ir.stmt, Asm.abstract_asm list) with_fcontext)
    ?(debug=false)
    (fcontexts: func_contexts)
    ((fname, stmt, _): Ir.func_decl)
    : Asm.abstract_asm list =

  let eat_stmt = eat_stmt ~debug in

  let curr_ctx = get_context fcontexts fname in
  let body_asm = eat_stmt curr_ctx fcontexts stmt in
  let num_temps = List.length (fakes_of_asms body_asm) in

  let directives = [globl fname; align 4] in
  let label = [Lab fname] in

  (* Building function prologue
   *  - use enter to save rbp, update rbp/rsp,
   *      allocate num_temps + num_caller_save + num_callee_save + num tuple
   *      returns + num stack arguments
   *  - Align stack if not aligned to 16 bytes. To maintain this,
   *      we maintain the invariant that all stackframes have
   *      16k bytes for some k\in N. This should work if the
   *      bottom of the stack is a multiple of 16.
   *  - move passed mult. ret pointers into fresh temps
   *  - save callee saved registers
   *)
  let num_callee = List.length callee_saved_no_bp in
  let num_caller = List.length caller_saved_no_sp in
  let tot_temps = num_temps + num_callee + num_caller in
  let tot_rets_n_args = curr_ctx.max_rets + curr_ctx.max_args in
  (* saved rip + saved rbp + temps + rets n args  *)
  let tot_stack_size = 1 + 1 + tot_temps + tot_rets_n_args in

  let section_wrap = section_wrap debug in

  let prologue =
    let init = [enter (const ((tot_temps + tot_rets_n_args) * 8)) (const 0)] in
    let padding =
      if tot_stack_size mod 2 = 0
        then []
        else [pushq (const 0)] in
    let ret_ptr_mov =
      if curr_ctx.num_rets < 2
        then []
        else [movq (Asm.callee_arg_op 0) (Reg ret_ptr_reg)] in
    let save_callee = List.map callee_saved_no_bp ~f:(fun r ->
      movq (Reg (Real r)) (Mem (mem_of_saved_reg r))
    ) in
    section_wrap init "init" @
    section_wrap padding "padding" @
    section_wrap ret_ptr_mov "ret_ptr_mov" @
    section_wrap save_callee "save_callee"
  in
  section_wrap directives "directives" @
  section_wrap label "label" @
  section_wrap prologue "prologue" @
  section_wrap body_asm "body_asm"

let eat_comp_unit
    (eat_func_decl: (Ir.func_decl, Asm.abstract_asm list) without_fcontext)
    ?(debug=false)
    (fcontexts: func_contexts)
    ((_, func_decls): Ir.comp_unit)
    : Asm.abstract_asm list list =
  let decl_list = String.Map.data func_decls in
  let fun_asm : Asm.abstract_asm list list =
    List.map ~f:(eat_func_decl ~debug fcontexts) decl_list in
  let directives : Asm.abstract_asm list = [text] in
  directives :: fun_asm

let munch_func_decl
  ?(debug=false)
  (fcontexts: func_contexts)
  (func_decl: Ir.func_decl)
  : Asm.abstract_asm list =
  eat_func_decl munch_stmt ~debug fcontexts func_decl

let munch_comp_unit
    ?(debug=false)
    (fcontexts: func_contexts)
    (comp_unit: Ir.comp_unit)
    : Asm.abstract_asm list list =
  eat_comp_unit munch_func_decl ~debug fcontexts comp_unit

(* displacement is only allowed to be 32 bits *)
let get_displ (op: Ir.binop_code) x =
  let open Ir in
  if min_int32 <= x && x <= max_int32 then
    match op with
    | ADD -> Some x
    | SUB -> Some (Int64.neg x)
    | _ -> failwith "shouldn't happen -- get_displ"
  else
    None

(* mem operation of the binop: reg1 * {1,2,4,8} + reg2 +/- constant *)
let binop_mem_mult_add (m: Ir.expr) reg1 reg2 displ =
  match m with
  | Ir.Const 1L -> BaseOff (displ, reg2, reg1, One)
  | Ir.Const 2L -> BaseOff (displ, reg2, reg1, Two)
  | Ir.Const 4L -> BaseOff (displ, reg2, reg1, Four)
  | Ir.Const 8L -> BaseOff (displ, reg2, reg1, Eight)
  | _ -> failwith "cannot happen -- binop_mem_mult_add"

(* mem operation of the binop: reg * {1,2,3,4,5,8,9} +/- const *)
let binop_mem_mul (m: Ir.expr) reg1 displ =
  match m with
  | Ir.Const 1L -> Off (displ, reg1, One)
  | Ir.Const 2L -> Off (displ, reg1, Two)
  | Ir.Const 3L -> BaseOff (displ, reg1, reg1, Two)
  | Ir.Const 4L -> Off (displ, reg1, Four)
  | Ir.Const 5L -> BaseOff (displ, reg1, reg1, Four)
  | Ir.Const 8L -> Off (displ, reg1, Eight)
  | Ir.Const 9L -> BaseOff (displ, reg1, reg1, Eight)
  | _ -> failwith "cannot happen -- binop_mem_mul"

(* mem operation of the binop: reg1 + reg2 +/- const *)
let binop_mem_add reg1 reg2 displ =
  BaseOff (displ, reg1, reg2, One)

(* mem operation of the binop reg +/- const *)
let binop_mem_const_add reg1 displ =
  Base (displ, reg1)

let flip_op (op: Ir.binop_code) =
  match op with
  | LT -> Ir.GT
  | GT -> Ir.LT
  | LEQ -> Ir.GEQ
  | GEQ -> Ir.LEQ
  | _ -> failwith "shouldn't happen -- flip_op"

let rec chomp_binop
  ?(debug=false)
  (curr_ctx: func_context)
  (fcontexts: func_contexts)
  (e: Ir.expr)
  (dest: abstract_reg option) =

  let chomp_expr = chomp_expr ~debug in

  match e with
  (* lea cases with constants *)
  (* lea-case1: reg1 = reg1 * {1,2,4,8} + reg2 +/- const
    * multiply constant (1,2,4,8) is denoted as m
    * operation of constant is denoted as op
    * subtraction is only allowed when constant is on right hand side
    * e1 is expression that is being multiplied
    * e2 is expression that is being added
    * e3 is constant that is being added
  *)
  (* c + (r1 * {1,2,4,8} + r2) *)
  | BinOp ((Const x as e3), (ADD as op), BinOp (BinOp (e1, MUL, (Const (1L|2L|4L|8L) as m)), ADD, e2))
  | BinOp ((Const x as e3), (ADD as op), BinOp (BinOp ((Const (1L|2L|4L|8L) as m), MUL, e1), ADD, e2))
  (* c + (r2 + r1 * {1,2,4,8}) *)
  | BinOp ((Const x as e3), (ADD as op), BinOp (e2, ADD, BinOp (e1, MUL, (Const (1L|2L|4L|8L) as m))))
  | BinOp ((Const x as e3), (ADD as op), BinOp (e2, ADD, BinOp ((Const (1L|2L|4L|8L) as m), MUL, e1)))
  (* (r1 * {1,2,4,8} + r2) +/- c *)
  | BinOp (BinOp (BinOp (e1, MUL, (Const (1L|2L|4L|8L) as m)), ADD, e2), (ADD|SUB as op), (Const x as e3))
  | BinOp (BinOp (BinOp ((Const (1L|2L|4L|8L) as m), MUL, e1), ADD, e2), (ADD|SUB as op), (Const x as e3))
  (* (r2 + r1 * {1,2,4,8}) +/- c *)
  | BinOp (BinOp (e2, ADD, BinOp (e1, MUL, (Const (1L|2L|4L|8L) as m))), (ADD|SUB as op), (Const x as e3))
  | BinOp (BinOp (e2, ADD, BinOp ((Const (1L|2L|4L|8L) as m), MUL, e1)), (ADD|SUB as op), (Const x as e3))
  (* r2 + (r1 * {1,2,4,8} +/- c *)
  | BinOp (e2, ADD, BinOp (BinOp (e1, MUL, (Const (1L|2L|4L|8L) as m)), (ADD|SUB as op), (Const x as e3)))
  | BinOp (e2, ADD, BinOp (BinOp ((Const (1L|2L|4L|8L) as m), MUL, e1), (ADD|SUB as op), (Const x as e3)))
  (* r2 + (c + r1 * {1,2,4,8}) *)
  | BinOp (e2, ADD, BinOp ((Const x as e3), (ADD as op), BinOp (e1, MUL, (Const (1L|2L|4L|8L) as m))))
  | BinOp (e2, ADD, BinOp ((Const x as e3), (ADD as op), BinOp ((Const (1L|2L|4L|8L) as m), MUL, e1)))
  (* (r1 * {1,2,4,8} +/- c) + r2 *)
  | BinOp (BinOp (BinOp (e1, MUL, (Const (1L|2L|4L|8L) as m)), (ADD|SUB as op), (Const x as e3)), ADD, e2)
  | BinOp (BinOp (BinOp ((Const (1L|2L|4L|8L) as m), MUL, e1), (ADD|SUB as op), (Const x as e3)), ADD, e2)
  (* (c + r1 * {1,2,4,8}) + r2 *)
  | BinOp (BinOp ((Const x as e3), (ADD as op), BinOp (e1, MUL, (Const (1L|2L|4L|8L) as m))), ADD, e2)
  | BinOp (BinOp ((Const x as e3), (ADD as op), BinOp ((Const (1L|2L|4L|8L) as m), MUL, e1)), ADD, e2) ->
    begin
      let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
      let (reg2, asm2) = chomp_expr curr_ctx fcontexts e2 in
      let (reg3, asm3) = chomp_expr curr_ctx fcontexts e3 in
      let displ = get_displ op x in
      let binop_mem = binop_mem_mult_add m reg1 reg2 displ in
      let is_32 = min_int32 <= x && x <= max_int32 in
      match is_32, dest with
      | true, Some dest_reg ->
        (dest_reg, asm1 @ asm2 @ [leaq (Mem binop_mem) (Reg dest_reg)])
      | true, None ->
        (reg1, asm1 @ asm2 @ [leaq (Mem binop_mem) (Reg reg1)])
      | false, Some dest_reg ->
        (dest_reg, asm1 @ asm2 @ [leaq (Mem binop_mem) (Reg reg1)] @ asm3 @ (non_imm_binop op reg3 reg1 dest_reg))
      | false, None ->
        (reg1, asm1 @ asm2 @ [leaq (Mem binop_mem) (Reg reg1)] @ asm3 @ (non_imm_binop op reg3 reg1 reg1))
    end
  (* lea-case2: reg = reg * {1,2,3,4,5,8,9} +/ const
    * multiply constant (1,2,3,4,5,8,9) is denoted as m
    * operation of constant is denoted as op
    * subtraction is only allowed when constant is on right hand side
    * e1 is expression that is being multiplied
    * e2 is constant that is being added
  *)
  | BinOp (BinOp (e1, MUL, (Const (1L|2L|3L|4L|5L|8L|9L) as m)), (ADD|SUB as op), (Const x as e2))
  | BinOp (BinOp ((Const (1L|2L|3L|4L|5L|8L|9L) as m), MUL, e1), (ADD|SUB as op), (Const x as e2))
  | BinOp ((Const x as e2), (ADD as op), BinOp (e1, MUL, (Const (1L|2L|3L|4L|5L|8L|9L) as m)))
  | BinOp ((Const x as e2), (ADD as op), BinOp ((Const (1L|2L|3L|4L|5L|8L|9L) as m), MUL, e1)) ->
    begin
      let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
      let (reg2, asm2) = chomp_expr curr_ctx fcontexts e2 in
      let displ = get_displ op x in
      let binop_mem = binop_mem_mul m reg1 displ in
      let is_32 = min_int32 <= x && x <= max_int32 in
      match is_32, dest with
      | true, Some dest_reg ->
        (dest_reg, asm1 @ [leaq (Mem binop_mem) (Reg dest_reg)])
      | true, None ->
        (reg1, asm1 @ [leaq (Mem binop_mem) (Reg reg1)])
      | false, Some dest_reg ->
        (dest_reg, asm1 @ [leaq (Mem binop_mem) (Reg reg1)] @ asm2 @ (non_imm_binop op reg2 reg1 dest_reg))
      | false, None ->
        (reg1, asm1 @ [leaq (Mem binop_mem) (Reg reg1)] @ asm2 @ (non_imm_binop op reg2 reg1 reg1))
    end
  (* lea-case3: reg2 = reg1 + reg2 +/- const
    * operation of constant is denoted as op
    * subtraction is only allowed when constant is on right hand side
    * e1, e2 are expressions that are being added
    * e3 is constant that is being added
  *)
  | BinOp ((Const x as e3), (ADD as op), BinOp (e1, ADD, e2))
  | BinOp (BinOp (e1, ADD, e2), (ADD|SUB as op), (Const x as e3))
  | BinOp (e1, ADD, BinOp ((Const x as e3), (ADD as op), e2))
  | BinOp (BinOp ((Const x as e3), (ADD as op), e2), ADD, e1)
  | BinOp (e1, ADD, BinOp (e2, (ADD|SUB as op), (Const x as e3)))
  | BinOp (BinOp (e2, (ADD|SUB as op), (Const x as e3)), ADD, e1) ->
    begin
      let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
      let (reg2, asm2) = chomp_expr curr_ctx fcontexts e2 in
      let (reg3, asm3) = chomp_expr curr_ctx fcontexts e3 in
      let displ = get_displ op x in
      let binop_mem = binop_mem_add reg1 reg2 displ in
      let is_32 = min_int32 <= x && x <= max_int32 in
      match is_32, dest with
      | true, Some dest_reg ->
        (dest_reg, asm1 @ asm2 @ [leaq (Mem binop_mem) (Reg dest_reg)])
      | true, None ->
        (reg2, asm1 @ asm2 @ [leaq (Mem binop_mem) (Reg reg2)])
      | false, Some dest_reg ->
        (dest_reg,
          asm1 @ asm2 @ [leaq (Mem binop_mem) (Reg reg2)] @ asm3 @ (non_imm_binop op reg2 reg3 dest_reg))
      | false, None ->
        (reg3,
          asm1 @ asm2 @ [leaq (Mem binop_mem) (Reg reg2)] @ asm3 @ (non_imm_binop op reg2 reg3 reg3))
    end
  (* decr case *)
  | BinOp ((Temp reg as e1), SUB, Const 1L) ->
    begin
      match dest with
        | Some dest_reg when dest_reg <> (Fake reg) ->
          let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
          (dest_reg, asm1 @ imm_binop SUB 1L reg1 dest_reg)
        | _ -> ((Fake reg), [decq (Reg (Fake reg))])
    end
  | BinOp (e1, SUB, Const 1L) ->
    begin
      let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
      match dest with
        | Some dest_reg when dest_reg <> reg1 -> (dest_reg, asm1 @ imm_binop SUB 1L reg1 dest_reg)
        | _ -> (reg1, asm1 @ [decq (Reg reg1)])
    end
  (* incr cases *)
  | BinOp ((Temp reg as e1), ADD, Const 1L)
  | BinOp (Const 1L, ADD, (Temp reg as e1)) ->
    begin
      match dest with
        | Some dest_reg when dest_reg <> (Fake reg) ->
          let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
          (dest_reg, asm1 @ imm_binop ADD 1L reg1 dest_reg)
        | _ -> ((Fake reg), [incq (Reg (Fake reg))])
    end
  | BinOp (e1, ADD, Const 1L)
  | BinOp (Const 1L, ADD, e1) ->
    begin
      let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
      match dest with
        | Some dest_reg when dest_reg <> reg1 -> (dest_reg, asm1 @ imm_binop ADD 1L reg1 dest_reg)
        | _ -> (reg1, asm1 @ [incq (Reg reg1)])
    end
  (* lea-case4: reg = reg +/- const
    * operation of constant is denoted as op
    * subtraction is only allowed when constant is on right hand side
    * e1 is expression that is being added
    * e2 is constant that is being added
  *)
  | BinOp (e1, (ADD|SUB as op), (Const x as e2))
  | BinOp ((Const x as e2), (ADD as op), e1) ->
    begin
      let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
      let (reg2, asm2) = chomp_expr curr_ctx fcontexts e2 in
      let displ = get_displ op x in
      let binop_mem = binop_mem_const_add reg1 displ in
      let is_32 = min_int32 <= x && x <= max_int32 in
      match is_32, dest with
      | true, Some dest_reg ->
        (dest_reg, asm1 @ [leaq (Mem binop_mem) (Reg dest_reg)])
      | true, None ->
        (reg1, asm1 @ [leaq (Mem binop_mem) (Reg reg1)])
      | false, Some dest_reg ->
        (dest_reg, asm1 @ asm2 @ (non_imm_binop op reg2 reg1 dest_reg))
      | false, None ->
        (reg1, asm1 @ asm2 @ (non_imm_binop op reg2 reg1 reg1))
    end
  (* lea cases without constants *)
  (* lea-case5: reg1 = reg1 * {1,2,4,8} + reg2
    * multiply constant (1,2,4,8) is denoted as m
    * e1 is expression that is being multiplied
    * e2 is expression that is being added
  *)
  | BinOp (BinOp ((Const (1L|2L|4L|8L) as m), MUL, e1), ADD, e2)
  | BinOp (BinOp (e1, MUL, (Const (1L|2L|4L|8L) as m)), ADD, e2)
  | BinOp (e2, ADD, BinOp ((Const (1L|2L|4L|8L) as m), MUL, e1))
  | BinOp (e2, ADD, BinOp (e1, MUL, (Const (1L|2L|4L|8L) as m))) ->
    begin
      let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
      let (reg2, asm2) = chomp_expr curr_ctx fcontexts e2 in
      let binop_mem = binop_mem_mult_add m reg1 reg2 None in
      match dest with
        | Some dest_reg -> (dest_reg, asm1 @ asm2 @ [leaq (Mem binop_mem) (Reg dest_reg)])
        | None -> (reg1, asm1 @ asm2 @ [leaq (Mem binop_mem) (Reg reg1)])
    end
  (* lea-case6: reg1 = reg1 * {1,2,3,4,5,8,9}
    * multiply constant (1,2,3,4,5,8,9) is denoted as m
    * e1 is expression that is being multiplied
  *)
  | BinOp (e1, MUL, (Const (1L|2L|3L|4L|5L|8L|9L) as m))
  | BinOp ((Const (1L|2L|3L|4L|5L|8L|9L) as m), MUL, e1) ->
    begin
      let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
      let binop_mem = binop_mem_mul m reg1 None in
      match dest with
        | Some dest_reg -> (dest_reg, asm1 @ [leaq (Mem binop_mem) (Reg dest_reg)])
        | None -> (reg1, asm1 @ [leaq (Mem binop_mem) (Reg reg1)])
    end
  (* lea-case7: reg1 = reg1 + reg2
    * e1 is expression that is being multiplied
    * e2 is expression that is being added
  *)
  | BinOp (e1, ADD, e2) ->
    begin
      let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
      let (reg2, asm2) = chomp_expr curr_ctx fcontexts e2 in
      let binop_mem = binop_mem_add reg1 reg2 None in
      match dest with
        | Some dest_reg -> (dest_reg, asm1 @ asm2 @ [leaq (Mem binop_mem) (Reg dest_reg)])
        | None -> (reg1, asm1 @ asm2 @ [leaq (Mem binop_mem) (Reg reg1)])
    end
  (* mod 2 case *)
  | BinOp (BinOp (e1, MOD, Const 2L), EQ, Const 0L)
  | BinOp (Const 0L, EQ, BinOp (e1, MOD, Const 2L)) ->
    begin
      let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
      match dest with
      | Some dest_reg ->
          (dest_reg, asm1 @ [bt (Asm.Const 0L) (Reg reg1); asetnc (Reg (Real Cl));
            movq (Reg (Real Rcx)) (Reg dest_reg)])
      | None ->
          (reg1, asm1 @ [bt (Asm.Const 0L) (Reg reg1); asetnc (Reg (Real Cl));
            movq (Reg (Real Rcx)) (Reg reg1)])
    end
  | BinOp (BinOp (e1, MOD, Const 2L), EQ, Const 1L)
  | BinOp (Const 1L, EQ, BinOp (e1, MOD, Const 2L)) ->
    begin
      let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
      match dest with
      | Some dest_reg ->
          (dest_reg, asm1 @ [bt (Asm.Const 0L) (Reg reg1); asetc (Reg (Real Cl));
            movq (Reg (Real Rcx)) (Reg dest_reg)])
      | None ->
          (reg1, asm1 @ [bt (Asm.Const 0L) (Reg reg1); asetc (Reg (Real Cl));
            movq (Reg (Real Rcx)) (Reg reg1)])
    end
  (* neg case *)
  | BinOp (Const 0L, SUB, (Temp reg as e1)) ->
    begin
      match dest with
        | Some dest_reg when dest_reg <> (Fake reg) ->
          let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
          (dest_reg, asm1 @ imm_binop SUB 0L reg1 dest_reg)
        | _ -> ((Fake reg), [negq (Reg (Fake reg))])
    end
  | BinOp (Const 0L, SUB, e1) ->
    begin
      let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
      match dest with
        | Some dest_reg -> (dest_reg, asm1 @ imm_binop SUB 0L reg1 dest_reg)
        | _ -> (reg1, asm1 @ [negq (Reg reg1)])
    end
  (* comparisons with zero *)
  | BinOp (Temp reg, ((EQ|NEQ|LT|GT|LEQ|GEQ) as op), Const 0L)
  (* commutative operations *)
  | BinOp (Const 0L, ((EQ|NEQ) as op), Temp reg) ->
    begin
      let reg1 = Fake reg in
      match dest with
        | Some dest_reg -> (dest_reg, (cmp_zero op reg1 dest_reg))
        | None -> (reg1, (cmp_zero op reg1 reg1))
    end
  (* non-commutative operations *)
  | BinOp (Const 0L, ((LT|GT|LEQ|GEQ) as op), Temp reg) ->
    begin
      let flipped_op = flip_op op in
      let reg1 = Fake reg in
      match dest with
        | Some dest_reg -> (dest_reg, (cmp_zero flipped_op reg1 dest_reg))
        | None -> (reg1, (cmp_zero flipped_op reg1 reg1))
    end
  (* comparisons with zero *)
  | BinOp (e1, ((EQ|NEQ|LT|GT|LEQ|GEQ) as op), Const 0L)
  (* commutative operations *)
  | BinOp (Const 0L, ((EQ|NEQ) as op), e1) ->
    begin
      let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
      match dest with
        | Some dest_reg -> (dest_reg, asm1 @ (cmp_zero op reg1 dest_reg))
        | None -> (reg1, asm1 @ (cmp_zero op reg1 reg1))
    end
  (* non-commutative operations *)
  | BinOp (Const 0L, ((LT|GT|LEQ|GEQ) as op), e1) ->
    begin
      let flipped_op = flip_op op in
      let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
      match dest with
        | Some dest_reg -> (dest_reg, asm1 @ (cmp_zero flipped_op reg1 dest_reg))
        | None -> (reg1, asm1 @ (cmp_zero flipped_op reg1 reg1))
    end
  (* binop with immediate cases *)
  (* add and sub are not included b/c it is taken care of in lea cases *)
  | BinOp (Temp reg, ((AND|OR|XOR) as op), (Const x as e2))
  | BinOp ((Const x as e2), ((AND|OR|XOR) as op), Temp reg) ->
    begin
      let reg1 = Fake reg in
      let (reg2, asm2) = chomp_expr curr_ctx fcontexts e2 in
      (* checking if immidiate is within 32 bits *)
      let is_32 = min_int32 <= x && x <= max_int32 in
      match is_32, dest with
      | true, Some dest_reg ->
        (dest_reg, (imm_binop op x reg1 dest_reg))
      | true, None ->
        (reg1, (imm_binop op x reg1 reg1))
      | false, Some dest_reg ->
        (dest_reg, (non_imm_binop op reg1 reg2 dest_reg))
      | false, None ->
        (reg2, asm2 @ (non_imm_binop op reg1 reg2 reg2))
    end
  | BinOp (e1, ((AND|OR|XOR) as op), (Const x as e2))
  | BinOp ((Const x as e2), ((AND|OR|XOR) as op), e1) ->
    begin
      let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
      let (reg2, asm2) = chomp_expr curr_ctx fcontexts e2 in
      (* checking if immidiate is within 32 bits *)
      let is_32 = min_int32 <= x && x <= max_int32 in
      match is_32, dest with
      | true, Some dest_reg ->
        (dest_reg, asm1 @ (imm_binop op x reg1 dest_reg))
      | true, None ->
        (reg1, asm1 @ (imm_binop op x reg1 reg1))
      | false, Some dest_reg ->
        (dest_reg, asm1 @ (non_imm_binop op reg1 reg2 dest_reg))
      | false, None ->
        (reg2, asm1 @ asm2 @ (non_imm_binop op reg1 reg2 reg2))
    end
  | BinOp (Temp reg, ((LSHIFT|RSHIFT|ARSHIFT) as op), (Const x as e2)) ->
    begin
      let reg1 = Fake reg in
      let (reg2, asm2) = chomp_expr curr_ctx fcontexts e2 in
      (* checking if shift is within 8 bits *)
      let is_8 = Int64.neg(128L) <= x && x <= 128L in
      match is_8, dest with
      | true, Some dest_reg ->
        (dest_reg, (imm_shift op x reg1 dest_reg))
      | true, None ->
        (reg1, (imm_shift op x reg1 reg1))
      | false, Some dest_reg ->
        (dest_reg, (non_imm_shift op reg1 reg2 dest_reg))
      | false, None ->
        (reg1, asm2 @ (non_imm_shift op reg1 reg2 reg1))
    end
  | BinOp (e1, ((LSHIFT|RSHIFT|ARSHIFT) as op), (Const x as e2))
  | BinOp ((Const x as e2), ((LSHIFT|RSHIFT|ARSHIFT) as op), e1) ->
    begin
      let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
      let (reg2, asm2) = chomp_expr curr_ctx fcontexts e2 in
      (* checking if shift is within 8 bits *)
      let is_8 = Int64.neg(128L) <= x && x <= 128L in
      match is_8, dest with
      | true, Some dest_reg ->
        (dest_reg, asm1 @ (imm_shift op x reg1 dest_reg))
      | true, None ->
        (reg1, asm1 @ (imm_shift op x reg1 reg1))
      | false, Some dest_reg ->
        (dest_reg, asm1 @ (non_imm_shift op reg1 reg2 dest_reg))
      | false, None ->
        (reg1, asm1 @ asm2 @ (non_imm_shift op reg1 reg2 reg1))
    end
  | BinOp (Temp reg, ((EQ|NEQ|LT|GT|LEQ|GEQ) as op), (Const x as e2))
  (* commutative operations *)
  | BinOp ((Const x as e2), ((EQ|NEQ) as op), Temp reg) ->
    begin
      let reg1 = Fake reg in
      let (reg2, asm2) = chomp_expr curr_ctx fcontexts e2 in
      let is_32 = min_int32 <= x && x <= max_int32 in
      match is_32, dest with
      | true, Some dest_reg ->
        (dest_reg, (imm_cmp op x reg1 dest_reg))
      | true, None ->
        (reg1, (imm_cmp op x reg1 reg1))
      | false, Some dest_reg ->
        (dest_reg, asm2 @ (non_imm_cmp op reg1 reg2 dest_reg))
      | false, None ->
        (reg2, asm2 @ (non_imm_cmp op reg1 reg2 reg2))
    end
  (* non-commutative operations *)
  | BinOp ((Const x as e2), ((LT|GT|LEQ|GEQ) as op), Temp reg) ->
    begin
      let flipped_op = flip_op op in
      let reg1 = Fake reg in
      let (reg2, asm2) = chomp_expr curr_ctx fcontexts e2 in
      let is_32 = min_int32 <= x && x <= max_int32 in
      match is_32, dest with
      | true, Some dest_reg ->
        (dest_reg, (imm_cmp flipped_op x reg1 dest_reg))
      | true, None ->
        (reg1, (imm_cmp flipped_op x reg1 reg1))
      | false, Some dest_reg ->
        (dest_reg, asm2 @ (non_imm_cmp flipped_op reg1 reg2 dest_reg))
      | false, None ->
        (reg2, asm2 @ (non_imm_cmp flipped_op reg1 reg2 reg2))
    end
  | BinOp (e1, ((EQ|NEQ|LT|GT|LEQ|GEQ) as op), (Const x as e2))
  (* commutative operations *)
  | BinOp ((Const x as e2), ((EQ|NEQ) as op), e1) ->
    begin
      let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
      let (reg2, asm2) = chomp_expr curr_ctx fcontexts e2 in
      let is_32 = min_int32 <= x && x <= max_int32 in
      match is_32, dest with
      | true, Some dest_reg ->
        (dest_reg, asm1 @ (imm_cmp op x reg1 dest_reg))
      | true, None ->
        (reg1, asm1 @ (imm_cmp op x reg1 reg1))
      | false, Some dest_reg ->
        (dest_reg, asm1 @ asm2 @ (non_imm_cmp op reg1 reg2 dest_reg))
      | false, None ->
        (reg2, asm1 @ asm2 @ (non_imm_cmp op reg1 reg2 reg2))
    end
  (* non-commutative operations *)
  | BinOp ((Const x as e2), ((LT|GT|LEQ|GEQ) as op), e1) ->
    begin
      let flipped_op = flip_op op in
      let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
      let (reg2, asm2) = chomp_expr curr_ctx fcontexts e2 in
      let is_32 = min_int32 <= x && x <= max_int32 in
      match is_32, dest with
      | true, Some dest_reg ->
        (dest_reg, asm1 @ (imm_cmp flipped_op x reg1 dest_reg))
      | true, None ->
        (reg1, asm1 @ (imm_cmp flipped_op x reg1 reg1))
      | false, Some dest_reg ->
        (dest_reg, asm1 @ asm2 @ (non_imm_cmp flipped_op reg1 reg2 dest_reg))
      | false, None ->
        (reg2, asm1 @ asm2 @ (non_imm_cmp flipped_op reg1 reg2 reg2))
    end
  | BinOp (Temp reg1, opcode, Temp reg2) ->
    create_binop_instr opcode (Fake reg1) [] (Fake reg2) [] dest
  | BinOp (Temp reg1, opcode, e2) ->
    let (reg2, asm2) = chomp_expr curr_ctx fcontexts e2 in
    create_binop_instr opcode (Fake reg1) [] reg2 asm2 dest
  | BinOp (e1, opcode, Temp reg2) ->
    let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
    create_binop_instr opcode reg1 asm1 (Fake reg2) [] dest
  (* binop with non-immediate cases *)
  | BinOp (e1, opcode, e2) ->
    let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
    let (reg2, asm2) = chomp_expr curr_ctx fcontexts e2 in
    create_binop_instr opcode reg1 asm1 reg2 asm2 dest
  | _ -> failwith "shouldn't happen - chomp_binop curr_ctx fcontexts"

and chomp_expr
  ?(debug=false)
  (curr_ctx: func_context)
  (fcontexts: func_contexts)
  (e: Ir.expr) =

  let chomp_binop = chomp_binop ~debug in
  let munch_expr = munch_expr ~debug in

  match e with
  | BinOp _ -> chomp_binop curr_ctx fcontexts e None
  | (Const _ | Mem _ | Temp _| Call _| Name _| ESeq _) ->
    let (r, asm) = munch_expr curr_ctx fcontexts e in
    (Fake r, asm)

and chomp_stmt
    ?(debug=false)
    (curr_ctx: func_context)
    (fcontexts: func_contexts)
    (s: Ir.stmt) =

  let chomp_expr = chomp_expr ~debug in
  let chomp_stmt = chomp_stmt ~debug in

  match s with
  | CJumpOne (e1, tru) ->
    begin
      let tru_label = Asm.Label tru in
      match e1 with
      (* mod 2 case *)
      | BinOp (BinOp (e1, MOD, Const 2L), EQ, Const 0L)
      | BinOp (Const 0L, EQ, BinOp (e1, MOD, Const 2L)) ->
        let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
        asm1 @ [bt (Asm.Const 0L) (Reg reg1); jnc tru_label]
      | BinOp (BinOp (e1, MOD, Const 2L), EQ, Const 1L)
      | BinOp (Const 1L, EQ, BinOp (e1, MOD, Const 2L)) ->
        let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
        asm1 @ [bt (Asm.Const 0L) (Reg reg1); jc tru_label]
      (* comparing with 0 *)
      | BinOp (e1, ((EQ|NEQ|LT|GT|LEQ|GEQ) as op), Const 0L)
      | BinOp (Const 0L, ((EQ|NEQ) as op), e1) ->
        let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
        asm1 @ (cmp_zero_jump op reg1 tru_label)
      | BinOp (Const 0L, ((LT|GT|LEQ|GEQ) as op), e1) ->
        let flipped_op = flip_op op in
        let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
        asm1 @ (cmp_zero_jump flipped_op reg1 tru_label)
      (* comparing with constant *)
      | BinOp (e1, ((EQ|NEQ|LT|GT|LEQ|GEQ) as op), (Const x as e2))
      | BinOp ((Const x as e2), ((EQ|NEQ) as op), e1) ->
        let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
        let (reg2, asm2) = chomp_expr curr_ctx fcontexts e2 in
        if min_int32 <= x && x <= max_int32 then
          asm1 @ (imm_cmp_jump op reg1 x tru_label)
        else
          asm1 @ asm2 @ (non_imm_cmp_jump op reg1 reg2 tru_label)
      | BinOp ((Const x as e2), ((LT|GT|LEQ|GEQ) as op), e1) ->
        let flipped_op = flip_op op in
        let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
        let (reg2, asm2) = chomp_expr curr_ctx fcontexts e2 in
        if min_int32 <= x && x <= max_int32 then
          asm1 @ (imm_cmp_jump flipped_op reg1 x tru_label)
        else
          asm1 @ asm2 @ (non_imm_cmp_jump flipped_op reg1 reg2 tru_label)
      | BinOp (e1, ((EQ|NEQ|LT|GT|LEQ|GEQ) as op), e2) ->
        let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
        let (reg2, asm2) = chomp_expr curr_ctx fcontexts e2 in
        asm1 @ asm2 @ (non_imm_cmp_jump op reg1 reg2 tru_label)
      | _ ->
        let (binop_reg, binop_lst) = chomp_expr curr_ctx fcontexts e1 in
        let tru_label = Asm.Label tru in
        let jump_lst = [
          cmpq (Const 0L) (Reg binop_reg);
          jnz tru_label;
        ] in
        binop_lst @ jump_lst
    end
  | Exp e -> snd (chomp_expr curr_ctx fcontexts e)
  | Move (Temp n, (BinOp _ as e)) ->
    begin
      let dest =
        (* moving return values to _RETi before returning *)
        match FreshRetReg.get n with
        | Some i -> Asm.callee_ret_op ret_ptr_reg i
        | None -> Reg (Fake n)
      in
      match dest with
      | Reg r -> snd (chomp_binop curr_ctx fcontexts e (Some r))
      | Mem _ ->
        let (reg, asm) = chomp_binop curr_ctx fcontexts e None in
        asm @ [movq (Reg reg) dest]
      | _ -> failwith "cannot happen Move (Temp n, (BinOp _ as e))"
    end
   | Move (Temp n, e) ->
    begin
      let dest =
        (* moving return values to _RETi before returning *)
        match FreshRetReg.get n with
        | Some i -> Asm.callee_ret_op ret_ptr_reg i
        | None -> Reg (Fake n)
      in
      let (e_reg, e_lst) = chomp_expr curr_ctx fcontexts e in
      e_lst @ [movq (Reg e_reg) dest]
    end
  | Move (Mem (e1, _), e2) ->
    let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
    let (reg2, asm2) = chomp_expr curr_ctx fcontexts e2 in
    asm1 @ asm2 @ [movq (Reg reg2) (Mem (Base (None, reg1)))]
  | Seq s_list -> List.map ~f:(chomp_stmt curr_ctx fcontexts) s_list |> List.concat
  | (Label _ | Return| Move _| Jump _| CJump _) -> munch_stmt curr_ctx fcontexts s

let chomp_func_decl
  ?(debug=false)
  (fcontexts: func_contexts)
  (func_decl: Ir.func_decl) =
  eat_func_decl chomp_stmt ~debug fcontexts func_decl

let chomp_comp_unit
    ?(debug=false)
    (fcontexts: func_contexts)
    (comp_unit: Ir.comp_unit) =
  eat_comp_unit chomp_func_decl ~debug fcontexts comp_unit

let asm_eat
  (eat_comp_unit: (Ir.comp_unit, Asm.abstract_asm list list) without_fcontext)
  ?(debug=false)
  (FullProg (_, _, interfaces): Typecheck.full_prog)
  (comp_unit : Ir.comp_unit) : Asm.asm list =
  let callable_decls =
    let f acc (_, Ast.S.Interface cdlist) = cdlist @ acc in
    List.fold_left ~f ~init:[] interfaces in
  let func_contexts = get_context_map callable_decls comp_unit in
  let munched = eat_comp_unit ~debug func_contexts comp_unit in
  List.concat_map ~f:(register_allocate ~debug) munched

let asm_munch
  ?(debug=false)
  (full_prog: Typecheck.full_prog)
  (comp_unit : Ir.comp_unit) : Asm.asm list =
  asm_eat munch_comp_unit ~debug full_prog comp_unit

let asm_chomp
  ?(debug=false)
  (full_prog: Typecheck.full_prog)
  (comp_unit : Ir.comp_unit) : Asm.asm list =
  asm_eat chomp_comp_unit ~debug full_prog comp_unit

