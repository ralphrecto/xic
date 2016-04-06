open Core.Std
open Asm
open Func_context
open Ir_generation
open Typecheck

module FreshReg   = Fresh.Make(struct let name = "_asmreg" end)
module FreshLabel = Fresh.Make(struct let name = "_asmlabel" end)
let ret_ptr_reg   = Fake "_asmretptr"

let max_int32 = Int64.of_int32_exn (Int32.max_value)
let min_int32 = Int64.of_int32_exn (Int32.min_value)

let binop_to_instr (op: Ir.binop_code) =
  match op with
  | Ir.ADD -> addq
  | Ir.SUB -> subq
  | Ir.AND -> andq
  | Ir.OR -> orq
  | Ir.XOR -> xorq
  | _ -> failwith "shouldn't happen -- binop_to_instr"

let cmp_to_instr (op: Ir.binop_code) =
  match op with
  | Ir.EQ -> sete
  | Ir.NEQ -> setne
  | Ir.LT -> setl
  | Ir.GT -> setg
  | Ir.LEQ -> setle
  | Ir.GEQ -> setge
  | _ -> failwith "shouldn't happen -- cmp_to_instr"

let cmp_zero_to_instr (op: Ir.binop_code) =
  let open Ir in
  match op with
  | EQ -> setz
  | NEQ -> setnz
  | LT -> sets
  | GT -> setg
  | LEQ -> setle
  | GEQ -> setns
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
  [cmpq (Reg reg1) (Reg reg2); (cmp_to_instr op) (Reg dest)]

let cmp_zero op reg1 dest =
  [test (Reg reg1) (Reg reg1); (cmp_zero_to_instr op) (Reg dest)]

let imm_cmp op const reg dest =
  [cmpq (Asm.Const const) (Reg reg); (cmp_to_instr op) (Reg dest)]

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

let non_imm_binop op reg1 reg2 dest =
  if reg2 = dest then 
    [(binop_to_instr op) (Reg reg1) (Reg reg2)]
  else if reg1 = dest then 
    [(binop_to_instr op) (Reg reg2) (Reg reg1)]
  else
    [(binop_to_instr op) (Reg reg1) (Reg reg2); movq (Reg reg2) (Reg dest)]

let rec munch_expr
  (curr_ctx: func_context)
  (fcontexts: func_contexts)
  (e: Ir.expr) =
  match e with
  | Ir.BinOp (e1, opcode, e2) ->
    begin
      let (reg1, asm1) = munch_expr curr_ctx fcontexts e1 in
      let (reg2, asm2) = munch_expr curr_ctx fcontexts e2 in
      match opcode with
      | Ir.ADD | Ir.SUB | Ir.AND | Ir.OR | Ir.XOR ->
        (reg2, asm1 @ asm2 @ (non_imm_binop opcode reg1 reg2 reg2))
      | Ir.LSHIFT | Ir.RSHIFT | Ir.ARSHIFT ->
        (reg1, asm1 @ asm2 @ (non_imm_shift opcode reg1 reg2 reg1))
      | Ir.EQ | Ir.NEQ | Ir.LT | Ir.GT | Ir.LEQ | Ir.GEQ ->
        (reg2, asm1 @ asm2 @ (non_imm_cmp opcode reg1 reg2 reg2))
      | Ir.MUL | Ir.HMUL ->
        let mul_asm = [
          movq (Reg reg2) (Reg (Real Rax));
          imulq (Reg reg1);
        ] in
        let r = if opcode = Ir.MUL then Rax else Rdx in
        (Real r, asm1 @ asm2 @ mul_asm)
      | Ir.DIV | Ir.MOD ->
        let div_asm = [
          movq (Reg reg1) (Reg (Real Rax));
          idivq (Reg reg2);
        ] in
        let r = if opcode = Ir.DIV then Rax else Rdx in
        (Real r, asm1 @ asm2 @ div_asm)
    end
  | Ir.Const c ->
      let new_tmp = FreshReg.fresh () in
      (Fake new_tmp, [mov (Asm.Const c) (Reg (Fake new_tmp))])
  | Ir.Mem (e, _) ->
      let (e_reg, e_asm) = munch_expr curr_ctx fcontexts e in
      let new_tmp = FreshReg.fresh () in
      (Fake new_tmp, e_asm @ [mov (Mem (Base (None, e_reg))) (Reg (Fake new_tmp))])
  | Ir.Temp str -> begin
      let new_tmp = Fake (FreshReg.fresh ()) in
      match FreshRetReg.get str with
      (* moving rets from callee return *)
      | Some i ->
          let retmov =
            if i < 2 then
               movq (Reg (Real (ret_reg i))) (Reg new_tmp)
            else
               movq (Mem ((i-2+curr_ctx.max_args)$(Real Rsp))) (Reg new_tmp) in
          (new_tmp, [retmov])
      | None -> begin
          (* moving passed arguments as callee into vars *)
          match FreshArgReg.get str with
          | Some i ->
              let i' = (max 0 (curr_ctx.num_rets - 2)) + i in
              let argmov =
                if i' < 2 then
                   movq (Reg (Real (arg_reg i'))) (Reg new_tmp)
                else
                  (* +1 to skip rip *)
                  movq (Mem ((i'-6+1)$(Real Rbp))) (Reg new_tmp) in
              (new_tmp, [argmov])
          | None ->
              let fresh = FreshReg.fresh () in
              (Fake fresh, [mov (Reg (Fake str)) (Reg (Fake fresh))])
      end
  end
  | Ir.Call (Ir.Name (fname), arglist) ->
      let callee_ctx = String.Map.find_exn fcontexts fname in
      let (arg_regs, arg_asms) =
        List.unzip (List.map ~f:(munch_expr curr_ctx fcontexts) arglist) in
      let (ret_reg, ret_asm) =
        if callee_ctx.num_rets - 2 > 0 then
          let new_tmp = Fake (FreshReg.fresh ()) in
          let asm = leaq (Mem ((curr_ctx.max_args)$(Real Rsp))) (Reg new_tmp) in
          ([new_tmp], [asm])
        else ([], []) in
      let f i argsrc =
        let dest =
          if i < 6 then Reg (Real (arg_reg i))
          else Mem ((i-6)$(Real Rsp)) in
        movq (Reg argsrc) dest in
      let mov_asms = List.mapi ~f (ret_reg @ arg_regs) in
      (Real Rax, (List.concat arg_asms) @ ret_asm @ mov_asms @ [call (Label fname)])
  | Ir.Name _ -> failwith "Name should never be munched by itself"
  | Ir.Call _ -> failwith "Call should always have a Name first"
  | Ir.ESeq _ -> failwith "ESeq shouldn't exist"

and munch_stmt
    (curr_ctx: func_context)
    (fcontexts: func_contexts)
    (s: Ir.stmt) =
  match s with
  | Ir.CJumpOne (e1, tru) ->
    begin
      match e1 with
      | Ir.BinOp (e1, ((Ir.EQ|Ir.NEQ|Ir.LT|Ir.GT|Ir.LEQ|Ir.GEQ) as op), e2) ->
        let tru_label = Asm.Label tru in
        let cond_jump = cmp_to_jump_instr op tru_label in
        let (e1_reg, e1_lst) = munch_expr curr_ctx fcontexts e1 in
        let (e2_reg, e2_lst) = munch_expr curr_ctx fcontexts e2 in
        let jump_lst = [
          cmpq (Reg e2_reg) (Reg e1_reg);
          cond_jump;
        ] in
        e1_lst @ e2_lst @ jump_lst
      | _ ->
        let (binop_reg, binop_lst) = munch_expr curr_ctx fcontexts e1 in
        let tru_label = Asm.Label tru in
        let jump_lst = [
          cmpq (Const 0L) (Reg binop_reg);
          jnz tru_label;
        ] in
        binop_lst @ jump_lst
    end
  | Ir.Jump (Ir.Name s) -> [jmp (Asm.Label s)]
  | Ir.Exp e -> snd (munch_expr curr_ctx fcontexts e)
  | Ir.Label l -> [label_op l]
  | Ir.Move (Ir.Temp n, e) -> 
    let dest =
      (* moving return values to _RETi before returning *)
      match FreshRetReg.get n with
      | Some i ->
        if i < 2 then Reg (Real (ret_reg i))
        else Mem ((i-2)$(ret_ptr_reg))
      | None -> Reg (Fake n) 
    in
    let (e_reg, e_lst) = munch_expr curr_ctx fcontexts e in
    e_lst @ [movq (Reg e_reg) dest]
  | Ir.Move (Ir.Mem (e1, _), e2) ->
    let (e1_reg, e1_lst) = munch_expr curr_ctx fcontexts e1 in
    let (e2_reg, e2_lst) = munch_expr curr_ctx fcontexts e2 in
    e1_lst @ e2_lst @ [movq (Reg e2_reg) (Mem (Base (None, e1_reg)))]
  | Ir.Seq s_list -> List.map ~f:(munch_stmt curr_ctx fcontexts) s_list |> List.concat
  | Ir.Return -> [leave; ret]
  | Ir.Move _ -> failwith "Move has a non TEMP/MEM lhs"
  | Ir.Jump _ -> failwith "jump to a non label shouldn't exist"
  | Ir.CJump _ -> failwith "cjump shouldn't exist"

and munch_func_decl
    (fcontexts: func_contexts)
    ((fname, stmt, _): Ir.func_decl) =

  let curr_ctx = String.Map.find_exn fcontexts fname in
  let body_asm = munch_stmt curr_ctx fcontexts stmt in
  let num_temps = List.length (fakes_of_asms body_asm) in

  let label = [Lab fname] in
  let directives = [globl fname; align 4] in

  (* Building function prologue
   *  - use enter to save rbp, update rbp/rsp,
   *      allocate num_temps + num_caller_save words on stack
   *  - Align stack if not aligned to 16 bytes. To maintain this,
   *      we maintain the invariant that all stackframes have
   *      16k bytes for some k\in N. This should work if the
   *      bottom of the stack is a multiple of 16.
   *  - allocate space for tuple returns + argument passing
   *  - move passed mult. ret pointers into fresh temps
  *)
  let tot_temps = num_temps + num_caller_save in
  let tot_rets_n_args = curr_ctx.max_rets + curr_ctx.max_args in
  (* saved rip + saved rbp + temps + rets n args  *)
  let tot_stack_size = 1 + 1 + tot_temps + tot_rets_n_args in
  let prologue =
    let init = [enter (const tot_temps) (const 0)] in
    let padding =
      if tot_stack_size mod 2 = 0 then []
      else [pushq (const 0)] in
    let rets_n_args =
      [subq (const (tot_rets_n_args * 8)) (Reg (Real Rsp))] in
    let ret_ptr_mov =
      if curr_ctx.num_rets - 2 < 1 then []
      else [movq (Reg (Real (arg_reg 0))) (Reg ret_ptr_reg)] in
    init @ padding @ rets_n_args @ ret_ptr_mov in

  directives @ label @ prologue @ body_asm

and munch_comp_unit
    (fcontexts: func_contexts)
    ((_, func_decls): Ir.comp_unit) =
  let decl_list = String.Map.data func_decls in
  let fun_asm = List.concat_map ~f:(munch_func_decl fcontexts) decl_list in
  let directives = [text] in
  directives @ fun_asm

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

let rec chomp_binop 
  (curr_ctx: func_context)
  (fcontexts: func_contexts)
  (e: Ir.expr) 
  (dest: abstract_reg option) =   
  match e with
  (* mod 2 case *)
  | BinOp (BinOp (e1, MOD, Const 2L), EQ, Const 0L)
  | BinOp (Const 0L, EQ, BinOp (e1, MOD, Const 2L)) ->
    begin
      let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
      match dest with
      | Some dest_reg -> (dest_reg, asm1 @ [bt (Asm.Const 0L) (Reg reg1); setnc (Reg dest_reg)])
      | None -> (reg1, asm1 @ [bt (Asm.Const 0L) (Reg reg1); setnc (Reg reg1)])
    end
  | BinOp (BinOp (e1, MOD, Const 2L), EQ, Const 1L)
  | BinOp (Const 1L, EQ, BinOp (e1, MOD, Const 2L)) ->
    begin
      let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
      match dest with
      | Some dest_reg -> (dest_reg, asm1 @ [bt (Asm.Const 0L) (Reg reg1); setc (Reg dest_reg)])
      | None -> (reg1, asm1 @ [bt (Asm.Const 0L) (Reg reg1); setc (Reg reg1)])
    end
  (* neg case *)
  | BinOp (Const 0L, SUB, e1) ->
    begin
      let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
      match dest with
        | Some dest_reg when dest_reg <> reg1 -> (dest_reg, asm1 @ imm_binop SUB 0L reg1 dest_reg)
        | _ -> (reg1, asm1 @ [negq (Reg reg1)])  
    end
  (* incr cases *)
  | BinOp (e1, ADD, Const 1L)
  | BinOp (Const 1L, ADD, e1) ->
    begin
      let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
      match dest with
        | Some dest_reg when dest_reg <> reg1 -> (dest_reg, asm1 @ imm_binop ADD 1L reg1 dest_reg) 
        | _ -> (reg1, asm1 @ [incq (Reg reg1)])
    end
  (* decr case *)
  | BinOp (e1, SUB, Const 1L) ->
    begin
      let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
      match dest with
        | Some dest_reg when dest_reg <> reg1 -> (dest_reg, asm1 @ imm_binop SUB 1L reg1 dest_reg) 
        | _ -> (reg1, asm1 @ [decq (Reg reg1)])
    end
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
  (* comparisons with zeros *)
  | BinOp (e1, ((EQ|NEQ|LT|GT|LEQ|GEQ) as op), Const 0L)
  | BinOp (Const 0L, ((EQ|NEQ|LT|GT|LEQ|GEQ) as op), e1) ->
    begin
      let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
      match dest with 
        | Some dest_reg -> (dest_reg, asm1 @ (cmp_zero op reg1 dest_reg))
        | None -> (reg1, asm1 @ (cmp_zero op reg1 reg1))
    end
  (* binop with immediate cases *)
  (* add and sub are not included b/c it is taken care of in lea cases *)
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
  | BinOp (e1, ((EQ|NEQ|LT|GT|LEQ|GEQ) as op), (Const x as e2))
  | BinOp ((Const x as e2), ((EQ|NEQ|LT|GT|LEQ|GEQ) as op), e1) ->
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
  (* binop with non-immediate cases *)
  | BinOp (e1, opcode, e2) ->
    begin
      let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
      let (reg2, asm2) = chomp_expr curr_ctx fcontexts e2 in
      match opcode with
      | ADD | SUB | AND | OR | XOR ->
        begin
          match dest with
          | Some dest_reg -> (dest_reg, asm1 @ asm2 @ (non_imm_binop opcode reg1 reg2 dest_reg)) 
          | None -> (reg2, asm1 @ asm2 @ (non_imm_binop opcode reg1 reg2 reg2))
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
            let r = if opcode = MUL then Rax else Rdx in
            let mul_asm = [movq (Reg reg2) (Reg (Real Rax)); imulq (Reg reg1)] in
            (Real r, asm1 @ asm2 @ mul_asm)
        end
      | DIV | MOD ->
        begin
          match dest with
          | Some dest_reg ->
            let r = if opcode = DIV then Real Rax else Real Rdx in
            let div_asm = [movq (Reg reg1) (Reg (Real Rax)); idivq (Reg reg2); movq (Reg r) (Reg dest_reg)] in
            (dest_reg, asm1 @ asm2 @ div_asm)
          | None ->
            let r = if opcode = DIV then Rax else Rdx in
            let div_asm = [movq (Reg reg1) (Reg (Real Rax)); idivq (Reg reg2)] in
            (Real r, asm1 @ asm2 @ div_asm)
        end
    end
  | _ -> failwith "shouldn't happen - chomp_binop curr_ctx fcontexts"

and chomp_expr 
  (curr_ctx: func_context)
  (fcontexts: func_contexts)
  (e: Ir.expr) =
  match e with
  | BinOp _ -> chomp_binop curr_ctx fcontexts e None
  | (Const _ | Mem _ | Temp _| Call _| Name _| ESeq _) -> munch_expr curr_ctx fcontexts e

and chomp_stmt
    (curr_ctx: func_context)
    (fcontexts: func_contexts)
    (s: Ir.stmt) =
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
      | BinOp (Const 0L, ((EQ|NEQ|LT|GT|LEQ|GEQ) as op), e1) ->
        let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
        asm1 @ (cmp_zero_jump op reg1 tru_label)
      (* comparing with constant *)
      | BinOp (e1, ((EQ|NEQ|LT|GT|LEQ|GEQ) as op), (Const x as e2))
      | BinOp ((Const x as e2), ((EQ|NEQ|LT|GT|LEQ|GEQ) as op), e1) ->
        let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
        let (reg2, asm2) = chomp_expr curr_ctx fcontexts e2 in
        if min_int32 <= x && x <= max_int32 then
          asm1 @ (imm_cmp_jump op reg1 x tru_label)
        else
          asm1 @ asm2 @ (non_imm_cmp_jump op reg1 reg2 tru_label)
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
  | Jump (Name s) -> [jmp (Asm.Label s)]
  | Exp e -> snd (chomp_expr curr_ctx fcontexts e)
  | Label l -> [label_op l]
  | Move (Temp n, (BinOp _ as e)) ->
    begin
      let dest =
        (* moving return values to _RETi before returning *)
        match FreshRetReg.get n with
        | Some i ->
            if i < 2 then Reg (Real (ret_reg i))
            else Mem ((i-2)$(ret_ptr_reg))
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
    let dest =
      (* moving return values to _RETi before returning *)
      match FreshRetReg.get n with
      | Some i ->
          if i < 2 then Reg (Real (ret_reg i))
          else Mem ((i-2)$(ret_ptr_reg))
      | None -> Reg (Fake n) 
    in
    let (e_reg, e_lst) = chomp_expr curr_ctx fcontexts e in
    e_lst @ [movq (Reg e_reg) dest]
  | Move (Mem (e1, _), e2) ->
    let (reg1, asm1) = chomp_expr curr_ctx fcontexts e1 in
    let (reg2, asm2) = chomp_expr curr_ctx fcontexts e2 in
    asm1 @ asm2 @ [movq (Reg reg2) (Mem (Base (None, reg1)))]
  | Seq s_list -> List.map ~f:(chomp_stmt curr_ctx fcontexts) s_list |> List.concat
  | Return -> [leave; ret]
  | Move _ -> failwith "Move has a non TEMP/MEM lhs"
  | Jump _ -> failwith "jump to a non label shouldn't exist"
  | CJump _ -> failwith "cjump shouldn't exist"

let register_allocate asms =
  (* spill_env maps each fake name to an index, starting at 1, into the stack.
   * For example, if the fake name "foo" is mapped to n in spill_env, then Reg
   * (Fake "foo") will be spilled to -8n(%rbp). *)
  let spill_env =
    fakes_of_asms asms
    |> List.mapi ~f:(fun i asm -> (asm, i + 1))
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

      let pre = List.map fakes ~f:(fun fake -> mov (spill fake) (fake_to_op fake)) in
      let translation = [abstract_reg_map (translate_reg reg_env) asm] in
      let post = List.map fakes ~f:(fun fake -> mov (fake_to_op fake) (spill fake)) in
      pre @ translation @ post
    | Lab l -> [Lab l]
    | Directive (d, args) -> [Directive (d, args)]
  in

  List.concat_map ~f:(allocate spill_env) asms
