module Long = Int64
open Core.Std
open Cfg
open Dataflow
open Ir
open Tiling
open Fresh

module CcpLattice = struct
  type reachable = Reach | Unreach
  type defined = Undef | Def of Int64.t | Overdef
  type data = (reachable * defined String.Map.t)

  let ( ** ) (reach1, defined1) (reach2, defined2) =
    let reachable_meet r1 r2 =
      match r1, r2 with
      | Unreach, r | r, Unreach -> r
      | _ -> Reach
    in

    let defined_meet def1 def2 =
      match def1, def2 with
      | Undef, def
      | def, Undef -> def
      | Def i1, Def i2 when Int64.equal i1 i2 -> def1
      | Def _, Def _
      | Overdef, _
      | _, Overdef -> Overdef
    in

    let f ~key:temp ~data:def1 acc =
      let def2 = String.Map.find_exn defined2 temp in
      let new_def = defined_meet def1 def2 in
      String.Map.add acc ~key:temp ~data:new_def
    in
    let new_defined = String.Map.fold ~f ~init:String.Map.empty defined1 in
    let new_reach = reachable_meet reach1 reach2 in
    (new_reach, new_defined )

  let ( === ) (reach1, defined1) (reach2, defined2) =
    let def_equal def1 def2 =
      match def1, def2 with
      | Undef, Undef -> true
      | Def i1, Def i2 -> Int64.equal i1 i2
      | Overdef, Overdef -> true
      | _ -> false
    in
    match reach1, reach2 with
    | Reach, Reach
    | Unreach, Unreach -> String.Map.equal def_equal defined1 defined2
    | _ -> false

  let to_string (reach, defined) =
    let def_to_string = function
      | Undef -> "undef"
      | Def i -> "def " ^ (Int64.to_string i)
      | Overdef -> "overdef"
    in
    let f ~key:temp ~data:def acc =
      acc ^ (Printf.sprintf "%s: %s," temp (def_to_string def))
    in
    let defined_string = String.Map.fold ~f ~init:"" defined in
    match reach with
    | Reach -> Printf.sprintf "(reach, [%s])" defined_string
    | Unreach -> Printf.sprintf "(unreach, [%s])" defined_string
end

let rec get_temp_expr (e: expr) : String.Set.t =
  let open String.Set in
  match e with
  | BinOp (e1, _, e2) -> union (get_temp_expr e1) (get_temp_expr e2)
  | Call (_, elst) ->
    let f acc e = union acc (get_temp_expr e) in
    List.fold_left ~f ~init: empty elst
  | Mem (e1, _) -> get_temp_expr e1
  | Temp s -> add empty s
  | Const _ | Name _ -> empty
  | ESeq _ -> failwith "shouldn't exist!"

let rec get_temp_stmt (s: stmt) : String.Set.t =
  let open String.Set in
  match s with
  | CJumpOne (e1, _) -> get_temp_expr e1
  | Exp e1 -> get_temp_expr e1
  | Move (e1, e2) -> union (get_temp_expr e1) (get_temp_expr e2)
  | Seq slst ->
      let f acc s = union acc (get_temp_stmt s) in
      List.fold_left ~f ~init: empty slst
  | Jump _ | Label _ | Return -> empty
  | CJump _ -> failwith "shouldn't exist!"

let get_all_temps g =
  let module I = IrDataStartExit in
  let module C = IrCfg in
  let f e acc =
    match e with
    | I.Start
    | I.Exit -> acc
    | I.Node {ir; _} -> String.Set.union acc (get_temp_stmt ir)
  in
  C.fold_vertex f g String.Set.empty

let map_temp_undef temps =
  let module L = CcpLattice in
  let f acc e = String.Map.add acc ~key:e ~data:L.Undef in
  String.Set.fold ~f ~init:String.Map.empty temps

let eval_binop (i1: Int64.t) (op: Ir.binop_code) (i2: Int64.t) =
  let open Long in
  let open Big_int in
  let ( + ) = Long.add in
  let ( - ) = Long.sub in
  let ( * ) = Long.mul in
  let ( / ) = Long.div in
  let ( % ) = Long.rem in
  match op with
  | ADD -> Const (i1 + i2)
  | SUB -> Const (i1 - i2)
  | MUL -> Const (i1 * i2)
  | HMUL ->
    let i1' = big_int_of_int64 i1 in
    let i2' = big_int_of_int64 i2 in
    let mult = mult_big_int i1' i2' in
    let shifted = shift_right_big_int mult 64 in
    let result = int64_of_big_int shifted in
    Const result
  | DIV -> if compare i2 0L = 0
             then BinOp (Const i1, op, Const i2)
             else Const (i1 / i2)
  | MOD -> if compare i2 0L = 0
             then BinOp (Const i1, op, Const i2)
             else Const (i1 % i2)
  | AND -> Const (logand i1 i2)
  | OR -> Const (logor i1 i2)
  | XOR -> Const (logxor i1 i2)
  | LSHIFT ->
    let i2' = to_int i2 in
    Const (shift_left i1 i2')
  | RSHIFT ->
    let i2' = to_int i2 in
    Const (shift_right_logical i1 i2')
  | ARSHIFT ->
    let i2' = to_int i2 in
    Const (shift_right i1 i2')
  | EQ -> if (compare i1 i2) = 0 then Const 1L else Const 0L
  | NEQ -> if (compare i1 i2) <> 0 then Const 1L else Const 0L
  | LT -> if (compare i1 i2) < 0 then Const 1L else Const 0L
  | GT -> if (compare i1 i2) > 0 then Const 1L else Const 0L
  | LEQ -> if (compare i1 i2) <= 0 then Const 1L else Const 0L
  | GEQ -> if (compare i1 i2) >= 0 then Const 1L else Const 0L

let rec eval_expr (mapping: CcpLattice.defined String.Map.t) (e: expr) : CcpLattice.defined =
  let open CcpLattice in
  match e with
  | BinOp (e1, op, e2) ->
    begin
      match eval_expr mapping e1, eval_expr mapping e2 with
      | Overdef, _
      | _, Overdef -> Overdef
      | Undef, _
      | _, Undef -> Undef
      | Def i1, Def i2 -> begin
        match eval_binop i1 op i2 with
        | Const i -> Def i
        | _ -> Overdef
      end
    end
  | Temp s -> String.Map.find_exn mapping s
  | Const i -> Def i
  | Call _
  | Mem _
  | Name _ -> Overdef
  | ESeq _ -> failwith "shouldn't exist!"

let eval_stmt mapping s l =
  let open CcpLattice in
  match s with
  | Move (Temp x, e1) -> (Reach, String.Map.add mapping ~key:x ~data:(eval_expr mapping e1))
  | Move _ -> (Reach, mapping)
  | CJumpOne (e1, _) ->
    begin
      match eval_expr mapping e1, l with
      | Undef, _
      | Overdef, _ -> (Reach, mapping)
      | Def 1L, Cfg.EdgeData.True -> (Reach, mapping)
      | Def 1L, Cfg.EdgeData.False -> (Unreach, mapping)
      | Def 0L, Cfg.EdgeData.True -> (Unreach, mapping)
      | Def 0L, Cfg.EdgeData.False -> (Reach, mapping)
      | _ -> failwith "can't happen"
    end
  | Exp _ -> (Reach, mapping)
  | Jump _ | Label _ ->  (Reach, mapping)
  | Return -> (Unreach, mapping)
  | Seq _
  | CJump _ -> failwith "shouldn't exist!"

let rec subst_expr mapping e =
  let open CcpLattice in
  match e with
  | BinOp (e1, op, e2) ->
    begin
      match subst_expr mapping e1, subst_expr mapping e2 with
      | Const i1, Const i2 -> eval_binop i1 op i2
      | Const i1, _ -> BinOp (Const i1, op, e2)
      | _, Const i2 -> BinOp (e1, op, Const i2)
      | _ -> e
    end
  | Call (n, elst) -> Call (n, List.map ~f:(subst_expr mapping) elst)
  | Mem (e1, t) -> Mem (subst_expr mapping e1, t)
  | Temp s ->
    begin
      match String.Map.find_exn mapping s with
      | Def i -> Const i
      | _ -> e
    end
  | Const _
  | Name _ -> e
  | ESeq _ -> failwith "shouldn't exist!"

let subst_stmt mapping s =
  let open CcpLattice in
  match s with
  | Move (Temp s1, e1) -> Move (Temp s1, subst_expr mapping e1)
  | Move (Mem (e1, t), e2) -> Move (Mem (subst_expr mapping e1, t), subst_expr mapping e2)
  | CJumpOne (e1, s1) -> CJumpOne (subst_expr mapping e1, s1)
  | Exp e1 -> Exp (subst_expr mapping e1)
  | Jump _ | Label _ | Return -> s
  | Move _
  | Seq _
  | CJump _ -> failwith "shouldn't exist!"

module CcpCFG = struct
  module Lattice = CcpLattice
  module CFG = IrCfg
  module IDSE = IrDataStartExit
  module C = CFG
  module L = Lattice
  open L
  open C

  type graph = CFG.t
  type node = CFG.V.t
  type edge = CFG.E.t
  type data = Lattice.data

  let direction = `Forward

  type extra_info = L.defined String.Map.t

  let init undef_mapping (_: graph) =
    fun n ->
      match n with
      | IDSE.Start -> (L.Reach, undef_mapping)
      | _ -> (L.Unreach, undef_mapping)

  let transfer undef_mapping (e: edge) (d: data) =
    let src = E.src e in
    let label = C.E.label e in
    match d, src with
    | (L.Unreach, _), _ -> (L.Unreach, undef_mapping)
    | _, IDSE.Node {ir; _} -> eval_stmt (snd d) ir label
    | _, IDSE.Start
    | _, IDSE.Exit -> failwith "cannot happen"
end

module M = Cfg.IrStartExitMap

let map (g: IrCfg.t) ~(f:IrCfg.vertex -> 'a) : 'a M.t =
  IrCfg.fold_vertex (fun v m -> M.add m ~key:v ~data:(f v)) g M.empty

(* Given a function from edges to lattice elements, construct a map from
 * vertices to their in values, assuming we did a forwards analysis. *)
let make_in_forwards (g: IrCfg.t) (f: IrCfg.E.t -> 'a) (top: 'a) (bot: 'a): 'a M.t =
  let module L = CcpLattice in
  let module SE = Cfg.IrDataStartExit in
  let h v =
    match v with
    | SE.Start -> bot
    | SE.Exit | SE.Node _ -> IrCfg.fold_pred_e (fun e a -> L.(f e ** a)) g v top
  in
  map g ~f:h

module Ccp = Dataflow.GenericAnalysis(CcpCFG)

let ccp irs =
  let module C = IrCfg in
  let module IDSE = IrDataStartExit in
  let module L = CcpLattice in
  let g = C.create_cfg irs in
  let undef_mapping = map_temp_undef (get_all_temps g) in
  let ccp_e = Ccp.worklist undef_mapping g in
  let ccp_v_map = make_in_forwards g ccp_e (L.Unreach, undef_mapping) (L.Reach, undef_mapping) in
  let f_vertex v acc =
    match v with
    | IDSE.Start
    | IDSE.Exit -> acc
    | IDSE.Node {num; ir} ->
      begin
        match M.find_exn ccp_v_map v with
        | L.Unreach, _ -> acc
        | L.Reach, mapping -> Int.Map.add ~key:num ~data:(subst_stmt mapping ir) acc
      end
  in
  C.fold_vertex f_vertex g Int.Map.empty
  |> Int.Map.to_alist
  |> List.sort ~cmp:(fun (i1, _) (i2, _) -> Pervasives.compare i1 i2)
  |> List.map ~f:snd

let ccp_comp_unit (id, funcs) =
  let f ((fname, stmt, typ): Ir.func_decl) =
    match stmt with
    | Seq irs -> (fname, Seq (ccp irs), typ)
    | _ -> failwith "ccp_comp_unit: lowered func_decls should only have seqs"
  in
  (id, String.Map.map ~f funcs)
