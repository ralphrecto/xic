open Core.Std
open Cfg
open Dataflow
open Ir
open Tiling
open Fresh

(* ************************************************************************** *)
(* Helpers                                                                    *)
(* ************************************************************************** *)
module ExprSet = struct
  include Set.Make (struct
    type t = expr [@@deriving sexp, compare]
  end)

  let concat_map xs ~f =
    union_list (List.map xs ~f)

  let to_string irs =
    to_list irs
    |> List.map ~f:Ir.string_of_expr
    |> List.map ~f:(fun s -> "  " ^ s ^ ",")
    |> Util.join
    |> fun s -> "{\n" ^ s ^ "\n}"

  let to_small_string irs =
    to_list irs
    |> List.map ~f:Ir.string_of_expr
    |> Util.commas
    |> fun s -> "{" ^ s ^ "}"
end
open ExprSet

module ExprMap = Map.Make(struct
  type t = expr [@@deriving sexp, compare]
end)

module ExprSetIntersectLattice = struct
  type data = ExprSet.t
  let ( ** ) = ExprSet.inter
  let ( === ) = ExprSet.equal
  let to_string = ExprSet.to_string
end

module ExprSetUnionLattice = struct
  type data = ExprSet.t
  let ( ** ) = ExprSet.union
  let ( === ) = ExprSet.equal
  let to_string = ExprSet.to_string
end

module type ExprSetIntersectCFG = sig
  include Dataflow.CFGWithLatticeT with
    module Lattice = ExprSetIntersectLattice and
    module CFG = Cfg.IrCfg
end

module type ExprSetUnionCFG = sig
  include Dataflow.CFGWithLatticeT with
    module Lattice = ExprSetUnionLattice and
    module CFG = Cfg.IrCfg
end

let rec get_subexpr (e: expr) : ExprSet.t =
  match e with
  | BinOp (e1, (DIV|MOD), e2) -> union (get_subexpr e1) (get_subexpr e2)
  | BinOp (e1, _, e2) -> add (union (get_subexpr e1) (get_subexpr e2)) e
  | Call (_, elst) ->
    let f acc e = union acc (get_subexpr e) in
    List.fold_left ~f ~init: empty elst
  | Mem (e1, _) -> add (get_subexpr e1) e
  | Temp _ | Const _ | Name _ -> empty
  | ESeq _ -> failwith "shouldn't exist!"

and get_subexpr_stmt (s: stmt) : ExprSet.t =
  match s with
  | CJumpOne (e1, _) -> get_subexpr e1
  | Exp e1 -> get_subexpr e1
  | Move (e1, e2) -> union (get_subexpr e1) (get_subexpr e2)
  | Seq slst ->
      let f acc s = union acc (get_subexpr_stmt s) in
      List.fold_left ~f ~init: empty slst
  | Jump _ | Label _ | Return -> empty
  | CJump _ -> failwith "shouldn't exist!"

let rec get_mem_temp (e: expr) : ExprSet.t =
  match e with
  | BinOp (e1, _, e2) -> union (get_mem_temp e1) (get_mem_temp e2)
  | Call (_, elst) ->
    let f acc e = union acc (get_mem_temp e) in
    List.fold_left ~f ~init: empty elst
  | Mem (e1, _) -> add (get_mem_temp e1) e
  | Temp _ -> add empty e
  | Const _ | Name _ -> empty
  | ESeq _ -> failwith "shouldn't exist!"

let get_subexpr_stmt_v n =
  let module I = IrDataStartExit in
  match n with
  | I.Node {ir; _} -> get_subexpr_stmt ir
  | I.Start | I.Exit -> ExprSet.empty

let rec kill_func_args (elst: expr list) : ExprSet.t =
  let f acc e =
    match e with
    | Mem _ -> add acc e
    | Call (_, elst') -> kill_func_args elst'
    | _ -> empty
  in
  List.fold_left ~f ~init: empty elst

and kill_expr (e: expr) : ExprSet.t =
  match e with
  | Call (_, elst) -> kill_func_args elst
  | Temp _
  | Mem _ -> add empty e
  | BinOp _
  | Const _
  | Name _ -> empty
  | ESeq _ -> failwith "shouldn't exist!"

and kill_stmt (s: stmt) : ExprSet.t =
  match s with
  | Move ((Temp _ | Mem _) as e1, Call (_, elst)) ->
    let set = add empty e1 in
    union set (kill_func_args elst)
  | Move ((Temp _ | Mem _) as e1, _) -> add empty e1
  | Seq slst ->
      let f acc s =
        let set = kill_stmt s in
        union acc set
      in
      List.fold_left ~f ~init: empty slst
  | Exp e -> kill_expr e
  | CJumpOne _
  | Jump _
  | Label _
  | Return -> empty
  | Move _
  | CJump _ -> failwith "shouldn't exist!"

let kill_stmt_v n =
  let module I = IrDataStartExit in
  match n with
  | I.Node {ir; _} -> kill_stmt ir
  | I.Start | I.Exit -> ExprSet.empty

(* ************************************************************************** *)
(* Preprocessing Step                                                         *)
(* ************************************************************************** *)
let dummy_ir = Label "__dummy"
let preprocess g =
  let open Cfg in
  let open IrCfg in
  let open IrData in
  let open IrDataStartExit in

  (* compare two IR CFG nodes *)
  let v_compare a b =
    Int.compare (to_int a) (to_int b)
  in

  (* compare two IR CFG edges *)
  let e_compare a b =
    v_compare (V.label (E.src a)) (V.label (E.src b))
  in

  (* dummy node helpers *)
  let fresh_num = nb_vertex g in
  let dummy i = V.create (Node {num=i; ir=dummy_ir}) in

  (* gather all the vertices with more than on predecessor *)
  let vs = vertex_set g in
  let multiple_preds =
    VertexSet.filter ~f:(fun v -> in_degree g v > 1) vs
    |> VertexSet.to_list
    |> List.sort ~cmp:v_compare
  in

  (* add dummy nodes on all the incoming edges *)
  List.fold_left multiple_preds ~init:fresh_num ~f:(fun a v ->
    let preds =
      preds_e g v
      |> EdgeSet.to_list
      |> List.sort ~cmp:e_compare
    in
    List.fold_left preds ~init:a ~f:(fun a e ->
      let d = dummy a in
      remove_edge_e g e;
      add_vertex g d;
      add_edge g (E.src e) d;
      add_edge g d (E.dst e);
      a + 1
    )
  ) |> ignore

(* ************************************************************************** *)
(* Anticipated Expressions                                                    *)
(* ************************************************************************** *)
module BusyExprCFG = struct
  module Lattice = ExprSetIntersectLattice
  module CFG = IrCfg
  module IDSE = IrDataStartExit
  open Lattice
  open CFG

  type graph = CFG.t
  type node = CFG.V.t
  type edge = CFG.E.t
  type data = Lattice.data

  let direction = `Backward

  type extra_info = {
    g     : graph;
    univ  : data;
    uses  : node -> data;
    kills : node -> data;
  }

  let init ({univ; _} : extra_info) (_: graph) =
    fun n ->
      match n with
      | IDSE.Exit -> empty
      | IDSE.Start
      | IDSE.Node _ -> univ

  let transfer ({uses; kills; _}: extra_info) (e: edge) (d: data) =
    let node = E.dst e in
    let use = uses node in
    let kill = kills node in
    let f acc expr =
      let mem_temps = get_mem_temp expr in
      if ExprSet.is_empty (inter mem_temps kill) then
        add acc expr
      else
        acc
    in
    let diff_expr_kill = fold ~f ~init: empty d in
    union use diff_expr_kill
end

module BusyExpr = Dataflow.GenericAnalysis(BusyExprCFG)

(* ************************************************************************** *)
(* Available Expressions                                                      *)
(* ************************************************************************** *)
module AvailExprCFG = struct
  module Lattice = ExprSetIntersectLattice
  module CFG = IrCfg
  module IDSE = IrDataStartExit
  open Lattice
  open CFG

  type graph = CFG.t
  type node = CFG.V.t
  type edge = CFG.E.t
  type data = Lattice.data

  let direction = `Forward

  type extra_info = {
    g     : graph;
    univ  : data;
    busy  : node -> data;
    kills : node -> data;
  }

  let (+) = ExprSet.union

  let init ({univ; _}: extra_info) (_: graph) =
    fun n ->
      match n with
      | IDSE.Start -> empty
      | IDSE.Exit
      | IDSE.Node _ -> univ

  let transfer ({busy; kills; _}: extra_info) (e:edge) (d: data) =
    let source = E.src e in
    let anticipated = busy source in
    let kill = kills source in
    let union' = anticipated + d in
    let f acc expr =
      let mem_temps = get_mem_temp expr in
      if ExprSet.is_empty (inter mem_temps kill) then
        add acc expr
      else
        acc
    in
    fold ~f ~init: empty union'
end

module AvailExpr = Dataflow.GenericAnalysis(AvailExprCFG)

(* ************************************************************************** *)
(* Postponable Expressions                                                    *)
(* ************************************************************************** *)
module PostponeExprCFG = struct
  module Lattice = ExprSetIntersectLattice
  module CFG = IrCfg
  module IDSE = IrDataStartExit
  open Lattice
  open CFG

  type graph = CFG.t
  type node = CFG.V.t
  type edge = CFG.E.t
  type data = Lattice.data

  let direction = `Forward

  type extra_info = {
    g        : graph;
    univ     : data;
    uses     : node -> data;
    earliest : node -> data;
  }

  let (+) = ExprSet.union
  let (-) = ExprSet.diff

  let init ({univ; _}: extra_info) (_: graph) =
    fun n ->
      match n with
      | IDSE.Start -> empty
      | IDSE.Exit
      | IDSE.Node _ -> univ

  let transfer ({earliest; uses; _}: extra_info) (e: edge) (d: data) =
    let source = E.src e in
    printf "transferring edge %s\n" (CFG.string_of_edge e);
    printf "  source = %s\n" (CFG.string_of_vertex source);
    printf "  earliest = %s\n" (ExprSet.to_small_string (earliest source));
    printf "  d = %s\n" (ExprSet.to_small_string d);
    printf "  uses = %s\n" (ExprSet.to_small_string (uses source));
    ((earliest source) + d) - (uses source)
end

module PostponeExpr = Dataflow.GenericAnalysis(PostponeExprCFG)

(* ************************************************************************** *)
(* Used Expressions                                                           *)
(* ************************************************************************** *)
module UsedExprCFG = struct
  module Lattice = ExprSetUnionLattice
  module CFG = IrCfg

  type graph = CFG.t
  type node = CFG.V.t
  type edge = CFG.E.t
  type data = Lattice.data

  let direction = `Backward

  type extra_info = {
    uses   : node -> data;
    latest : node -> data;
  }

  let init _ _ _ = ExprSet.empty

  let (+) = ExprSet.union
  let (-) = ExprSet.diff

  let transfer {uses; latest} e x =
    let n = CFG.E.dst e in
    (uses n + x) - latest n
end

module UsedExpr = Dataflow.GenericAnalysis(UsedExprCFG)

(* ************************************************************************** *)
(* Whole Enchilada                                                            *)
(* ************************************************************************** *)
module C = Cfg.IrCfg
module E = ExprSet
module EM = ExprMap
module IL = ExprSetIntersectLattice
module M = Cfg.IrStartExitMap
module SE = Cfg.IrDataStartExit

let subst exp ~uses ~latest ~used ~freshes =
  let rec help exp =
    if (not (E.mem latest exp)) || E.mem used exp then
      EM.find_exn freshes exp
    else
      match exp with
      | BinOp (lhs, o, rhs) -> BinOp (help lhs, o, help rhs)
      | Call (f, args) -> Call (f, List.map ~f:help args)
      | Mem (e, t) -> Mem (help e, t)
      | ESeq _ -> failwith "eseq shouldn't even exist; it's lowered"
      | Const _ | Name _ | Temp _ -> exp
  in
  if E.mem uses exp
    then help exp
    else exp

let flatten g =
  let f v acc =
    match v with
    | SE.Exit -> acc
    | SE.Node {num; ir=Ir.Seq irs} ->
      begin
        let rev_irs = List.rev irs in
        match rev_irs with
        | last::tl ->
            if last = dummy_ir then
              match C.VertexSet.to_list (C.succs g v) with
              | [SE.Node {num; _}] ->
                begin
                  match Int.Map.find acc num with
                  | None -> Int.Map.add acc ~key:num ~data:(List.rev tl)
                  | Some irs' -> Int.Map.add acc ~key:num ~data:((List.rev tl)@irs')
                end
              | [SE.Exit] -> (assert ((List.length tl) = 0); acc)
              | _ -> failwith "shouldn't happen flatten"
            else
              begin
                match Int.Map.find acc num with
                | None -> Int.Map.add acc ~key:num ~data:irs
                | Some irs' -> Int.Map.add acc ~key:num ~data:(irs'@irs)
              end
        | _ -> failwith "shouldn't happen flatten"
      end
    | SE.Node _ -> failwith "shouldn't happen"
    | SE.Start -> failwith "no more start"
  in
  let mapping_list = Int.Map.to_alist (C.fold_vertex f g Int.Map.empty) in
  List.sort ~cmp:(fun (i1, _) (i2, _) -> Pervasives.compare i1 i2) mapping_list
  |> List.map ~f:snd
  |> List.concat


let red_elim g ~univ ~uses ~latest ~used =
  (* step (a) *)
  let freshes =
    E.to_list univ
    |> List.sort ~cmp:Ir.compare_expr
    |> List.remove_consecutive_duplicates ~equal:(fun e e' -> Ir.compare_expr e e' = 0)
    |> List.map ~f:(fun e -> (e, Ir.Temp (Ir_generation.FreshTemp.fresh ())))
    |> EM.of_alist_exn
  in

  (* step (b) *)
  let new_g = C.copy g in
  let f v =
    let new_irs = E.fold univ ~init:[] ~f:(fun acc exp ->
      if E.mem (latest v) exp && E.mem (used v) exp
        then (Ir.Move (EM.find_exn freshes exp, exp))::acc
        else acc
    )
    in
    match v with
    | SE.Start ->
      let newv = C.V.create (Node {num=(-1); ir=Ir.Seq (new_irs@[dummy_ir])}) in
      C.swap new_g ~oldv:v ~newv
    | SE.Node {num; ir} ->
      let newv = C.V.create (Node {num; ir=Ir.Seq (new_irs@[ir])}) in
      C.swap new_g ~oldv:v ~newv
    | SE.Exit -> ()
  in
  C.iter_vertex f g;

  (* step (c) *)
  let g = new_g in
  let new_g = C.copy g in
  let f v =
    match v with
    | SE.Start | SE.Exit -> ()
    | SE.Node {num; ir=Seq irs} as v -> begin
      match List.rev irs with
      | last::tl -> begin
        if last = dummy_ir then
          ()
        else
          let n = SE.Node {num; ir=last} in
          let sub e = subst e ~uses:(uses n)
                              ~latest:(latest n)
                              ~used:(used n)
                              ~freshes in
          let new_last =
            match last with
            | CJumpOne (e, l) -> CJumpOne (sub e, l)
            | Jump e -> Jump (sub e)
            | Exp e -> Exp (sub e)
            | Move (dst, src) -> Move (sub dst, sub src)
            | Label _
            | Return -> last
            | CJump _ -> failwith "shouldn't be any cjumps"
            | Seq _ -> failwith "shouldn't be any seqs"
          in
          let newv = SE.Node {num; ir=Seq (List.rev (new_last::tl))} in
          C.swap new_g ~oldv:v ~newv
      end
      | [] -> failwith "all seqs should have at least one thing"
    end
    | SE.Node _ -> failwith "red_elim: all irs should be Seqs"
  in
  C.iter_vertex f g;

  new_g

let pre irs =
  (* map a function into a map! *)
  let map (g: C.t) ~(f:C.vertex -> 'a) : 'a M.t =
    C.fold_vertex (fun v m -> M.add m ~key:v ~data:(f v)) g M.empty
  in

  (* turn a map into a function *)
  let fun_of_map m v =
    match M.find m v with
    | Some es -> es
    | None ->
        let key = C.string_of_vertex v in
        let map = M.to_string (E.to_small_string) m in
        failwith (sprintf "vertex '%s' not in map %s\n" key map)
  in

  (* Given a function from edges to lattice elements, construct a map from
   * vertices to their in values, assuming we did a backwards analysis. *)
  let make_in_backwards (g: C.t) (f: C.E.t -> 'a) (top: 'a): 'a M.t =
    let h v =
      match v with
      | SE.Start -> C.fold_succ_e (fun e a -> IL.(f e ** a)) g v top
      | SE.Exit | SE.Node _ -> C.fold_pred_e (fun e _ -> f e) g v top
    in
    map g ~f:h
  in

  (* Given a function from edges to lattice elements, construct a map from
   * vertices to their in values, assuming we did a forwards analysis. *)
  let make_in_forwards (g: C.t) (f: C.E.t -> 'a) (top: 'a) (bot: 'a): 'a M.t =
    let h v =
      match v with
      | SE.Start -> bot
      | SE.Exit | SE.Node _ -> C.fold_pred_e (fun e a -> IL.(f e ** a)) g v top
    in
    map g ~f:h
  in

  (* Given a function from edges to lattice elements, construct a map from
   * vertices to their in values, assuming we did a backwards analysis. *)
  let make_out_backwards (g: C.t) (f: C.E.t -> 'a) (top: 'a): 'a M.t =
    let h v =
      match v with
      | SE.Exit -> C.fold_pred_e (fun e a -> IL.(f e ** a)) g v top
      | SE.Start | SE.Node _ -> C.fold_succ_e (fun e _ -> f e) g v top
    in
    map g ~f:h
  in

  let g = C.create_cfg irs in
  preprocess g;
  let univ = ExprSet.concat_map irs ~f:get_subexpr_stmt in
  let uses = map g ~f:get_subexpr_stmt_v in
  let kills = map g ~f:kill_stmt_v in
  let uses_fun = fun_of_map uses in
  let kills_fun = fun_of_map kills in

  let busy_e = BusyExpr.worklist {g; univ; uses=uses_fun; kills=kills_fun} g in
  let busy_v = make_in_backwards g busy_e univ in
  let busy_fun = fun_of_map busy_v in

  let avail_e = AvailExpr.worklist {g; univ; busy=busy_fun; kills=kills_fun} g in
  let avail_v = make_in_forwards g avail_e univ E.empty in

  let earliest_v = map g ~f:(fun v ->
    M.(E.diff (find_exn busy_v v) (find_exn avail_v v))
  ) in
  let earliest_fun = fun_of_map earliest_v in

  let post_e = PostponeExpr.worklist {g; univ; uses=uses_fun; earliest=earliest_fun} g in
  let post_v = make_in_forwards g post_e univ E.empty in
  let post_fun = fun_of_map post_v in

  let f v =
    (* g uses post earliest (n: node) = *)
    ExprSet.(filter (union (earliest_fun v) (post_fun v)) ~f:(fun exp ->
      mem (uses_fun v) exp || C.VertexSet.exists (C.succs g v) ~f:(fun v' ->
        not (mem (earliest_fun v') exp || mem (post_fun v') exp)
      )
    ))
  in
  let latest_v = map g ~f in
  let latest_fun = fun_of_map latest_v in

  let used_e = UsedExpr.worklist {uses=uses_fun; latest=latest_fun} g in
  let used_v = make_out_backwards g used_e univ in
  let used_fun = fun_of_map used_v in

  let g = red_elim g ~univ ~uses:uses_fun ~latest:latest_fun ~used:used_fun in

  flatten g
