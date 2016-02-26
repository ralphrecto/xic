type 'a expr = Plus of 'a * ' a expr * 'a expr
  | Concat of 'a * 'a expr * 'a expr
  | Int of 'a * int
  | String of 'a * string

type typ = IntT | StringT

type position = int * int

type error = IntError of position | StringError of position

type t_info = error list * typ

let get_t_info (e: t_info expr) : t_info = match e with
  | Int (i, _)
  | String (i, _)
  | Concat (i, _, _)
  | Plus (i, _, _) -> i


let rec typecheck = function
  | Int (p, n) -> Int (([], IntT), n)
  | String (p, s) -> String (([], StringT), s)
  | Plus (p, e1, e2) -> begin
    let e1', e2' = typecheck e1, typecheck e2 in
    let (err1, t1), (err2, t2) = get_t_info e1', get_t_info e2' in
    if t1 = IntT && t2 = IntT then
      Plus ((err1 @ err2, IntT), e1', e2')
    else 
      Plus (((IntError p) :: err1 @ err2, IntT), e1', e2')
  end
  | Concat (p, e1, e2) -> begin
    let e1', e2' = typecheck e1, typecheck e1 in
    let (err1, t1), (err2, t2) = get_t_info e1', get_t_info e2' in
    if t1 = StringT && t2 == StringT then
      Concat ((err1 @ err2, StringT), e1', e2')
    else
      Concat (((StringError p) :: err1 @ err2, StringT), e1', e2')
  end
