open Core.Std

let init xs =
  match List.rev xs with
  | [] -> []
  | _::ys -> List.rev ys

let join =
  String.concat ~sep:"\n"

let commas =
  String.concat ~sep:","

let pairs xs =
  match xs with
  | [] -> []
  | _::xs' -> List.zip_exn (init xs) (xs')

let all_eq xs ys =
  let (<=) xs ys =
    List.for_all xs ~f:(List.mem ys)
  in
  xs <= ys && ys <= xs

let get_and_incr (r: int ref) : int =
  let x = !r in
  incr r;
  x

