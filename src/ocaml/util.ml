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
  | x::xs' -> List.zip_exn (init xs) (xs')

let all_eq xs ys =
  let (<=) xs ys =
    List.for_all xs ~f:(List.mem ys)
  in
  xs <= ys && ys <= xs
