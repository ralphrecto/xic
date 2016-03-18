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
