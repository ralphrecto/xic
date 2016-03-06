open Core.Std

let init xs =
  match List.rev xs with
  | [] -> []
  | _::ys -> List.rev ys

let commas ss =
  String.concat ~sep:"," ss

