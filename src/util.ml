let init xs =
  match List.rev xs with
  | [] -> []
  | _::ys -> List.rev ys

