let init xs =
  match List.rev xs with
  | [] -> []
  | y::ys -> List.rev ys

