open Core.Std
open Async.Std

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

let int_of_string s =
  try Some (Int.of_string s) with _ -> None

let ordered_dedup xs =
  let rec help xs acc =
    match xs with
    | x::xs -> if List.mem xs x then help xs acc else help xs (x::acc)
    | [] -> acc
  in
  help (List.rev xs) []

let time thunk =
  let start = Time.now () in
  let x = thunk () in
  let stop = Time.now () in
  (Time.diff stop start, x)

let time_def d =
  let start = Time.now () in
  d >>= fun x ->
  let stop = Time.now () in
  return (Time.diff stop start, x)
