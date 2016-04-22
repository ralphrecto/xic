open Core.Std
open OUnit2

let (===) (x: 'a) (y: 'a) : unit =
  assert_equal x y

let assert_true (b: bool) : unit =
	b === true

let assert_false (b: bool) : unit =
	b === false
