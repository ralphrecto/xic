open Core.Std
open OUnit2

let (===) (x: 'a) (y: 'a) : unit =
  assert_equal x y

let assert_true (b: bool) : unit =
	b === true

let assert_false (b: bool) : unit =
	b === false

module IntEq = struct
  let (===) (x: int) (y: int) : unit =
    assert_equal ~printer:string_of_int x y
end
