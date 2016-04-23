open Core.Std
open OUnit2

let (===) (x: 'a) (y: 'a) : unit =
  assert_equal x y

let assert_true (b: bool) : unit =
	assert_equal ~printer:string_of_bool true b

let assert_false (b: bool) : unit =
	assert_equal ~printer:string_of_bool false b

module BoolEq = struct
  let (===) (x: bool) (y: bool) : unit =
    assert_equal ~printer:string_of_bool x y
end

module IntEq = struct
  let (===) (x: int) (y: int) : unit =
    assert_equal ~printer:string_of_int x y
end
