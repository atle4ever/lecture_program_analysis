open OUnit
open Ex3

module N =
struct
  type t = INT of int | BOTTOM
  let order = function
      BOTTOM, _ -> true
    |  _, BOTTOM -> false
    | INT(i1), INT(i2) -> i1 <= i2

  let lub = function
      BOTTOM, r -> r
    | l, BOTTOM -> l
    | INT(i1), INT(i2) -> if i1 < i2 then INT(i2) else INT(i1)

  let bottom = BOTTOM

  let make = function(i) -> INT(i)

end

module N_FIX = Fix(N)

let test_freevariable _ =
  let f = function (n) -> N.make(3) in
    assert_equal (N.make(3)) (N_FIX.fix(f))

let suite = "Test ex3" >:::
  ["test_freevariable" >:: test_freevariable]

