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

(*************************************)
(*************************************)

module OrderedInt : Set.OrderedType with type t = int =
struct
  type t = int
  let compare = compare
end

module IntSet = Set.Make(OrderedInt)

module IntSetCPO : CPO  with type t = IntSet.t =
struct
  type t = IntSet.t
  exception NoLUB
  let order (x, y) = IntSet.subset x y
  let lub (x, y) = IntSet.union x y
  let bottom = IntSet.empty
end

module F = Fix(IntSetCPO)

let f x = IntSet.union x (IntSet.singleton 3)

let fixedpoint = F.fix f

let u = IntSet.elements fixedpoint

let test_freevariable _ =
  let f = function (n) -> N.make(3) in
    assert_equal (N.make(3)) (N_FIX.fix(f))

let test_freevariable2 _ =
  assert_equal [3] u

let suite = "Test ex3" >:::
  ["test_freevariable" >:: test_freevariable;
   "test_freevariable" >:: test_freevariable2]


