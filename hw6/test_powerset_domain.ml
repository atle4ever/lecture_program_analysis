open OUnit
open Functors

module Var = PrimitiveSet(struct type t = string let compare = compare exception TooMany let all = fun () -> ["a";"b";"c";"d"] end)
module Int = PrimitiveSet(struct type t = int let compare = compare exception TooMany let all = fun () -> [0;1;2;3;4;5] end)

module Z = PowersetDomain (Int)

let test_remove _ =
  let x = Z.make [1; 2; 3] in
    assert_equal (Z.make [2; 3]) (Z.remove 1 x);
    assert_equal (Z.make [1; 2; 3]) (Z.remove 4 x);
    assert_equal (Z.make [0; 1; 2; 3; 5]) (Z.remove 4 Z.top)


let suite = "PowerSet Domain" >:::
  ["test_remove" >:: test_remove]
