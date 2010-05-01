open OUnit
open Functors
open Example

let x = Z.make [1;2;3]
let y = Z.make [1;2;3;4]
let z = Z.make [4;5;6]

let test_leq _ =
  assert_bool "Not equal 1" (Z.leq Z.top Z.top);
  assert_bool "Not equal 2" (not (Z.leq Z.top Z.bot));
  assert_bool "Not equal 3" (Z.leq Z.bot Z.top);
  assert_bool "Not equal 4" (Z.leq Z.bot Z.bot);

  assert_bool "Not equal 5" (Z.leq x Z.top);
  assert_bool "Not equal 6" (not (Z.leq x Z.bot));
  assert_bool "Not equal 7" (not (Z.leq Z.top x));
  assert_bool "Not equal 8" (Z.leq Z.bot x);

  assert_bool "Not equal 9" (Z.leq x y);
  assert_bool "Not equal 10" (not (Z.leq y x));

  assert_bool "Not equal 11" (not (Z.leq x z));
  assert_bool "Not equal 12" (not (Z.leq z x));

  assert_bool "Not equal 13" (Z.leq x x)

let equal_Z x y = (Z.leq x y) && (Z.leq y x)

let test_remove _ =
  let o = Z.make [1] in
    assert_bool "Not equal 1" (equal_Z Z.bot (Z.remove 1 Z.bot));
    assert_bool "Not equal 2" (equal_Z (Z.make [0; 1; 2; 3; 5; 6; 7; 8; 9]) (Z.remove 4 Z.top));
    assert_bool "Not equal 3" (equal_Z (Z.make [2; 3]) (Z.remove 1 x));
    assert_bool "Not equal 4" (equal_Z (Z.make [1; 2; 3]) (Z.remove 4 x));
    assert_bool "Not equal 5" (equal_Z Z.bot (Z.remove 1 o))

let test_diff _ =
  assert_bool "Not equal 1" (equal_Z Z.bot (Z.diff Z.bot x));
  assert_bool "Not equal 2" (equal_Z x (Z.diff x Z.bot));
  assert_bool "Not equal 3" (equal_Z (Z.make [0;4;5;6;7;8;9]) (Z.diff Z.top x));
  assert_bool "Not equal 4" (equal_Z (Z.make [4]) (Z.diff y x));
  assert_bool "Not equal 5" (equal_Z Z.bot (Z.diff x y));
  assert_bool "Not equal 6" (equal_Z x (Z.diff x z))

let test_inter _ =
  let y' = Z.make [2;3;4;5] in
    assert_bool "Not equal 1" (equal_Z Z.bot (Z.inter Z.bot x));
    assert_bool "Not equal 2" (equal_Z Z.bot (Z.inter x Z.bot));
    assert_bool "Not equal 3" (equal_Z x (Z.inter Z.top x));
    assert_bool "Not equal 4" (equal_Z x (Z.inter x Z.top));
    assert_bool "Not equal 5" (equal_Z x (Z.inter x y));
    assert_bool "Not equal 6" (equal_Z x (Z.inter y x));
    assert_bool "Not equal 7" (equal_Z (Z.make [2;3]) (Z.inter x y'));
    assert_bool "Not equal 8" (equal_Z (Z.make [2;3]) (Z.inter y' x));
    assert_bool "Not equal 9" (equal_Z Z.bot (Z.inter x z));
    assert_bool "Not equal 10" (equal_Z Z.bot (Z.inter z x))

let test_union _ =
  assert_bool "Not equal 1" (equal_Z x (Z.union Z.bot x));
  assert_bool "Not equal 2" (equal_Z x (Z.union x Z.bot));
  assert_bool "Not equal 3" (equal_Z Z.top (Z.union x Z.top));
  assert_bool "Not equal 4" (equal_Z Z.top (Z.union Z.top x));
  assert_bool "Not equal 5" (equal_Z y (Z.union y x));
  assert_bool "Not equal 6" (equal_Z y (Z.union x y));
  assert_bool "Not equal 7" (equal_Z (Z.make [1;2;3;4;5;6]) (Z.union x z));
  assert_bool "Not equal 8" (equal_Z (Z.make [1;2;3;4;5;6]) (Z.union z x))

let suite = "PowerSet Domain" >:::
  ["test_leq" >:: test_leq;
   "test_remove" >:: test_remove;
   "test_diff" >:: test_diff;
   "test_inter" >:: test_inter;
   "test_union" >:: test_union]
