open OUnit
open Functors
open Example

let x = Mem.make [("a", Z.make[1]); ("b", Z.make[1;2]); ("c", Z.make[1;2;3])]

let y = Mem.make [("a", Z.make[1;2]); ("b", Z.make[1;2;3;4]); ("c", Z.make[1;2;3]); ("d", Z.make[1;2;3;4])]

let w = Mem.make [("b", Z.make[4;5]); ("c", Z.make[4;5;6]); ("d", Z.make[4;5;6;7])]

let z = Mem.make [("d", Z.make[1;2;3])]

let test_leq _ =
  assert_bool "Not equal 1" (Mem.leq Mem.top Mem.top);
  assert_bool "Not equal 2" (not (Mem.leq Mem.top Mem.bot));
  assert_bool "Not equal 3" (Mem.leq Mem.bot Mem.top);
  assert_bool "Not equal 4" (Mem.leq Mem.bot Mem.bot);

  assert_bool "Not equal 5" (Mem.leq x Mem.top);
  assert_bool "Not equal 6" (not (Mem.leq x Mem.bot));
  assert_bool "Not equal 7" (not (Mem.leq Mem.top x));
  assert_bool "Not equal 8" (Mem.leq Mem.bot x);

  assert_bool "Not equal 9" (Mem.leq x y);
  assert_bool "Not equal 10" (not (Mem.leq y x));

  assert_bool "Not equal 11" (not (Mem.leq x z));
  assert_bool "Not equal 12" (not (Mem.leq z x));

  assert_bool "Not equal 13" (not (Mem.leq x w));
  assert_bool "Not equal 14" (not (Mem.leq w x));

  assert_bool "Not equal 15" (Mem.leq x x)

let equal_Mem x y = (Mem.leq x y) && (Mem.leq y x)
let equal_Z x y = (Z.leq x y) && (Z.leq y x)

let test_image _ =
  assert_bool "Not equal 1" (equal_Z (Z.bot) (Mem.image Mem.bot "a"));
  assert_bool "Not equal 2" (equal_Z (Z.top) (Mem.image Mem.top "a"));

  assert_bool "Not equal 3" (equal_Z (Z.make [1]) (Mem.image x "a"));
  assert_bool "Not equal 4" (equal_Z (Z.make [1;2]) (Mem.image x "b"));
  assert_bool "Not equal 5" (equal_Z (Z.make [1;2;3]) (Mem.image x "c"));
  assert_bool "Not equal 6" (equal_Z (Z.bot) (Mem.image x "d"))

let test_join _ =
  let a = Mem.make [("a", Z.make[1]); ("b", Z.make[1;2]); ("c", Z.make[1;2;3]); ("d", Z.make[1;2;3])] in
  let b = Mem.make [("a", Z.make[1]); ("b", Z.make[1;2;4;5]); ("c", Z.make[1;2;3;4;5;6]); ("d", Z.make[4;5;6;7])] in
    assert_bool "Not equal 1" (equal_Mem x (Mem.join Mem.bot x));
    assert_bool "Not equal 2" (equal_Mem x (Mem.join x Mem.bot));
    assert_bool "Not equal 3" (equal_Mem Mem.top (Mem.join Mem.top x));
    assert_bool "Not equal 4" (equal_Mem Mem.top (Mem.join x Mem.top));

    assert_bool "Not equal 5" (equal_Mem y (Mem.join x y));
    assert_bool "Not equal 6" (equal_Mem y (Mem.join y x));

    assert_bool "Not equal 7" (equal_Mem a (Mem.join x z));
    assert_bool "Not equal 8" (equal_Mem a (Mem.join z x));

    assert_bool "Not equal 9" (equal_Mem b (Mem.join x w));
    assert_bool "Not equal 10" (equal_Mem b (Mem.join w x))

let test_update _ =
  let x' = Mem.update Mem.bot "a" (Z.make [1]) in
  let x' = Mem.update x' "b" (Z.make [1;2]) in
  let x' = Mem.update x' "c" (Z.make [1;2;3]) in

  let y' = Mem.update x "a" (Z.make [1;2]) in
  let y' = Mem.update y' "b" (Z.make [1;2;3;4]) in
  let y' = Mem.update y' "c" (Z.make [1;2;3]) in
  let y' = Mem.update y' "d" (Z.make [1;2;3;4]) in

  let w' = Mem.make [("a", Z.make [1;2;3]); ("b", Z.top); ("c", Z.top); ("d", Z.top)] in
    assert_bool "Not equal 1" (equal_Mem x x');
    assert_bool "Not equal 2" (equal_Mem y y');
    assert_bool "Not equal 3" (equal_Mem w' (Mem.update Mem.top "a" (Z.make [1;2;3])))

let test_fold _ =
  let x' = Mem.make [("a", Z.make [1]); ("b", Z.make [2]); ("c", Z.make [3])] in
    assert_bool "Not equal 1" (equal_Z Z.bot (Mem.fold (fun l r a -> Z.join a r) Mem.bot Z.bot));
    assert_bool "Not equal 2" (equal_Z Z.top (Mem.fold (fun l r a -> Z.join a r) Mem.top Z.bot));
    assert_bool "Not equal 3" (equal_Z (Z.make [1;2;3;4]) (Mem.fold (fun l r a -> Z.join a r) x' (Z.make [4])))

let test_map _ =
  let x = Mem.make [("a", Z.make [1]); ("b", Z.make [2]); ("c", Z.make [3])] in
  let x' = Mem.make [("a", Z.make [1;4]); ("b", Z.make [2;4]); ("c", Z.make [3;4]); ("d", Z.make [4])] in
  let a = Mem.make [("a", Z.make [1]); ("b", Z.make [1]); ("c", Z.make [1]); ("d", Z.make [1])] in
    assert_bool "Not equal 1" (equal_Mem x' (Mem.map (fun l r -> (l, Z.join r (Z.make [4]))) x));
    assert_bool "Not equal 2" (equal_Mem a (Mem.map (fun l r -> (l, Z.make [1])) Mem.top));
    assert_bool "Not equal 3" (equal_Mem a (Mem.map (fun l r -> (l, Z.make [1])) Mem.bot))

let suite = "Fun Domain" >:::
  ["test_leq" >:: test_leq;
   "test_image" >:: test_image;
   "test_join" >:: test_join;
   "test_update" >:: test_update;
   "test_fold" >:: test_fold;
   "test_map" >:: test_map
  ]
