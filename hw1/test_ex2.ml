open OUnit
open Ex2

let test_zipper _ =
  assert_equal [] (zipper ([], []));
  assert_equal [1;2;3] (zipper ([], [1;2;3]));
  assert_equal [1;2;3] (zipper ([1;2;3], []));
  assert_equal [1;4;2;5;3;6] (zipper ([1;2;3], [4;5;6]));
  assert_equal [1;4;2;5;3;6;7] (zipper ([1;2;3], [4;5;6;7]));
  assert_equal [0;4;1;5;2;6;3] (zipper ([0;1;2;3], [4;5;6]))

let suite = "Test ex2" >:::
  ["test_zipper" >:: test_zipper]
