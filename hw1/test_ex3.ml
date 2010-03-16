open OUnit
open Ex3

let test_zipperN _ =
    assert_equal [1;2;3] (zipperN [[1;2;3]]);
    assert_equal [1;4;2;5;3;6] (zipperN [[1;2;3]; [4;5;6]]);
    assert_equal [1;4;7;2;5;8;3;6;9] (zipperN [[1;2;3]; [4;5;6]; [7;8;9]]);
    assert_equal [1;3;7;2;4;8;5;9;6] (zipperN [[1;2]; [3;4;5;6]; [7;8;9]]);
    assert_equal [1] (zipperN [[1]]);
    assert_equal [] (zipperN [[]]);
    assert_equal [] (zipperN [[]; []; []])

let suite = "Test ex3" >:::
  ["test_zipperN" >:: test_zipperN]
