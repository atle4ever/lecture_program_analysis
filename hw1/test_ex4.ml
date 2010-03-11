open OUnit
open Ex4

let rec cn i =
    if i = 0 then ZERO
    else SUCC (cn (i-1))

let test_natadd _ =
    assert_equal (cn 0) (natadd ((cn 0), (cn 0)));
    assert_equal (cn 3) (natadd ((cn 0), (cn 3)));
    assert_equal (cn 3) (natadd ((cn 3), (cn 0)));
    assert_equal (cn 5) (natadd ((cn 2), (cn 3)))

let test_natmul _ =
    assert_equal (cn 0) (natmul ((cn 0), (cn 0)));
    assert_equal (cn 0) (natmul ((cn 0), (cn 3)));
    assert_equal (cn 0) (natmul ((cn 3), (cn 0)));
    assert_equal (cn 3) (natmul ((cn 3), (cn 1)));
    assert_equal (cn 3) (natmul ((cn 1), (cn 3)));
    assert_equal (cn 6) (natmul ((cn 2), (cn 3)))

let suite = "Test ex4" >:::
    ["test natadd" >:: test_natadd;
     "test natmul" >:: test_natmul]
