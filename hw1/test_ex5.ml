open OUnit
open Ex5

let test_basis _ =
  assert_equal (eval TRUE) true;
  assert_equal (eval FALSE) false

let test_not _ =
  assert_equal (eval (NOT TRUE)) false;
  assert_equal (eval (NOT FALSE)) true

let test_andalso _ =
  assert_equal (eval (ANDALSO (TRUE, TRUE))) true;
  assert_equal (eval (ANDALSO (FALSE, TRUE))) false;
  assert_equal (eval (ANDALSO (TRUE, FALSE))) false;
  assert_equal (eval (ANDALSO (FALSE, FALSE))) false

let test_orelse _ =
  assert_equal (eval (ORELSE (TRUE, TRUE))) true;
  assert_equal (eval (ORELSE (FALSE, TRUE))) true;
  assert_equal (eval (ORELSE (TRUE, FALSE))) true;
  assert_equal (eval (ORELSE (FALSE, FALSE))) false

let test_imply _ =
  assert_equal (eval (IMPLY (TRUE, TRUE))) true;
  assert_equal (eval (IMPLY (FALSE, TRUE))) true;
  assert_equal (eval (IMPLY (TRUE, FALSE))) false;
  assert_equal (eval (IMPLY (FALSE, FALSE))) true

let test_composition _ =
  assert_equal (eval (IMPLY (ORELSE(TRUE, ANDALSO(TRUE,FALSE)), (ORELSE(FALSE, IMPLY(TRUE, FALSE)))))) false

let test_less _ =
  assert_equal (eval (LESS (NUM 2, (PLUS (MINUS (NUM 3, NUM 2), NUM 7))))) true

let suite = "Test ex5" >:::
    ["test_basis" >:: test_basis;
     "test_not" >:: test_not;
     "test_andalso" >:: test_andalso;
     "test_orelse" >:: test_orelse;
     "test_imply" >:: test_imply;
     "test_composition" >:: test_composition;
     "test_less" >:: test_less];
