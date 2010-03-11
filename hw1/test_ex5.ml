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

(*
let test_num _ =
  assert_equal (eval_expr (NUM 3)) 3

let test_plus _ =
  assert_equal (eval_expr (PLUS (NUM 3, NUM 3))) 6

let test_minus _ =
  assert_equal (eval_expr (MINUS (NUM 4, NUM 3))) 1
*)

let test_less _ =
  assert_equal (eval (LESS (NUM 2, (PLUS (MINUS (NUM 3, NUM 2), NUM 7))))) true

let suite = "Test ex5" >:::
    ["test_basis" >:: test_basis;
     "test_not" >:: test_not;
     "test_andalso" >:: test_andalso;
     "test_orelse" >:: test_orelse;
     "test_imply" >:: test_imply;
     (*
       "test_num" >:: test_num;
     "test_plus" >:: test_plus;
     "test_minus" >:: test_minus;
     *)
     "test_less" >:: test_less];
