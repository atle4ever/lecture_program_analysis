open OUnit
open Ex6

let test_num _ =
  assert_equal (eval (NUM 3)) 3

let test_plus _ =
  assert_equal (eval (PLUS (NUM 3, NUM 3))) 6

let test_minus _ =
  assert_equal (eval (MINUS (NUM 4, NUM 3))) 1

let test_mult _ =
  assert_equal (eval (MULT (NUM 3, NUM 4))) 12

let test_divide _ =
  assert_equal (eval (DIVIDE (NUM 6, NUM 2))) 3;
  assert_equal (eval (DIVIDE (NUM 2, NUM 3))) 0;
  assert_raises Division_by_zero (fun _ -> (eval (DIVIDE (NUM 6, NUM 0))))

let test_max _ =
  assert_equal (eval (MAX [])) 0;
  assert_equal (eval (MAX [NUM 1])) 1;
  assert_equal (eval (MAX [NUM 1; NUM 3; NUM 2])) 3

let suite = "Test ex6" >:::
  ["test_num" >:: test_num;
   "test_plus" >:: test_plus;
   "test_minus" >:: test_minus;
   "test_mult" >:: test_mult;
   "test_divide" >:: test_divide;
   "test_max" >:: test_num];



