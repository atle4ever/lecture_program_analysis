open OUnit
open Ex1

let test_zero _ =
  assert_equal 1 (iter(0, (fun x -> 2+x)) 1);
  assert_equal 1 (iter(0, (fun x -> 2*x)) 1);
  assert_equal 1 (iter(0, (fun x -> 2/x)) 1)

let test_nega _ =
  assert_raises (Ex1.Error "# of iter is less than 0") (fun _ -> iter(-1, (fun x -> 2+x)))

let test_posi _ =
  assert_equal 7 (iter(3, (fun x -> 2+x)) 1);
  assert_equal 8 (iter(3, (fun x -> 2*x)) 1);
  assert_equal (-2) (iter(3, (fun x -> x-1)) 1)

let suite = "Test ex1" >:::
  ["test_zero" >:: test_zero;
  "test_nega" >:: test_nega;
  "test_posi" >:: test_posi];
