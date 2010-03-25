open OUnit

(* Collect the tests of different modules into one test suite *)
let suite = "PA HW3" >:::
  [Test_ex3.suite]


let _ =
  run_test_tt_main suite
