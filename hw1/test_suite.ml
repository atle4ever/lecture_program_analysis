open OUnit

(* Collect the tests of different modules into one test suite *)
let suite = "PA HW1" >:::
  [Test_ex1.suite;
  Test_ex2.suite;
  Test_ex3.suite;
  Test_ex4.suite;
  Test_ex5.suite;
  Test_ex6.suite;
  Test_ex7.suite]

let _ =
  run_test_tt_main suite
