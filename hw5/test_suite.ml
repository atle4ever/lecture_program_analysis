open OUnit

(* Collect the tests of different modules into one test suite *)
let suite = "PA HW5" >:::
  [Test_eval_exp.suite]


let _ =
  run_test_tt_main suite
