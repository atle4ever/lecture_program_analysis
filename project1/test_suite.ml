open OUnit

(* Collect the tests of different modules into one test suite *)
let suite = "PA Project1" >:::
   [Test_domain.suite]

let _ =
  run_test_tt_main suite
