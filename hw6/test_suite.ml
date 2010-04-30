open OUnit

let test_powerset_domain_remove _ =
  ()

(* Collect the tests of different modules into one test suite *)
let suite = "PA HW6" >:::
  [Test_powerset_domain.suite;
   Test_fun_domain.suite;
   Test_zintvl.suite]


let _ =
  run_test_tt_main suite
