open OUnit
open Ex1

let assert_less result expect limit =
  assert_bool "Excess margin of error" (abs_float(result -. expect) < limit)

let test_arithmatic _ =
  assert_equal 3.0 (mathemadiga (ADD (REAL 1.0, REAL 2.0)));
  assert_equal 4.1 (mathemadiga (SUB (REAL 5.1, REAL 1.0)));
  assert_equal 1.44 (mathemadiga (MUL (REAL 1.2, REAL 1.2)));
  assert_equal 1.2 (mathemadiga (DIV (REAL 1.44, REAL 1.2)));

  assert_equal 3.0 (mathemadiga (ADD (INT 1, INT 2)));
  assert_equal 4.0 (mathemadiga (SUB (INT 5, INT 1)));
  assert_equal 6.0 (mathemadiga (MUL (INT 2, INT 3)));
  (* CHECK 1 *)
  assert_equal 2.5 (mathemadiga (DIV (INT 10, INT 4)))

let test_sigma _ =
  assert_equal 10.0 (mathemadiga (SIGMA (REAL 1.0, REAL 10.0, INT 1)));
  assert_equal 55.0 (mathemadiga (SIGMA (REAL 1.0, REAL 10.0, X)));
  (* CHECK 3 *)
  assert_equal 55.0 (mathemadiga (SIGMA (REAL 1.0, REAL 10.9, X)))

let test_integral _ =
  assert_less (mathemadiga (INTEGRAL (REAL 1.0, REAL 10.0, INT 1))) 9.0 0.5;
  assert_less (mathemadiga (INTEGRAL (REAL 1.0, REAL 10.0, X))) 49.5 0.5;
  (* CHECK 5 *)
  assert_less (mathemadiga (INTEGRAL (REAL 10.0, REAL 1.0, X))) (-49.5) 0.5;
  assert_less (mathemadiga (INTEGRAL (REAL 2.0, REAL 2.0, X))) 0.0 0.5;
  assert_less (mathemadiga (INTEGRAL (REAL 0.0, REAL 1.0, ADD(MUL(X, X),X)))) 0.83 0.5;
  (* CHECK 4 *)
  assert_less (mathemadiga (INTEGRAL (REAL 1.0, REAL 10.9, X))) 58.0 0.5

let test_sigma_integeral _ =
  assert_equal 18.0 (mathemadiga (SIGMA (INT 1, INT 3, SIGMA (INT 1, INT 3, X))));
  assert_less (mathemadiga (INTEGRAL (REAL 0.0, SIGMA(INT 1, INT 5, INT 1), X))) 12.5 0.5

    (* CHECK 2 *)
let test_invalidsigma _ =
  assert_raises InvalidSigma (fun _ -> mathemadiga (SIGMA (REAL 5.0, REAL 1.0, X)));
  assert_equal 1.0 (mathemadiga (SIGMA (REAL 0.0, REAL 1.0, X)))

let test_dividebyzero _ =
  assert_raises DivideByZero (fun _ -> mathemadiga (DIV (REAL 4.0, REAL 0.0)));
  assert_equal 2.0 (mathemadiga (DIV (REAL 4.0, REAL 2.0)))

let test_freevariable _ =
  assert_raises FreeVariable (fun _ -> mathemadiga (ADD (REAL 4.0, X)))

let suite = "Test ex1" >:::
  ["test_arithmatic" >:: test_arithmatic;
   "test_sigma" >:: test_sigma;
   "test_integral" >:: test_integral;
   "test_sigma_integeral" >:: test_sigma_integeral;
   "test_invalidsigma" >:: test_invalidsigma;
   "test_dividebyzero" >:: test_dividebyzero;
   "test_freevariable" >:: test_freevariable]

