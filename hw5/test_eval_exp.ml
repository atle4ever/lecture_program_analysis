open OUnit
open K
open Printer
open Value

let m = Memory.bind "a" (INT 3) emptyMemory
let m = Memory.bind "b" (LOC "a") m
let m = Memory.bind "c" (LOC "d") m

let test_add _ =
  print_exp (ADD (NUM (-1), NUM 2));
  print_vs (eval_exp (ADD (NUM (-1), NUM 2)) emptyMemory);

  print_exp (ADD (READ, NUM 1));
  print_vs (eval_exp (ADD (READ, NUM 1)) emptyMemory);

  print_exp (ADD (NUM 1, READ));
  print_vs (eval_exp (ADD (NUM 1, READ)) emptyMemory);

  print_exp (ADD (READ, READ));
  print_vs (eval_exp (ADD (READ, READ)) emptyMemory);

  (* LOC type is added *)
  print_exp (ADD (NUM 1, VAR "b"));
  assert_raises (Error "LOC is used as INT type") (fun _ -> eval_exp (ADD (NUM 1, VAR "b")) m);

  print_newline()

let test_minus _ =
  print_exp (MINUS (NUM 2));
  print_vs (eval_exp (MINUS (NUM 2)) emptyMemory);

  print_exp (MINUS READ);
  print_vs (eval_exp (MINUS READ) emptyMemory);

  (* LOC type is minus *)
  print_exp (MINUS (VAR "b"));
  assert_raises (Error "LOC is used as INT type") (fun _ -> eval_exp (MINUS (VAR "b")) m);

  print_newline()

let test_read _ =
  print_exp (READ);
  print_vs (eval_exp READ emptyMemory);

  print_newline()

let test_amper _ =
  print_exp (AMPER "test");
  print_vs (eval_exp (AMPER "test") emptyMemory);

  print_newline()

let test_var _ =
  print_exp (VAR "a");
  print_vs (eval_exp (VAR "a") m);

  print_exp (VAR "b");
  print_vs (eval_exp (VAR "b") m);

  (* Unbinded var *)
  print_exp (VAR "d");
  assert_raises (Error "var d is not binded") (fun _ -> eval_exp (VAR "d") m);

  print_newline()

let test_star _ =
  print_exp (STAR "b");
  print_vs (eval_exp (STAR "b") m);

  (* Unbinded var *)
  print_exp (STAR "c");
  assert_raises (Error "var d is not binded") (fun _ -> eval_exp (STAR "c") m);

  (* INT type is referenced *)
  print_exp (STAR "a");
  assert_raises (Error "INT is used as LOC type") (fun _ -> eval_exp (STAR "a") m);

  print_newline()

let suite = "Test eval_exp" >:::
  ["test_add" >:: test_add;
   "test_minus" >:: test_minus;
   "test_read" >:: test_read;
   "test_amper" >:: test_amper;
   "test_var" >:: test_var;
   "test_star" >:: test_star
  ]
