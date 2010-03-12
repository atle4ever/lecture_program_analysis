open OUnit
open Ex2

let test_queue _ =
  let q = IntListQ.emptyQ in
  let q = IntListQ.enQ (q, [1; 1; 1; 1]) in
  let q = IntListQ.enQ (q, [2]) in
  let q = IntListQ.enQ (q, [3]) in
  let q = IntListQ.enQ (q, [4]) in
  let q = IntListQ.enQ (q, [5]) in
  let r1, q = IntListQ.deQ q in
  let r2, q = IntListQ.deQ q in
  let r3, q = IntListQ.deQ q in
  let r4, q = IntListQ.deQ q in
  let r5, q = IntListQ.deQ q in
  let r6, q = IntListQ.deQ q in
  let r7, q = IntListQ.deQ q in
  let r8, q = IntListQ.deQ q in
    assert_equal [1] r1;
    assert_equal [1] r2;
    assert_equal [1] r3;
    assert_equal [1] r4;
    assert_equal [2] r5;
    assert_equal [3] r6;
    assert_equal [4] r7;
    assert_equal [5] r8

let suite = "Test ex2" >:::
  ["test_queue" >:: test_queue]
