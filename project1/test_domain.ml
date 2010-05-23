open OUnit
open Analyzer
open Domain
open K

let test_interval _ =
  let intv1 = Interval.make 1 5 in
  let intv2 = Interval.make (-2) 3 in
  let intv3 = Interval.make 6 10 in
  let intv4 = Interval.make 2 3 in
    assert_equal (Interval.join intv1 intv2) (Interval.make (-2) 5);
    assert_equal (Interval.join intv1 intv3) (Interval.make 1 10);
    assert_equal (Interval.join intv1 intv4) (intv1);
    assert_raises (Error "Invalid Z. low > high") (fun _ -> Interval.make 5 1);
    assert_equal false (Interval.eq intv1 intv2);
    assert_equal true (Interval.eq intv1 intv1)

let test_m _ =
  let m1 = M.bind "a" (Val.Z (Parity.even, Interval.make 3 6)) M.empty in
  let m1 = M.bind "b" (Val.Bool Bool.mt) m1 in

  let m2 = M.bind "c" (Val.Loc (Loc.singleton "c")) M.empty in

  let m3 = M.bind "a" (Val.Z (Parity.odd, Interval.make 1 2)) M.empty in

  let r1 = M.bind "a" (Val.Z (Parity.even, Interval.make 3 6)) M.empty in
  let r1 = M.bind "b" (Val.Bool Bool.mt) r1 in
  let r1 = M.bind "c" (Val.Loc (Loc.singleton "c")) r1 in

  let r2 = M.bind "a" (Val.Z (Parity.top, Interval.make 1 6)) M.empty in
  let r2 = M.bind "b" (Val.Bool Bool.mt) r2 in

    assert_equal true (M.eq m1 m1);
    assert_equal false (M.eq m1 m2);
    assert_equal true (M.eq r1 (M.join m1 m2));

    assert_bool "test1" (M.eq (M.join m1 m2) r1);
    assert_bool "test2" (M.eq (M.join m1 m3) r2)


let suite = "Domain" >:::
  ["Interval" >:: test_interval;
   "M" >:: test_m
  ]

