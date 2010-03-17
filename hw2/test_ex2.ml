open OUnit
open Ex2

let test_queue _ =
  Random.self_init ();

  let rec rand_arr (n) =
    if n = 0 then []
    else [Random.int(100)] :: rand_arr (n-1)
  in
  let ra1 = rand_arr(10) in
  let ra2 = rand_arr(10) in
  let ra3 = rand_arr(10) in
  let queue1 = List.fold_left (fun q x -> IntListQ.enQ (q, x)) IntListQ.emptyQ ra1 in
  let queue2 = List.fold_left (fun q x -> IntListQ.enQ (q, x)) queue1 ra2 in
  let queue3 = List.fold_left (fun q x -> let (r, q') = IntListQ.deQ q in assert_equal r x; q') queue2 ra1 in
  let queue4 = List.fold_left (fun q x -> IntListQ.enQ (q, x)) queue3 ra3 in
  let queue5 = List.fold_left (fun q x -> let (r, q') = IntListQ.deQ q in assert_equal r x; q') queue4 ra2 in
  let queue6 = List.fold_left (fun q x -> let (r, q') = IntListQ.deQ q in assert_equal r x; q') queue5 ra3 in
    ()

let suite = "Test ex2" >:::
  ["test_queue" >:: test_queue]
