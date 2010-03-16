open OUnit
open Ex7

let test_heap _ =
  Random.self_init ();

  let rec rand_arr (n) =
    if n = 0 then []
    else Random.int(100) :: rand_arr (n-1)
  in
  let ra = rand_arr(20) in
  let heap = List.fold_left (fun heap x -> Ex7.insert (x, heap)) Ex7.EMPTY ra in
  let sorted_ra = List.sort compare ra in
    List.fold_left (fun heap x -> assert_equal x (Ex7.findMin heap); Ex7.deleteMin heap) heap sorted_ra;
    ()

let suite = "Test ex7" >:::
  ["test_head" >:: test_heap];

