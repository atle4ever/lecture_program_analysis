open OUnit

let test_a _ =
  ()

let suite = "Z interval" >:::
  ["test_a" >:: test_a]
