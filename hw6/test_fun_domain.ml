open OUnit

let test_a _ =
  ()

let suite = "Fun Domain" >:::
  ["test_a" >:: test_a]
