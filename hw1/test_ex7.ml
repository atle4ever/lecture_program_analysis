open OUnit
open Ex7

let test_heap _ =
  let h = Ex7.EMPTY in
  let h = Ex7.insert (4, h) in
  let h = Ex7.insert (8, h) in
  let h = Ex7.insert (10, h) in
  let h = Ex7.insert (9, h) in
  let h = Ex7.insert (1, h) in
  let h = Ex7.insert (3, h) in
  let h = Ex7.insert (5, h) in
    assert_equal 1 (Ex7.findMin h);
    let h = Ex7.deleteMin h in
      assert_equal 3 (Ex7.findMin h);
      let h = Ex7.deleteMin h in
        assert_equal 4 (Ex7.findMin h);
        let h = Ex7.deleteMin h in
          assert_equal 5 (Ex7.findMin h);
          let h = Ex7.deleteMin h in
            assert_equal 8 (Ex7.findMin h);
            let h = Ex7.deleteMin h in
              assert_equal 9 (Ex7.findMin h);
              let h = Ex7.deleteMin h in
                assert_equal 10 (Ex7.findMin h)

let suite = "Test ex7" >:::
  ["test_head" >:: test_heap];

