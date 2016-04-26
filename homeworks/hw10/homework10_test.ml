open OUnit2;;
open Homework10;;

let scale_test test_ctxt =
  assert_equal [0; 2; 4; 6; 8; 10; 12; 14; 16; 18] (prefix 10 (scale 2 nats));;

let suite =
  "suite">:::
    ["scale_test">::scale_test]

let () =
   run_test_tt_main suite
