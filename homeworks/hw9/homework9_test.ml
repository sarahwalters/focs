open OUnit2;;
open Homework9;;

let scale_test test_ctxt =
  assert_equal [0; 2; 4; 6; 8; 10; 12; 14; 16; 18] (prefix 10 (scale 2 nats));;
  assert_equal [0; 6; 12; 18; 24; 30; 36; 42; 48; 54] (prefix 10 (scale 3 evens));;
  assert_equal [4; 12; 20; 28; 36; 44; 52; 60; 68; 76] (prefix 10 (scale 4 odds));;
  assert_equal [0; 0; 0; 0; 0; 0; 0; 0; 0; 0] (prefix 10 (scale 0 nats));;

let mult_test test_ctxt =
  assert_equal [] (prefix 10 (mult nats nats));;
  assert_equal [] (prefix 10 (mult nats evens));;
  assert_equal [] (prefix 10 (mult nats odds));;
  assert_equal [] (prefix 10 (mult (cst 4) nats));;
  assert_equal [] (prefix 10 (mult (cst 0) nats));;

let suite =
  "suite">:::
    ["scale_test">::scale_test;
     "mult_test">::mult_test]

let () =
   run_test_tt_main suite
