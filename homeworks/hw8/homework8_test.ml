open OUnit2;;
open Homework8;;

let zero = (simplify q1_defs "_0");;
let one = (simplify q1_defs "_1");;
let two = (simplify q1_defs "_2");;
let three = (simplify q1_defs "_3");;
let five = (simplify q1_defs "_5");;
let six = (simplify q1_defs "succ _5");;
let t = (simplify default_defs "true");;
let f = (simplify default_defs "false");;

let minus_test test_ctxt =
  assert_equal zero (simplify q1_defs "minus _0 _0");;
  assert_equal zero (simplify q1_defs "minus _5 _5");;
  assert_equal zero (simplify q1_defs "minus _2 _3");;

  assert_equal one (simplify q1_defs "minus _3 _2");;
  assert_equal one (simplify q1_defs "minus _4 _3");;

  assert_equal two (simplify q1_defs "minus _4 _2");;
  assert_equal two (simplify q1_defs "minus _2 _0");;

  assert_equal five (simplify q1_defs "minus (succ (succ _5)) _2");;

let geq_test test_ctxt =
  assert_equal t (simplify q1_defs "geq _0 _0");;
  assert_equal t (simplify q1_defs "geq _1 _0");;
  assert_equal t (simplify q1_defs "geq _4 _4");;
  assert_equal t (simplify q1_defs "geq _5 _3");;

  assert_equal f (simplify q1_defs "geq _0 _1");;
  assert_equal f (simplify q1_defs "geq _2 _3");;
  assert_equal f (simplify q1_defs "geq _1 _5");;

let eq_test test_ctxt =
  assert_equal t (simplify q1_defs "eq _0 _0");;
  assert_equal t (simplify q1_defs "eq _1 _1");;
  assert_equal t (simplify q1_defs "eq _2 _2");;
  assert_equal t (simplify q1_defs "eq _3 _3");;

  assert_equal f (simplify q1_defs "eq _1 _4");;
  assert_equal f (simplify q1_defs "eq _2 _3");;
  assert_equal f (simplify q1_defs "eq _0 _1");;

let match_pair_test test_ctxt =
  assert_equal "f x y" (simplify q1_defs "match_pair (pair x y) f");;
  assert_equal three (simplify q1_defs "match_pair (pair _1 _2) plus");;
  assert_equal t (simplify q1_defs "match_pair (pair true false) or");;
  assert_equal f (simplify q1_defs "match_pair (pair true false) and");;
  assert_equal six (simplify q1_defs "(/p.match_pair p (/x./y.times (succ x) (succ y))) (pair _1 _2)");;
  assert_equal t (simplify q1_defs "(/p.match_pair p (/x./y.eq y (succ x))) (pair _1 _2)");;
  assert_equal f (simplify q1_defs "(/p.match_pair p (/x./y.eq y (succ x))) (pair _1 _3)");;

let fst_test test_ctxt =
  assert_equal "a" (simplify q1_defs "fst (pair a b)");;
  assert_equal three (simplify q1_defs "fst (pair _3 (plus _1 _1))");;

let snd_test test_ctxt =
  assert_equal "b" (simplify q1_defs "snd (pair a b)");;
  assert_equal two (simplify q1_defs "snd (pair _3 (plus _1 _1))");;

let update_fst_test test_ctxt =
  assert_equal "c" (simplify q1_defs "fst (update_fst (pair a b) c)");;
  assert_equal "b" (simplify q1_defs "snd (update_fst (pair a b) c)");;

let update_snd_test test_ctxt =
  assert_equal "a" (simplify q1_defs "fst (update_snd (pair a b) c)");;
  assert_equal "c" (simplify q1_defs "snd (update_snd (pair a b) c)");;

let int_test test_ctxt =
  assert_equal t (simplify q2_defs "fst (int _0)");;
  assert_equal t (simplify q2_defs "fst (int _1)");;
  assert_equal t (simplify q2_defs "fst (int _2)");;
  assert_equal zero (simplify q2_defs "snd (int _0)");;
  assert_equal one (simplify q2_defs "snd (int _1)");;
  assert_equal two (simplify q2_defs "snd (int _2)");;

let neg_int_test test_ctxt =
  assert_equal f (simplify q2_defs "fst (neg_int (int _3))");;
  assert_equal three (simplify q2_defs "snd (neg_int (int _3))");;
  assert_equal t (simplify q2_defs "fst (neg_int (neg_int (int _3)))");;
  assert_equal three (simplify q2_defs "snd (neg_int (neg_int (int _3)))");;

let plus_int_test test_ctxt =
  assert_equal t (simplify q2_defs "fst (plus_int (int _3) (int _2))");;
  assert_equal five (simplify q2_defs "snd (plus_int (int _3) (int _2))");;

  assert_equal t (simplify q2_defs "fst (plus_int (int _3) (neg_int (int _2)))");;
  assert_equal one (simplify q2_defs "snd (plus_int (int _3) (neg_int (int _2)))");;

  assert_equal f (simplify q2_defs "fst (plus_int (neg_int (int _3)) (int _2))");;
  assert_equal one (simplify q2_defs "snd (plus_int (neg_int (int _3)) (int _2))");;

  assert_equal f (simplify q2_defs "fst (plus_int (neg_int (int _3)) (neg_int (int _2)))");;
  assert_equal five (simplify q2_defs "snd (plus_int (neg_int (int _3)) (neg_int (int _2)))");;

let times_int_test test_ctxt =
  assert_equal t (simplify q2_defs "fst (times_int (int _3) (int _2))");;
  assert_equal six (simplify q2_defs "snd (times_int (int _3) (int _2))");;

  assert_equal f (simplify q2_defs "fst (times_int (int _3) (neg_int (int _2)))");;
  assert_equal six (simplify q2_defs "snd (times_int (int _3) (neg_int (int _2)))");;

  assert_equal f (simplify q2_defs "fst (times_int (neg_int (int _3)) (int _2))");;
  assert_equal six (simplify q2_defs "snd (times_int (neg_int (int _3)) (int _2))");;

  assert_equal t (simplify q2_defs "fst (times_int (neg_int (int _3)) (neg_int (int _2)))");;
  assert_equal six (simplify q2_defs "snd (times_int (neg_int (int _3)) (neg_int (int _2)))");;

let suite =
  "suite">:::
    ["minus_test">::minus_test;
     "geq_test">::geq_test;
     "eq_test">::eq_test;
     "match_pair_test">::match_pair_test;
     "fst_test">::fst_test;
     "snd_test">::snd_test;
     "update_fst_test">::update_fst_test;
     "update_snd_test">::update_snd_test;
     "int_test">::int_test;
     "neg_int_test">::neg_int_test;
     "plus_int_test">::plus_int_test;
     "times_int_test">::times_int_test;
     ]

let () =
   run_test_tt_main suite
