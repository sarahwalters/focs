open OUnit2;;
open Homework5;;

let startConfig_test test_ctxt =
  assert_equal {state = "start"; before = []; after = [">"]} (startConfig asbs "");;
  assert_equal {state = "start"; before = []; after = [">"; "a"; "b"]} (startConfig asbs "ab");;
  assert_equal {state = "start"; before = []; after = ["|"]} (startConfig anbn "");;
  assert_equal {state = "start"; before = []; after = ["|"; "a"; "a"; "b"; "b"]} (startConfig anbn "aabb");;

let acceptConfig_test test_ctxt =
  assert_equal false (acceptConfig asbs {state="start"; before=[]; after=["_"]});;
  assert_equal false (acceptConfig asbs {state="q1"; before=[]; after=[">";"a";"_"]});;
  assert_equal true (acceptConfig asbs {state="acc"; before=["b"]; after=[">";"a";"_"]});;
  assert_equal false (acceptConfig asbs {state="rej"; before=["b"]; after=[">";"a";"_"]});;

let rejectConfig_test test_ctxt =
  assert_equal false (rejectConfig asbs {state="start"; before=[]; after=["_"]});;
  assert_equal false (rejectConfig asbs {state="q1"; before=[]; after=[">";"a";"_"]});;
  assert_equal false (rejectConfig asbs {state="acc"; before=["b"]; after=[">";"a";"_"]});;
  assert_equal true (rejectConfig asbs {state="rej"; before=["b"]; after=[">";"a";"_"]});;

let haltConfig_test test_ctxt =
  assert_equal false (haltConfig asbs {state="start"; before=[]; after=["_"]});;
  assert_equal false (haltConfig asbs {state="q1"; before=[]; after=[">";"a";"_"]});;
  assert_equal true (haltConfig asbs {state="acc"; before=["b"]; after=[">";"a";"_"]});;
  assert_equal true (haltConfig asbs {state="rej"; before=["b"]; after=[">";"a";"_"]});;

let step_test test_ctxt =
  assert_equal {state = "acc"; before = ["_"]; after = []} (step asbs {state="start"; before=[]; after=["_"]});;
  assert_equal {state = "q1"; before = [">"; "a"; "b"]; after = ["b"]} (step asbs {state="start"; before=[">";"a"]; after=["b";"b"]});;
  assert_equal {state = "rej"; before = [">"; "a"; "a"]; after = ["b"]} (step asbs {state="q1"; before=[">";"a"]; after=["a";"b"]});;
  assert_equal {state = "q1"; before = [">"; "a"; "b"]; after = ["b"]} (step asbs {state="q1"; before=[">";"a"]; after=["b";"b"]});;
  assert_equal {state = "q2"; before = ["|"; "a"; "b"; "/"]; after = []} (step anbn {state="q1"; before=["|";"a";"b"]; after=["/"]});;
  assert_equal {state = "q2"; before = ["|"; "a"]; after = ["b"; "/"]} (step anbn {state="q2"; before=["|";"a";"b"]; after=["/"]});;
  assert_equal {state = "q4"; before = ["|"; "X"]; after = ["b"]} (step anbn {state="q3"; before=["|"]; after=["a";"b"]});;
  assert_equal {state = "q2"; before = ["|"; "X"; "X"]; after = []} (step anbn {state="q4"; before=["|";"X"]; after=["b"]});;

let run_test test_ctxt =
  assert_equal true (run asbs "aab");;
  assert_equal true (run anbn "aabb");;
  assert_equal true (run anbncn "aabbcc");;
  assert_equal false (run anbn "aabbbb");;

let tm_q2_a_test test_ctxt =
  assert_equal true (run tm_q2_a "");;
  assert_equal true (run tm_q2_a "c");;
  assert_equal true (run tm_q2_a "cc");;
  assert_equal true (run tm_q2_a "dd");;
  assert_equal true (run tm_q2_a "cddc");;
  assert_equal true (run tm_q2_a "cdc");;
  assert_equal true (run tm_q2_a "cdc");;
  assert_equal true (run tm_q2_a "ccdcc");;
  assert_equal false (run tm_q2_a "ccdccc");;
  assert_equal false (run tm_q2_a "ccdc");;
  assert_equal false (run tm_q2_a "dcdc");;

let tm_q2_b_test test_ctxt =
  assert_equal true (run tm_q2_b "");;
  assert_equal true (run tm_q2_b "baaa");;
  assert_equal true (run tm_q2_b "bbaaaaaa");;
  assert_equal true (run tm_q2_b "bbbaaaaaaaaa");;
  assert_equal false (run tm_q2_b "b");;
  assert_equal false (run tm_q2_b "ba");;
  assert_equal false (run tm_q2_b "baa");;
  assert_equal false (run tm_q2_b "baaaa");;
  assert_equal false (run tm_q2_b "bbaaaaaaa");;
  assert_equal false (run tm_q2_b "aaa");;
  assert_equal false (run tm_q2_b "abbb");;

let suite =
  "suite">:::
    ["startConfig_test">::startConfig_test;
     "acceptConfig_test">::acceptConfig_test;
     "rejectConfig_test">::rejectConfig_test;
     "haltConfig_test">::haltConfig_test;
     "step_test">::step_test;
     "run_test">::run_test;
     "tm_q2_a_test">::tm_q2_a_test;
     "tm_q2_b_test">::tm_q2_b_test]

let () =
   run_test_tt_main suite
