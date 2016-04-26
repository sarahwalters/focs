open OUnit2;;
open Homework3;;

let findTransitions_test test_ctxt =
   assert_equal [("start", 'a', "one")] (findTransitions (dfaThreeA, "start", 'a'));;
   assert_equal [("start", 'b', "start")] (findTransitions (dfaThreeA, "start", 'b'));;
   assert_equal [("one", 'b', "one")] (findTransitions (dfaThreeA, "one", 'b'));;
   assert_equal [(0, 'a', 0)] (findTransitions (nfaLastThreeB, 0, 'a'));;
   assert_equal [(0, 'b', 0); (0, 'b', 1)] (findTransitions (nfaLastThreeB, 0, 'b'));;

let isAccepting_test test_ctxt =
  assert_equal true (isAccepting (dfaThreeA, "start"));;
  assert_equal false (isAccepting (dfaThreeA, "one"));;
  assert_equal false (isAccepting (dfaThreeA, "two"));;
  assert_equal true (isAccepting (nfaLastThreeB, 3));;
  assert_equal false (isAccepting (nfaLastThreeB, 0));;

let step_test test_ctxt =
  assert_equal "one" (step (dfaThreeA, "start", 'a'));;
  assert_equal "start" (step (dfaThreeA, "start", 'b'));;
  assert_equal "two" (step (dfaThreeA, "one", 'a'));;
  assert_equal "one" (step (dfaThreeA, "one", 'b'));;
  assert_equal "start" (step (dfaThreeA, "two", 'a'));;
  assert_equal "two" (step (dfaThreeA, "two", 'b'));;

let steps_test test_ctxt =
  assert_equal "start" (steps (dfaThreeA, "start", []));;
  assert_equal "one" (steps (dfaThreeA, "start", ['a']));;
  assert_equal "one" (steps (dfaThreeA, "start", ['a';'b']));;
  assert_equal "two" (steps (dfaThreeA, "start", ['a';'b';'a']));;
  assert_equal "one" (steps (dfaThreeA, "one", []));;
  assert_equal "two" (steps (dfaThreeA, "one", ['a']));;
  assert_equal "two" (steps (dfaThreeA, "one", ['a';'b']));;
  assert_equal "start" (steps (dfaThreeA, "one", ['a';'b';'a']));;

let isDFA_test test_ctxt =
  assert_equal true (isDFA (dfaThreeA));;
  assert_equal false (isDFA (nfaLastThreeB));;

let acceptDFA_test test_ctxt =
  assert_equal true (acceptDFA (dfaThreeA, ""));;
  assert_equal false (acceptDFA (dfaThreeA, "a"));;
  assert_equal true (acceptDFA (dfaThreeA, "b"));;
  assert_equal false (acceptDFA (dfaThreeA, "aa"));;
  assert_equal true (acceptDFA (dfaThreeA, "aaa"));;
  assert_equal true (acceptDFA (dfaThreeA, "ababa"));;
  assert_equal false (acceptDFA (dfaThreeA, "abababa"));;

let dfa_q2_a_test test_ctxt =
  assert_equal true (acceptDFA (dfa_q2_a, ""));;
  assert_equal true (acceptDFA (dfa_q2_a, "a"));;
  assert_equal true (acceptDFA (dfa_q2_a, "b"));;
  assert_equal true (acceptDFA (dfa_q2_a, "aa"));;
  assert_equal true (acceptDFA (dfa_q2_a, "ba"));;
  assert_equal true (acceptDFA (dfa_q2_a, "ab"));;
  assert_equal true (acceptDFA (dfa_q2_a, "bb"));;
  assert_equal false (acceptDFA (dfa_q2_a, "bbb"));;

let dfa_q2_b_test test_ctxt =
  assert_equal true (acceptDFA (dfa_q2_b, ""));;
  assert_equal true (acceptDFA (dfa_q2_b, "a"));;
  assert_equal true (acceptDFA (dfa_q2_b, "ab"));;
  assert_equal true (acceptDFA (dfa_q2_b, "bab"));;
  assert_equal true (acceptDFA (dfa_q2_b, "baaa"));;
  assert_equal true (acceptDFA (dfa_q2_b, "abaa"));;
  assert_equal true (acceptDFA (dfa_q2_b, "abba"));;
  assert_equal true (acceptDFA (dfa_q2_b, "bbba"));;
  assert_equal false (acceptDFA (dfa_q2_b, "bbbb"));;
  assert_equal false (acceptDFA (dfa_q2_b, "aaaa"));;

let dfa_q2_c_test test_ctxt =
  assert_equal false (acceptDFA (dfa_q2_c, ""));;
  assert_equal false (acceptDFA (dfa_q2_c, "a"));;
  assert_equal false (acceptDFA (dfa_q2_c, "aa"));;
  assert_equal true (acceptDFA (dfa_q2_c, "aaa"));;
  assert_equal true (acceptDFA (dfa_q2_c, "aaabbb"));;
  assert_equal true (acceptDFA (dfa_q2_c, "bbbbaaa"));;

let nfa_q2_d_test test_ctxt =
  assert_equal true (acceptNFA (nfa_q2_d, "a"));;
  assert_equal true (acceptNFA (nfa_q2_d, "ba"));;
  assert_equal true (acceptNFA (nfa_q2_d, "aaba"));;
  assert_equal true (acceptNFA (nfa_q2_d, "aaaa"));;
  assert_equal false (acceptNFA (nfa_q2_d, "aaab"));;
  assert_equal false (acceptNFA (nfa_q2_d, "aaaab"));;
  assert_equal false (acceptNFA (nfa_q2_d, "aaabab"));;
  assert_equal false (acceptNFA (nfa_q2_d, ""));;


let keepTarget_test test_ctxt =
  assert_equal [] (keepTarget []);;
  assert_equal [2;3] (keepTarget [(1,'a',2);(1,'b',3)]);;
  assert_equal [3;2] (keepTarget [(1,'a',2);(1,'b',3);(2,'a',2)]);;
  assert_equal ["start";"one";"two"] (keepTarget dfaThreeA.delta);;
  assert_equal [0;1;2;3] (keepTarget nfaLastThreeB.delta);;

let isAcceptingAny_test test_ctxt =
  assert_equal false (isAcceptingAny (nfaLastThreeB, []));;
  assert_equal false (isAcceptingAny (nfaLastThreeB, [0]));;
  assert_equal false (isAcceptingAny (nfaLastThreeB, [0;1]));;
  assert_equal false (isAcceptingAny (nfaLastThreeB, [0;1;2]));;
  assert_equal true (isAcceptingAny (nfaLastThreeB, [0;1;2;3]));;
  assert_equal true (isAcceptingAny (nfaLastThreeB, [3]));;

let stepAll_test test_ctxt =
  assert_equal [] (stepAll (dfaThreeA, [], 'a'));;
  assert_equal ["one"] (stepAll (dfaThreeA, ["start"], 'a'));;
  assert_equal ["start"] (stepAll (dfaThreeA, ["start"], 'b'));;
  assert_equal ["one";"two"] (stepAll (dfaThreeA, ["start";"one"], 'a'));;
  assert_equal ["start";"one"] (stepAll (dfaThreeA, ["start";"one"], 'b'));;
  assert_equal [0] (stepAll (nfaLastThreeB, [0;1], 'a'));;
  assert_equal [0;1;2] (stepAll (nfaLastThreeB, [0;1], 'b'));;

let stepsAll_test test_ctxt =
  assert_equal [] (stepsAll (dfaThreeA, [], []));;
  assert_equal [] (stepsAll (dfaThreeA, [], ['a']));;
  assert_equal [] (stepsAll (dfaThreeA, [], ['a';'b']));;
  assert_equal ["start"] (stepsAll (dfaThreeA, ["start"], []));;
  assert_equal ["one"] (stepsAll (dfaThreeA, ["start"], ['a']));;
  assert_equal ["one"] (stepsAll (dfaThreeA, ["start"], ['a';'b']));;
  assert_equal ["two"] (stepsAll (dfaThreeA, ["start"], ['a';'a']));;
  assert_equal ["two";"start"] (stepsAll (dfaThreeA, ["start";"one"], ['a';'a']));;
  assert_equal ["two";"start"] (stepsAll (dfaThreeA, ["start";"one"], ['a';'a';'b']));;
  assert_equal ["start";"one"] (stepsAll (dfaThreeA, ["start";"one"], ['a';'a';'b';'a']));;
  assert_equal [0;1;2;3] (stepsAll (nfaLastThreeB, [0;1], ['a';'b';'b';'b']));;

let acceptNFA_test test_ctxt =
  assert_equal false (acceptNFA (dfaThreeA, "babab"));;
  assert_equal true (acceptNFA (dfaThreeA, "bababa"));;
  assert_equal true (acceptNFA (dfaThreeA, "bababab"));;
  assert_equal false (acceptNFA (nfaLastThreeB, "abb"));;
  assert_equal true (acceptNFA (nfaLastThreeB, "abbb"));;


let suite =
   "suite">:::
      ["findTransitions_test">::findTransitions_test;
       "isAccepting_test">::isAccepting_test;
       "step_test">::step_test;
       "steps_test">::steps_test;
       "isDFA_test">::isDFA_test;
       "acceptDFA_test">::acceptDFA_test;
       "dfa_q2_a_test">::dfa_q2_a_test;
       "dfa_q2_b_test">::dfa_q2_b_test;
       "dfa_q2_c_test">::dfa_q2_c_test;
       "nfa_q2_d_test">::nfa_q2_d_test;
       "keepTarget_test">::keepTarget_test;
       "isAcceptingAny_test">::isAcceptingAny_test;
       "stepAll_test">::stepAll_test;
       "acceptNFA_test">::acceptNFA_test]

let () =
   run_test_tt_main suite
