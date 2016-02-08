open OUnit2;;

let findTransitions_test test_ctxt =
   assert_equal [("start", 'a', "one")] (Homework3.findTransitions (Homework3.dfaThreeA, "start", 'a'));;
   assert_equal [("start", 'b', "start")] (Homework3.findTransitions (Homework3.dfaThreeA, "start", 'b'));;
   assert_equal [("one", 'b', "one")] (Homework3.findTransitions (Homework3.dfaThreeA, "one", 'b'));;
   assert_equal [(0, 'a', 0)] (Homework3.findTransitions (Homework3.nfaLastThreeB, 0, 'a'));;
   assert_equal [(0, 'b', 0); (0, 'b', 1)] (Homework3.findTransitions (Homework3.nfaLastThreeB, 0, 'b'));;

let isAccepting_test test_ctxt =
  assert_equal true (Homework3.isAccepting (Homework3.dfaThreeA, "start"));;
  assert_equal false (Homework3.isAccepting (Homework3.dfaThreeA, "one"));;
  assert_equal false (Homework3.isAccepting (Homework3.dfaThreeA, "two"));;
  assert_equal true (Homework3.isAccepting (Homework3.nfaLastThreeB, 3));;
  assert_equal false (Homework3.isAccepting (Homework3.nfaLastThreeB, 0));;

let step_test test_ctxt =
  assert_equal "one" (Homework3.step (Homework3.dfaThreeA, "start", 'a'));;
  assert_equal "start" (Homework3.step (Homework3.dfaThreeA, "start", 'b'));;
  assert_equal "two" (Homework3.step (Homework3.dfaThreeA, "one", 'a'));;
  assert_equal "one" (Homework3.step (Homework3.dfaThreeA, "one", 'b'));;
  assert_equal "start" (Homework3.step (Homework3.dfaThreeA, "two", 'a'));;
  assert_equal "two" (Homework3.step (Homework3.dfaThreeA, "two", 'b'));;

let steps_test test_ctxt =
  assert_equal "start" (Homework3.steps (Homework3.dfaThreeA, "start", []));;
  assert_equal "one" (Homework3.steps (Homework3.dfaThreeA, "start", ['a']));;
  assert_equal "one" (Homework3.steps (Homework3.dfaThreeA, "start", ['a';'b']));;
  assert_equal "two" (Homework3.steps (Homework3.dfaThreeA, "start", ['a';'b';'a']));;
  assert_equal "one" (Homework3.steps (Homework3.dfaThreeA, "one", []));;
  assert_equal "two" (Homework3.steps (Homework3.dfaThreeA, "one", ['a']));;
  assert_equal "two" (Homework3.steps (Homework3.dfaThreeA, "one", ['a';'b']));;
  assert_equal "start" (Homework3.steps (Homework3.dfaThreeA, "one", ['a';'b';'a']));;

let isDFA_test test_ctxt =
  assert_equal true (Homework3.isDFA (Homework3.dfaThreeA));;
  assert_equal false (Homework3.isDFA (Homework3.nfaLastThreeB));;

let acceptDFA_test test_ctxt =
  assert_equal true (Homework3.acceptDFA (Homework3.dfaThreeA, ""));;
  assert_equal false (Homework3.acceptDFA (Homework3.dfaThreeA, "a"));;
  assert_equal true (Homework3.acceptDFA (Homework3.dfaThreeA, "b"));;
  assert_equal false (Homework3.acceptDFA (Homework3.dfaThreeA, "aa"));;
  assert_equal true (Homework3.acceptDFA (Homework3.dfaThreeA, "aaa"));;
  assert_equal true (Homework3.acceptDFA (Homework3.dfaThreeA, "ababa"));;
  assert_equal false (Homework3.acceptDFA (Homework3.dfaThreeA, "abababa"));;

let dfa_q2_a_test test_ctxt =
  assert_equal true (Homework3.acceptDFA (Homework3.dfa_q2_a, ""));;
  assert_equal true (Homework3.acceptDFA (Homework3.dfa_q2_a, "a"));;
  assert_equal true (Homework3.acceptDFA (Homework3.dfa_q2_a, "b"));;
  assert_equal true (Homework3.acceptDFA (Homework3.dfa_q2_a, "aa"));;
  assert_equal true (Homework3.acceptDFA (Homework3.dfa_q2_a, "ba"));;
  assert_equal true (Homework3.acceptDFA (Homework3.dfa_q2_a, "ab"));;
  assert_equal true (Homework3.acceptDFA (Homework3.dfa_q2_a, "bb"));;
  assert_equal false (Homework3.acceptDFA (Homework3.dfa_q2_a, "bbb"));;

let dfa_q2_b_test test_ctxt =
  assert_equal true (Homework3.acceptDFA (Homework3.dfa_q2_b, ""));;
  assert_equal true (Homework3.acceptDFA (Homework3.dfa_q2_b, "a"));;
  assert_equal true (Homework3.acceptDFA (Homework3.dfa_q2_b, "ab"));;
  assert_equal true (Homework3.acceptDFA (Homework3.dfa_q2_b, "bab"));;
  assert_equal true (Homework3.acceptDFA (Homework3.dfa_q2_b, "baaa"));;
  assert_equal true (Homework3.acceptDFA (Homework3.dfa_q2_b, "abaa"));;
  assert_equal true (Homework3.acceptDFA (Homework3.dfa_q2_b, "abba"));;
  assert_equal true (Homework3.acceptDFA (Homework3.dfa_q2_b, "bbba"));;
  assert_equal false (Homework3.acceptDFA (Homework3.dfa_q2_b, "bbbb"));;
  assert_equal false (Homework3.acceptDFA (Homework3.dfa_q2_b, "aaaa"));;

let dfa_q2_c_test test_ctxt =
  assert_equal false (Homework3.acceptDFA (Homework3.dfa_q2_c, ""));;
  assert_equal false (Homework3.acceptDFA (Homework3.dfa_q2_c, "a"));;
  assert_equal false (Homework3.acceptDFA (Homework3.dfa_q2_c, "aa"));;
  assert_equal true (Homework3.acceptDFA (Homework3.dfa_q2_c, "aaa"));;
  assert_equal true (Homework3.acceptDFA (Homework3.dfa_q2_c, "aaabbb"));;
  assert_equal true (Homework3.acceptDFA (Homework3.dfa_q2_c, "bbbbaaa"));;

let nfa_q2_d_test test_ctxt =
  assert_equal true (Homework3.acceptNFA (Homework3.nfa_q2_d, "a"));;
  assert_equal true (Homework3.acceptNFA (Homework3.nfa_q2_d, "ba"));;
  assert_equal true (Homework3.acceptNFA (Homework3.nfa_q2_d, "aaba"));;
  assert_equal true (Homework3.acceptNFA (Homework3.nfa_q2_d, "aaaa"));;
  assert_equal false (Homework3.acceptNFA (Homework3.nfa_q2_d, "aaab"));;
  assert_equal false (Homework3.acceptNFA (Homework3.nfa_q2_d, "aaaab"));;
  assert_equal false (Homework3.acceptNFA (Homework3.nfa_q2_d, "aaabab"));;
  assert_equal false (Homework3.acceptNFA (Homework3.nfa_q2_d, ""));;


let keepTarget_test test_ctxt =
  assert_equal [] (Homework3.keepTarget []);;
  assert_equal [2;3] (Homework3.keepTarget [(1,'a',2);(1,'b',3)]);;
  assert_equal [3;2] (Homework3.keepTarget [(1,'a',2);(1,'b',3);(2,'a',2)]);;
  assert_equal ["start";"one";"two"] (Homework3.keepTarget Homework3.dfaThreeA.delta);;
  assert_equal [0;1;2;3] (Homework3.keepTarget Homework3.nfaLastThreeB.delta);;

let isAcceptingAny_test test_ctxt =
  assert_equal false (Homework3.isAcceptingAny (Homework3.nfaLastThreeB, []));;
  assert_equal false (Homework3.isAcceptingAny (Homework3.nfaLastThreeB, [0]));;
  assert_equal false (Homework3.isAcceptingAny (Homework3.nfaLastThreeB, [0;1]));;
  assert_equal false (Homework3.isAcceptingAny (Homework3.nfaLastThreeB, [0;1;2]));;
  assert_equal true (Homework3.isAcceptingAny (Homework3.nfaLastThreeB, [0;1;2;3]));;
  assert_equal true (Homework3.isAcceptingAny (Homework3.nfaLastThreeB, [3]));;

let stepAll_test test_ctxt =
  assert_equal [] (Homework3.stepAll (Homework3.dfaThreeA, [], 'a'));;
  assert_equal ["one"] (Homework3.stepAll (Homework3.dfaThreeA, ["start"], 'a'));;
  assert_equal ["start"] (Homework3.stepAll (Homework3.dfaThreeA, ["start"], 'b'));;
  assert_equal ["one";"two"] (Homework3.stepAll (Homework3.dfaThreeA, ["start";"one"], 'a'));;
  assert_equal ["start";"one"] (Homework3.stepAll (Homework3.dfaThreeA, ["start";"one"], 'b'));;
  assert_equal [0] (Homework3.stepAll (Homework3.nfaLastThreeB, [0;1], 'a'));;
  assert_equal [0;1;2] (Homework3.stepAll (Homework3.nfaLastThreeB, [0;1], 'b'));;

let stepsAll_test test_ctxt =
  assert_equal [] (Homework3.stepsAll (Homework3.dfaThreeA, [], []));;
  assert_equal [] (Homework3.stepsAll (Homework3.dfaThreeA, [], ['a']));;
  assert_equal [] (Homework3.stepsAll (Homework3.dfaThreeA, [], ['a';'b']));;
  assert_equal ["start"] (Homework3.stepsAll (Homework3.dfaThreeA, ["start"], []));;
  assert_equal ["one"] (Homework3.stepsAll (Homework3.dfaThreeA, ["start"], ['a']));;
  assert_equal ["one"] (Homework3.stepsAll (Homework3.dfaThreeA, ["start"], ['a';'b']));;
  assert_equal ["two"] (Homework3.stepsAll (Homework3.dfaThreeA, ["start"], ['a';'a']));;
  assert_equal ["two";"start"] (Homework3.stepsAll (Homework3.dfaThreeA, ["start";"one"], ['a';'a']));;
  assert_equal ["two";"start"] (Homework3.stepsAll (Homework3.dfaThreeA, ["start";"one"], ['a';'a';'b']));;
  assert_equal ["start";"one"] (Homework3.stepsAll (Homework3.dfaThreeA, ["start";"one"], ['a';'a';'b';'a']));;
  assert_equal [0;1;2;3] (Homework3.stepsAll (Homework3.nfaLastThreeB, [0;1], ['a';'b';'b';'b']));;

let acceptNFA_test test_ctxt =
  assert_equal false (Homework3.acceptNFA (Homework3.dfaThreeA, "babab"));;
  assert_equal true (Homework3.acceptNFA (Homework3.dfaThreeA, "bababa"));;
  assert_equal true (Homework3.acceptNFA (Homework3.dfaThreeA, "bababab"));;
  assert_equal false (Homework3.acceptNFA (Homework3.nfaLastThreeB, "abb"));;
  assert_equal true (Homework3.acceptNFA (Homework3.nfaLastThreeB, "abbb"));;


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
