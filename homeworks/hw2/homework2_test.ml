open OUnit2;;

let prepend_test test_ctxt =
   assert_equal [] (Homework2.prepend ("", []));;
   assert_equal ["hello";"world"] (Homework2.prepend ("", ["hello";"world"]));;
   assert_equal [] (Homework2.prepend ("test", []));;
   assert_equal ["testhello";"testworld"] (Homework2.prepend ("test", ["hello";"world"]));;

let concatenate_test test_ctxt =
   assert_equal [] (Homework2.concatenate([], []));;
   assert_equal [] (Homework2.concatenate([], ["hello";"world"]));;
   assert_equal ["ahello";"aworld"] (Homework2.concatenate(["a"], ["hello";"world"]));;
   assert_equal ["ahello";"aworld";"bhello";"bworld"] (Homework2.concatenate(["a";"b"], ["hello";"world"]));;
   assert_equal [] (Homework2.concatenate(["a";"b"], []));;
   assert_equal ["ahello";"bhello"] (Homework2.concatenate(["a";"b"], ["hello"]));;

let all_strings_test test_ctxt =
   assert_equal [""] (Homework2.all_strings ([], 4));;
   assert_equal ["";"a";"aa";"aaa";"aaaa"] (Homework2.all_strings (["a"], 4));;
   assert_equal ["";"a";"aa";"aaa";"aaaa";"aaab";"aab";"aaba";"aabb";"ab";"aba";"abaa";"abab";"abb";"abba";"abbb";"b";"ba";"baa";"baaa";"baab";"bab";"baba";"babb";"bb";"bba";"bbaa";"bbab";"bbb";"bbba";"bbbb"] (Homework2.all_strings (["a";"b"], 4));;
   assert_equal ["";"a";"b"] (Homework2.all_strings (["a";"b"], 1));;
   assert_equal [""] (Homework2.all_strings (["a";"b"], 0));;

let restrict_test test_ctxt =
   assert_equal [] (Homework2.restrict ([], 4));;
   assert_equal ["a";"b"] (Homework2.restrict(["a";"b"],4));;
   assert_equal [] (Homework2.restrict(["a";"b"],0));;
   assert_equal ["a";"b"] (Homework2.restrict(["a";"b"],1));;
   assert_equal ["a";"b"] (Homework2.restrict(["a";"b";"abc"],1));;
   assert_equal ["a";"b"] (Homework2.restrict(["a";"b";"abc"],2));;
   assert_equal ["a";"b";"abc"] (Homework2.restrict(["a";"b";"abc"],3));;

let langUnion_test test_ctxt =
   assert_equal true (Homework2.setEqual (Homework2.langUnion([],[],4), []));;
   assert_equal true (Homework2.setEqual (Homework2.langUnion(["a";"b"],["c";"d"],4), ["a";"b";"c";"d"]));;
   assert_equal true (Homework2.setEqual (Homework2.langUnion(["a";"b"],["abc";"abcd";"abcde"],4), ["a";"b";"abc";"abcd"]));;
   assert_equal true (Homework2.setEqual (Homework2.langUnion(["abc";"abcd";"abcde"],["a";"b"],4), ["abc";"abcd";"a";"b"]));;
   assert_equal true (Homework2.setEqual (Homework2.langUnion(["abc";"abcd";"abcde"],[],4), ["abc";"abcd"]));;
   assert_equal true (Homework2.setEqual (Homework2.langUnion([],["abc";"abcd";"abcde"],4), ["abc";"abcd"]));;

let langConcat_test test_ctxt =
   assert_equal true (Homework2.setEqual (Homework2.langConcat([],[],4), []));;
   assert_equal true (Homework2.setEqual (Homework2.langConcat(["a";"b"],[],4), []));;
   assert_equal true (Homework2.setEqual (Homework2.langConcat([],["c";"d"],4), []));;
   assert_equal true (Homework2.setEqual (Homework2.langConcat(["a";"b"],["c";"d"],4), ["ac";"ad";"bc";"bd"]));;
   assert_equal true (Homework2.setEqual (Homework2.langConcat(["ab";"abb"],["c";"cc";"ccc"],4), ["abc";"abcc";"abbc"]));;

let langStar_test test_ctxt =
   assert_equal true (Homework2.setEqual (Homework2.langStar([], 4), [""]));;
   assert_equal true (Homework2.setEqual (Homework2.langStar(["a"], 4), ["";"a";"aa";"aaa";"aaaa"]));;
   assert_equal true (Homework2.setEqual (Homework2.langStar(["a";"b"], 4), ["";"a";"b";"aa";"ab";"aaa";"aab";"aaaa";"aaab";"aaba";"aabb";"aba";"abb";"abaa";"abab";"abba";"abbb";"ba";"bb";"baa";"bab";"baaa";"baab";"baba";"babb";"bba";"bbb";"bbaa";"bbab";"bbba";"bbbb"]));;
   assert_equal true (Homework2.setEqual (Homework2.langStar(["a";"bc"], 4), ["";"a";"bc";"aa";"abc";"aaa";"aabc";"aaaa";"abca";"bca";"bcbc";"bcaa"]));;
   assert_equal true (Homework2.setEqual (Homework2.langStar(["a";"bc";"def"],4), ["";"a";"bc";"def";"aa";"abc";"adef";"aaa";"aabc";"aaaa";"abca";"bca";"bcbc";"bcaa";"defa"]));;

let suite =
   "suite">:::
      ["prepend_test">::prepend_test;
       "concatenate_test">::concatenate_test;
       "all_strings_test">::all_strings_test;
       "restrict_test">::restrict_test;
       "langUnion_test">::langUnion_test;
       "langConcat_test">::langConcat_test;
       "langStar_test">::langStar_test]

let () =
   run_test_tt_main suite
