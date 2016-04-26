open OUnit2;;
open Homework2;;

let prepend_test test_ctxt =
   assert_equal [] (prepend ("", []));;
   assert_equal ["hello";"world"] (prepend ("", ["hello";"world"]));;
   assert_equal [] (prepend ("test", []));;
   assert_equal ["testhello";"testworld"] (prepend ("test", ["hello";"world"]));;

let concatenate_test test_ctxt =
   assert_equal [] (concatenate([], []));;
   assert_equal [] (concatenate([], ["hello";"world"]));;
   assert_equal ["ahello";"aworld"] (concatenate(["a"], ["hello";"world"]));;
   assert_equal ["ahello";"aworld";"bhello";"bworld"] (concatenate(["a";"b"], ["hello";"world"]));;
   assert_equal [] (concatenate(["a";"b"], []));;
   assert_equal ["ahello";"bhello"] (concatenate(["a";"b"], ["hello"]));;

let all_strings_test test_ctxt =
   assert_equal [""] (all_strings ([], 4));;
   assert_equal ["";"a";"aa";"aaa";"aaaa"] (all_strings (["a"], 4));;
   assert_equal ["";"a";"aa";"aaa";"aaaa";"aaab";"aab";"aaba";"aabb";"ab";"aba";"abaa";"abab";"abb";"abba";"abbb";"b";"ba";"baa";"baaa";"baab";"bab";"baba";"babb";"bb";"bba";"bbaa";"bbab";"bbb";"bbba";"bbbb"] (all_strings (["a";"b"], 4));;
   assert_equal ["";"a";"b"] (all_strings (["a";"b"], 1));;
   assert_equal [""] (all_strings (["a";"b"], 0));;

let restrict_test test_ctxt =
   assert_equal [] (restrict ([], 4));;
   assert_equal ["a";"b"] (restrict(["a";"b"],4));;
   assert_equal [] (restrict(["a";"b"],0));;
   assert_equal ["a";"b"] (restrict(["a";"b"],1));;
   assert_equal ["a";"b"] (restrict(["a";"b";"abc"],1));;
   assert_equal ["a";"b"] (restrict(["a";"b";"abc"],2));;
   assert_equal ["a";"b";"abc"] (restrict(["a";"b";"abc"],3));;

let langUnion_test test_ctxt =
   assert_equal true (setEqual (langUnion([],[],4), []));;
   assert_equal true (setEqual (langUnion(["a";"b"],["c";"d"],4), ["a";"b";"c";"d"]));;
   assert_equal true (setEqual (langUnion(["a";"b"],["abc";"abcd";"abcde"],4), ["a";"b";"abc";"abcd"]));;
   assert_equal true (setEqual (langUnion(["abc";"abcd";"abcde"],["a";"b"],4), ["abc";"abcd";"a";"b"]));;
   assert_equal true (setEqual (langUnion(["abc";"abcd";"abcde"],[],4), ["abc";"abcd"]));;
   assert_equal true (setEqual (langUnion([],["abc";"abcd";"abcde"],4), ["abc";"abcd"]));;

let langConcat_test test_ctxt =
   assert_equal true (setEqual (langConcat([],[],4), []));;
   assert_equal true (setEqual (langConcat(["a";"b"],[],4), []));;
   assert_equal true (setEqual (langConcat([],["c";"d"],4), []));;
   assert_equal true (setEqual (langConcat(["a";"b"],["c";"d"],4), ["ac";"ad";"bc";"bd"]));;
   assert_equal true (setEqual (langConcat(["ab";"abb"],["c";"cc";"ccc"],4), ["abc";"abcc";"abbc"]));;

let langStar_test test_ctxt =
   assert_equal true (setEqual (langStar([], 4), [""]));;
   assert_equal true (setEqual (langStar(["a"], 4), ["";"a";"aa";"aaa";"aaaa"]));;
   assert_equal true (setEqual (langStar(["a";"b"], 4), ["";"a";"b";"aa";"ab";"aaa";"aab";"aaaa";"aaab";"aaba";"aabb";"aba";"abb";"abaa";"abab";"abba";"abbb";"ba";"bb";"baa";"bab";"baaa";"baab";"baba";"babb";"bba";"bbb";"bbaa";"bbab";"bbba";"bbbb"]));;
   assert_equal true (setEqual (langStar(["a";"bc"], 4), ["";"a";"bc";"aa";"abc";"aaa";"aabc";"aaaa";"abca";"bca";"bcbc";"bcaa"]));;
   assert_equal true (setEqual (langStar(["a";"bc";"def"],4), ["";"a";"bc";"def";"aa";"abc";"adef";"aaa";"aabc";"aaaa";"abca";"bca";"bcbc";"bcaa";"defa"]));;

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
