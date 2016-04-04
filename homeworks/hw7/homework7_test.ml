open OUnit2;;
open Homework7;;

let palindromes_test test_ctxt =
  assert_equal true (generate 10 palindromes "");;
  assert_equal true (generate 10 palindromes "b");;
  assert_equal true (generate 10 palindromes "aba");;
  assert_equal true (generate 10 palindromes "abcba");;
  assert_equal true (generate 10 palindromes "bbacccabb");;

  assert_equal false (generate 10 palindromes "ab");;
  assert_equal false (generate 10 palindromes "aab");;
  assert_equal false (generate 10 palindromes "abc");;

let ambncmn_test test_ctxt =
  assert_equal true (generate 10 ambncmn "");;
  assert_equal true (generate 10 ambncmn "ac");;
  assert_equal true (generate 10 ambncmn "bc");;
  assert_equal true (generate 10 ambncmn "aaaccc");;
  assert_equal true (generate 10 ambncmn "abbccc");;

  assert_equal false (generate 10 ambncmn "cb");;
  assert_equal false (generate 10 ambncmn "ca");;
  assert_equal false (generate 10 ambncmn "abccc");;

let amcmnbn_test test_ctxt =
  assert_equal true (generate 10 amcmnbn "");;
  assert_equal true (generate 10 amcmnbn "ac");;
  assert_equal true (generate 10 amcmnbn "cb");;
  assert_equal true (generate 10 amcmnbn "accb");;
  assert_equal true (generate 10 amcmnbn "aacccb");;

  assert_equal false (generate 10 amcmnbn "acc");;
  assert_equal false (generate 10 amcmnbn "acbc");;
  assert_equal false (generate 10 amcmnbn "bcca");;

let ambncm_test test_ctxt =
  assert_equal true (generate 10 ambncm "");;
  assert_equal true (generate 10 ambncm "bbbbbbb");;
  assert_equal true (generate 10 ambncm "ac");;
  assert_equal true (generate 10 ambncm "abbbbbbbc");;
  assert_equal true (generate 10 ambncm "aaaacccc");;
  assert_equal true (generate 10 ambncm "aaaabcccc");;

  assert_equal false (generate 10 ambncm "a");;
  assert_equal false (generate 10 ambncm "acc");;

let eqnum_test test_ctxt =
  assert_equal true (generate 10 eqnum "");;
  assert_equal true (generate 10 eqnum "de");;
  assert_equal true (generate 10 eqnum "deed");;
  assert_equal true (generate 10 eqnum "eedd");;
  assert_equal true (generate 10 eqnum "edeeddde");;

  assert_equal false (generate 10 eqnum "dde");;
  assert_equal false (generate 10 eqnum "edeeddd");;
  assert_equal false (generate 10 eqnum "d");;

let dfaGrammar_test test_ctxt =
  assert_equal true (generate 10 (dfaGrammar dfaThreeA) "");;
  assert_equal true (generate 10 (dfaGrammar dfaThreeA) "bb");;
  assert_equal true (generate 10 (dfaGrammar dfaThreeA) "aaa");;
  assert_equal true (generate 10 (dfaGrammar dfaThreeA) "ababa");;
  assert_equal true (generate 10 (dfaGrammar dfaThreeA) "bababbbab");;
  assert_equal true (generate 13 (dfaGrammar dfaThreeA) "bababbbaaaab");;

  assert_equal false (generate 10 (dfaGrammar dfaThreeA) "a");;
  assert_equal false (generate 10 (dfaGrammar dfaThreeA) "babb");;
  assert_equal false (generate 10 (dfaGrammar dfaThreeA) "babab");;

let addition_test test_ctxt =
  assert_equal true (generate 10 addition "+=");;
  assert_equal true (generate 10 addition "x+=x");;
  assert_equal true (generate 10 addition "+x=x");;
  assert_equal true (generate 10 addition "x+x=xx");;
  assert_equal true (generate 13 addition "xx+xxx=xxxxx");;

  assert_equal false (generate 10 addition "+x=");;
  assert_equal false (generate 10 addition "+=x");;
  assert_equal false (generate 10 addition "x+=");;
  assert_equal false (generate 10 addition "xx+xx=xxx");;
  assert_equal false (generate 10 addition "xx+xx=xxxxx");;
  assert_equal false (generate 10 addition "+");;
  assert_equal false (generate 10 addition "=");;

let powers2_test test_ctxt =
  assert_equal true (generate 10 powers2 "a");;
  assert_equal true (generate 10 powers2 "aa");;
  assert_equal true (generate 10 powers2 "aaaa");;
  assert_equal true (generate 30 powers2 "aaaaaaaa");;
  assert_equal true (generate 30 powers2 "aaaaaaaaaaaaaaaa");;

  assert_equal false (generate 10 powers2 "");;
  assert_equal false (generate 10 powers2 "aaa");;
  assert_equal false (generate 10 powers2 "aaaaaa");;

let suite =
  "suite">:::
    ["palindromes_test">::palindromes_test;
     "ambncmn_test">::ambncmn_test;
     "amcmnbn_test">::amcmnbn_test;
     "ambncm_test">::ambncm_test;
     "eqnum_test">::eqnum_test;
     "dfaGrammar_test">::dfaGrammar_test;
     "addition_test">::addition_test;
     "powers2_test">::powers2_test]

let () =
   run_test_tt_main suite
