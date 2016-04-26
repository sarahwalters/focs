open OUnit2;;
open Homework1;;

let gcd_test test_ctxt =
   assert_equal 1 (gcd (1,1));;
   assert_equal 1 (gcd (1,3));;
   assert_equal 2 (gcd (2,4));;
   assert_equal 2 (gcd (4,2));;
   assert_equal 2 (gcd (4,6));;
   assert_equal 3 (gcd (9,12));;
   assert_equal 10 (gcd (60,70));;

let is_coprime_test test_ctxt =
   assert_equal true (is_coprime (1,2));;
   assert_equal true (is_coprime (2,3));;
   assert_equal false (is_coprime (2,4));;
   assert_equal false (is_coprime (10,20));;
   assert_equal true (is_coprime (9,16));;

let euler_test test_ctxt =
   assert_equal 1 (euler 1);;
   assert_equal 1 (euler 2);;
   assert_equal 2 (euler 3);;
   assert_equal 2 (euler 4);;
   assert_equal 4 (euler 10);;
   assert_equal 8 (euler 20);;
   assert_equal 4000 (euler 5555);;

let coprimes_test test_ctxt =
   assert_equal [1] (coprimes 1);;
   assert_equal [1] (coprimes 2);;
   assert_equal [1;2] (coprimes 3);;
   assert_equal [1;3] (coprimes 4);;
   assert_equal [1;3;7;9] (coprimes 10);;
   assert_equal [1;3;7;9;11;13;17;19] (coprimes 20);;

let append_test test_ctxt =
   assert_equal [] (append ([],[]));;
   assert_equal [1] (append ([1],[]));;
   assert_equal [1] (append ([],[1]));;
   assert_equal [1;1] (append ([1],[1]));;
   assert_equal (append ([1;2;3],[4;5;6]));;
   assert_equal (append (["a"], ["b"]));;

let flatten_test test_ctxt =
   assert_equal [] (flatten []);;
   assert_equal [1;2;3] (flatten [[1;2;3]]);;
   assert_equal [1;2;3;4;5;6] (flatten [[1;2;3];[4;5;6]]);;
   assert_equal [1;2;3;4;5;6;7;8] (flatten [[1;2;3];[4;5;6];[7;8]]);;
   assert_equal [1;2;3;7;8] (flatten [[1;2;3];[];[7;8]]);;
   assert_equal ["a";"b"] (flatten [["a"];["b"]]);;

let last_test test_ctxt =
   assert_equal 1 (last [1]);;
   assert_equal 2 (last [1;2]);;
   assert_equal 8 (last [1;2;3;4;5;6;7;8]);;
   assert_equal "c" (last ["a";"b";"c"]);;

let nth_test test_ctxt =
   assert_equal "a" (nth (0,["a";"b";"c"]));;
   assert_equal "b" (nth (1,["a";"b";"c"]));;
   assert_equal "c" (nth (2,["a";"b";"c"]));;

let separate_test test_ctxt=
   assert_equal ([], []) (separate []);;
   assert_equal ([1], [2]) (separate [(1,2)]);;
   assert_equal ([1; 3], [2; 4]) (separate [(1,2);(3,4)]);;
   assert_equal ([1; 3; 5], [2; 4; 6]) (separate [(1,2);(3,4);(5,6)]);;
   assert_equal ([1; 2; 3], ["a"; "b"; "c"]) (separate [(1,"a");(2,"b");(3,"c")]);;

let setIn_test test_ctxt=
   assert_equal false (setIn (1,[]));;
   assert_equal false (setIn (1,[2;3]));;
   assert_equal true (setIn (1,[1;2;3]));;
   assert_equal true (setIn (1,[3;4;4;1;1;]));;
   assert_equal true (setIn ("a",["b";"a";"b"]));;

let setSub_test test_ctxt =
   assert_equal true (setSub ([],[]));;
   assert_equal true (setSub ([],[1;1;1]));;
   assert_equal true (setSub ([1],[1;1;1]));;
   assert_equal true (setSub ([1;1;],[1;1;1]));;
   assert_equal true (setSub ([1;1],[1;2;3]));;
   assert_equal false (setSub ([1;1;],[2;3]));;
   assert_equal false (setSub ([1],[]));;
   assert_equal true (setSub (["a"],["a";"b"]));;

let setEqual_test test_ctxt =
   assert_equal true (setEqual ([],[]));;
   assert_equal true (setEqual ([1],[1]));;
   assert_equal true (setEqual ([1],[1;1;1]));;
   assert_equal true (setEqual ([1;1;1],[1;1]));;
   assert_equal false (setEqual ([1;2],[1;2;3]));;
   assert_equal true (setEqual ([1;2],[2;1]));;
   assert_equal true (setEqual ([1;1;2],[2;2;1]));;
   assert_equal true (setEqual (["a";"b"],["b";"a"]));;

let setUnion_test test_ctxt =
   assert_equal true (setEqual (setUnion ([],[]), []));;
   assert_equal true (setEqual (setUnion ([],[1;1]), [1]));;
   assert_equal true (setEqual (setUnion ([1;2],[]), [2;1]));;
   assert_equal true (setEqual (setUnion ([1;2;3],[4;5;6]), [1;2;3;4;5;6]));;
   assert_equal true (setEqual (setUnion ([1;2],[2;3;3]), [1;2;3]));;
   assert_equal true (setEqual (setUnion ([1;2],[2;1]), [1;2]));;
   assert_equal false (setEqual (setUnion ([1],[2]), [1]));;
   assert_equal false (setEqual (setUnion ([1],[2]), [2]));;
   assert_equal true (setEqual (setUnion (["a"],["b"]), ["a";"b"]));;

let setInter_test test_ctxt =
   assert_equal true (setEqual (setInter ([],[]), []));;
   assert_equal true (setEqual (setInter ([1;2],[1]), [1]));;
   assert_equal true (setEqual (setInter ([1;2],[2;3]), [2]));;
   assert_equal true (setEqual (setInter ([1;2;3],[3;3;2;2]), [2;3]));;
   assert_equal true (setEqual (setInter ([],[1;2;3]), []));;
   assert_equal true (setEqual (setInter ([1;2;3],[]), []));;
   assert_equal false (setEqual (setInter ([1;2],[2]), [1]));;
   assert_equal false (setEqual (setInter ([1;2],[2;3]), [1;3]));;
   assert_equal true (setEqual (setInter (["a";"b"],["c";"b"]), ["b"]));;

let setSize_test test_ctxt =
   assert_equal 0 (setSize []);;
   assert_equal 1 (setSize [1]);;
   assert_equal 3 (setSize [1;2;3]);;
   assert_equal 4 (setSize [1;1;1;2;2;2;3;3;3;4;4;4]);;
   assert_equal 3 (setSize [1;2;3;2;1]);;
   assert_equal 2 (setSize ["a";"a";"b"]);;

let suite =
   "suite">:::
      ["gcd_test">::gcd_test;
       "is_coprime_test">::is_coprime_test;
       "euler_test">::euler_test;
       "coprimes_test">::coprimes_test;
       "append_test">::append_test;
       "flatten_test">::flatten_test;
       "last_test">::last_test;
       "nth_test">::nth_test;
       "separate_test">::separate_test;
       "setIn_test">::setIn_test;
       "setSub_test">::setSub_test;
       "setEqual_test">::setEqual_test;
       "setUnion_test">::setUnion_test;
       "setInter_test">::setInter_test;
       "setSize_test">::setSize_test]

let () =
   run_test_tt_main suite
