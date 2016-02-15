open OUnit2;;

let dbl x = "double of "^(string_of_int x);;
let neg x = "negation of "^(string_of_int x);;

let isAccepting_test test_ctxt =
  assert_equal true (Homework4.isAccepting Homework4.dfaThreeA "start");;
  assert_equal false (Homework4.isAccepting Homework4.dfaThreeA "one");;
  assert_equal false (Homework4.isAccepting Homework4.dfaThreeA "two");;

let steps_test test_ctxt =
  assert_equal "start" (Homework4.steps Homework4.dfaThreeA "start" []);;
  assert_equal "one" (Homework4.steps Homework4.dfaThreeA "start" ['a']);;
  assert_equal "one" (Homework4.steps Homework4.dfaThreeA "start" ['a';'b']);;
  assert_equal "two" (Homework4.steps Homework4.dfaThreeA "start" ['a';'b';'a']);;
  assert_equal "one" (Homework4.steps Homework4.dfaThreeA "one" []);;
  assert_equal "two" (Homework4.steps Homework4.dfaThreeA "one" ['a']);;
  assert_equal "two" (Homework4.steps Homework4.dfaThreeA "one" ['a';'b']);;
  assert_equal "start" (Homework4.steps Homework4.dfaThreeA "one" ['a';'b';'a']);;

let acceptDFA_test test_ctxt =
  assert_equal true (Homework4.acceptDFA Homework4.dfaThreeA "");;
  assert_equal false (Homework4.acceptDFA Homework4.dfaThreeA "a");;
  assert_equal true (Homework4.acceptDFA Homework4.dfaThreeA "b");;
  assert_equal false (Homework4.acceptDFA Homework4.dfaThreeA "aa");;
  assert_equal true (Homework4.acceptDFA Homework4.dfaThreeA "aaa");;
  assert_equal true (Homework4.acceptDFA Homework4.dfaThreeA "ababa");;
  assert_equal false (Homework4.acceptDFA Homework4.dfaThreeA "abababa");;
  assert_equal true (Homework4.acceptDFA Homework4.dfaThreeA "aaaaaa");;


let at_least_test test_ctxt =
  assert_equal true (Homework4.at_least 0 (fun x -> x) []);;
  assert_equal false (Homework4.at_least 1 (fun x -> x) []);;
  assert_equal true (Homework4.at_least 0 (fun x -> x) [true;true;false]);;
  assert_equal true (Homework4.at_least 1 (fun x -> x > 0) [2;3;0]);;
  assert_equal true (Homework4.at_least 2 (fun x -> x > 0) [2;3;0]);;
  assert_equal false (Homework4.at_least 3 (fun x -> x > 0) [2;3;0]);;

let max_positive_test test_ctxt =
  assert_equal 0 (Homework4.max_positive []);;
  assert_equal 4 (Homework4.max_positive [4]);;
  assert_equal 5 (Homework4.max_positive [4;5]);;
  assert_equal 5 (Homework4.max_positive [5;4]);;
  assert_equal 6 (Homework4.max_positive [4;6;5]);;
  assert_equal 0 (Homework4.max_positive [-1;-2;-3]);;

let map_funs_test test_ctxt =
  assert_equal [] (Homework4.map_funs [] 3);;
  assert_equal ["double of 3"] (Homework4.map_funs [dbl] 3);;
  assert_equal ["double of 3";"negation of 3"] (Homework4.map_funs [dbl;neg] 3);;
  assert_equal ["double of 3";"negation of 3";"double of 3"] (Homework4.map_funs [dbl;neg;dbl] 3);;
  assert_equal [20;100] (Homework4.map_funs [(fun x -> x*2);(fun x -> x*x)] 10);;
  assert_equal ["+hello";"-hello"] (Homework4.map_funs [(fun x -> "+"^x);(fun x -> "-"^x)] "hello");;

let map_cross_test test_ctxt =
  assert_equal [] (Homework4.map_cross [] []);;
  assert_equal [] (Homework4.map_cross [] [1;2;3]);;
  assert_equal [] (Homework4.map_cross [dbl;neg] []);;
  assert_equal ["double of 3"] (Homework4.map_cross [dbl] [3]);;
  assert_equal ["double of 1"; "double of 2"; "double of 3"] (Homework4.map_cross [dbl] [1;2;3]);;
  assert_equal ["double of 3"; "negation of 3"] (Homework4.map_cross [dbl;neg] [3]);;
  assert_equal ["double of 1"; "negation of 1"; "double of 2"; "negation of 2"; "double of 3"; "negation of 3"] (Homework4.map_cross [dbl;neg] [1;2;3]);;
  assert_equal ["+hello"; "-hello"; "+world"; "-world"] (Homework4.map_cross [(fun x -> "+"^x);(fun x -> "-"^x)] ["hello";"world"]);;

let all_pairings_test test_ctxt =
  assert_equal [] (Homework4.all_pairings [] []);;
  assert_equal [] (Homework4.all_pairings [1;2] []);;
  assert_equal [] (Homework4.all_pairings [] ["a";"b";"c"]);;
  assert_equal [(1, "a"); (1, "b"); (1, "c")] (Homework4.all_pairings [1] ["a";"b";"c"]);;
  assert_equal [(1, "a"); (2, "a")] (Homework4.all_pairings [1;2] ["a"]);;
  assert_equal [(1, "a"); (1, "b"); (1, "c"); (2, "a"); (2, "b"); (2, "c")] (Homework4.all_pairings [1;2] ["a";"b";"c"]);;

let prefixes_test test_ctxt =
  assert_equal [[]] (Homework4.prefixes []);;
  assert_equal [[]; [1]] (Homework4.prefixes [1]);;
  assert_equal [[]; [1]; [1; 2]; [1; 2; 3]; [1; 2; 3; 4]] (Homework4.prefixes [1;2;3;4]);;
  assert_equal [[]; ["a"]; ["a"; "b"]] (Homework4.prefixes ["a";"b"]);;

let suffixes_test test_ctxt =
  assert_equal [[]] (Homework4.suffixes []);;
  assert_equal [[1]; []] (Homework4.suffixes [1]);;
  assert_equal [[1; 2; 3; 4]; [2; 3; 4]; [3; 4]; [4]; []] (Homework4.suffixes [1;2;3;4]);;
  assert_equal [["a"; "b"; "c"]; ["b"; "c"]; ["c"]; []] (Homework4.suffixes ["a";"b";"c"]);;

let inject_test test_ctxt =
  assert_equal [[99]] (Homework4.inject 99 []);;
  assert_equal [[99; 1]; [1; 99]] (Homework4.inject 99 [1]);;
  assert_equal [[99; 1; 2]; [1; 99; 2]; [1; 2; 99]] (Homework4.inject 99 [1;2]);;
  assert_equal [[99; 1; 2; 3; 4]; [1; 99; 2; 3; 4]; [1; 2; 99; 3; 4]; [1; 2; 3; 99; 4]; [1; 2; 3; 4; 99]] (Homework4.inject 99 [1;2;3;4]);;
  assert_equal [["X"; "a"; "b"]; ["a"; "X"; "b"]; ["a"; "b"; "X"]] (Homework4.inject "X" ["a";"b"]);;

let permutations_test test_ctxt =
  assert_equal [[]] (Homework4.permutations []);;
  assert_equal [[1]] (Homework4.permutations [1]);;
  assert_equal [[1; 2]; [2; 1]] (Homework4.permutations [1;2]);;
  assert_equal [[1; 2; 3]; [2; 1; 3]; [2; 3; 1]; [1; 3; 2]; [3; 1; 2]; [3; 2; 1]] (Homework4.permutations [1;2;3]);;
  assert_equal [["a"; "b"]; ["b"; "a"]] (Homework4.permutations ["a";"b"]);;

let suite =
  "suite">:::
    ["isAccepting_test">::isAccepting_test;
     "steps_test">::steps_test;
     "acceptDFA_test">::acceptDFA_test;
     "at_least_test">::at_least_test;
     "max_positive_test">::max_positive_test;
     "map_funs_test">::map_funs_test;
     "map_cross_test">::map_cross_test;
     "all_pairings_test">::all_pairings_test;
     "prefixes_test">::prefixes_test;
     "suffixes_test">::suffixes_test;
     "inject_test">::inject_test;
     "permutations_test">::permutations_test]

let () =
   run_test_tt_main suite
