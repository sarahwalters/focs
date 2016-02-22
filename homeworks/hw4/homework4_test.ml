open OUnit2;;
open Homework4;;

let dbl x = "double of "^(string_of_int x);;
let neg x = "negation of "^(string_of_int x);;

let isAccepting_test test_ctxt =
  assert_equal true (isAccepting dfaThreeA "start");;
  assert_equal false (isAccepting dfaThreeA "one");;
  assert_equal false (isAccepting dfaThreeA "two");;

let steps_test test_ctxt =
  assert_equal "start" (steps dfaThreeA "start" []);;
  assert_equal "one" (steps dfaThreeA "start" ['a']);;
  assert_equal "one" (steps dfaThreeA "start" ['a';'b']);;
  assert_equal "two" (steps dfaThreeA "start" ['a';'b';'a']);;
  assert_equal "one" (steps dfaThreeA "one" []);;
  assert_equal "two" (steps dfaThreeA "one" ['a']);;
  assert_equal "two" (steps dfaThreeA "one" ['a';'b']);;
  assert_equal "start" (steps dfaThreeA "one" ['a';'b';'a']);;

let acceptDFA_test test_ctxt =
  assert_equal true (acceptDFA dfaThreeA "");;
  assert_equal false (acceptDFA dfaThreeA "a");;
  assert_equal true (acceptDFA dfaThreeA "b");;
  assert_equal false (acceptDFA dfaThreeA "aa");;
  assert_equal true (acceptDFA dfaThreeA "aaa");;
  assert_equal true (acceptDFA dfaThreeA "ababa");;
  assert_equal false (acceptDFA dfaThreeA "abababa");;
  assert_equal true (acceptDFA dfaThreeA "aaaaaa");;


let at_least_test test_ctxt =
  assert_equal true (at_least 0 (fun x -> x) []);;
  assert_equal false (at_least 1 (fun x -> x) []);;
  assert_equal true (at_least 0 (fun x -> x) [true;true;false]);;
  assert_equal true (at_least 1 (fun x -> x > 0) [2;3;0]);;
  assert_equal true (at_least 2 (fun x -> x > 0) [2;3;0]);;
  assert_equal false (at_least 3 (fun x -> x > 0) [2;3;0]);;

let max_positive_test test_ctxt =
  assert_equal 0 (max_positive []);;
  assert_equal 4 (max_positive [4]);;
  assert_equal 5 (max_positive [4;5]);;
  assert_equal 5 (max_positive [5;4]);;
  assert_equal 6 (max_positive [4;6;5]);;
  assert_equal 0 (max_positive [-1;-2;-3]);;

let map_funs_test test_ctxt =
  assert_equal [] (map_funs [] 3);;
  assert_equal ["double of 3"] (map_funs [dbl] 3);;
  assert_equal ["double of 3";"negation of 3"] (map_funs [dbl;neg] 3);;
  assert_equal ["double of 3";"negation of 3";"double of 3"] (map_funs [dbl;neg;dbl] 3);;
  assert_equal [20;100] (map_funs [(fun x -> x*2);(fun x -> x*x)] 10);;
  assert_equal ["+hello";"-hello"] (map_funs [(fun x -> "+"^x);(fun x -> "-"^x)] "hello");;

let map_cross_test test_ctxt =
  assert_equal [] (map_cross [] []);;
  assert_equal [] (map_cross [] [1;2;3]);;
  assert_equal [] (map_cross [dbl;neg] []);;
  assert_equal ["double of 3"] (map_cross [dbl] [3]);;
  assert_equal ["double of 1"; "double of 2"; "double of 3"] (map_cross [dbl] [1;2;3]);;
  assert_equal ["double of 3"; "negation of 3"] (map_cross [dbl;neg] [3]);;
  assert_equal ["double of 1"; "negation of 1"; "double of 2"; "negation of 2"; "double of 3"; "negation of 3"] (map_cross [dbl;neg] [1;2;3]);;
  assert_equal ["+hello"; "-hello"; "+world"; "-world"] (map_cross [(fun x -> "+"^x);(fun x -> "-"^x)] ["hello";"world"]);;

let all_pairings_test test_ctxt =
  assert_equal [] (all_pairings [] []);;
  assert_equal [] (all_pairings [1;2] []);;
  assert_equal [] (all_pairings [] ["a";"b";"c"]);;
  assert_equal [(1, "a"); (1, "b"); (1, "c")] (all_pairings [1] ["a";"b";"c"]);;
  assert_equal [(1, "a"); (2, "a")] (all_pairings [1;2] ["a"]);;
  assert_equal [(1, "a"); (1, "b"); (1, "c"); (2, "a"); (2, "b"); (2, "c")] (all_pairings [1;2] ["a";"b";"c"]);;

let prefixes_test test_ctxt =
  assert_equal [[]] (prefixes []);;
  assert_equal [[]; [1]] (prefixes [1]);;
  assert_equal [[]; [1]; [1; 2]; [1; 2; 3]; [1; 2; 3; 4]] (prefixes [1;2;3;4]);;
  assert_equal [[]; ["a"]; ["a"; "b"]] (prefixes ["a";"b"]);;

let suffixes_test test_ctxt =
  assert_equal [[]] (suffixes []);;
  assert_equal [[1]; []] (suffixes [1]);;
  assert_equal [[1; 2; 3; 4]; [2; 3; 4]; [3; 4]; [4]; []] (suffixes [1;2;3;4]);;
  assert_equal [["a"; "b"; "c"]; ["b"; "c"]; ["c"]; []] (suffixes ["a";"b";"c"]);;

let inject_test test_ctxt =
  assert_equal [[99]] (inject 99 []);;
  assert_equal [[99; 1]; [1; 99]] (inject 99 [1]);;
  assert_equal [[99; 1; 2]; [1; 99; 2]; [1; 2; 99]] (inject 99 [1;2]);;
  assert_equal [[99; 1; 2; 3; 4]; [1; 99; 2; 3; 4]; [1; 2; 99; 3; 4]; [1; 2; 3; 99; 4]; [1; 2; 3; 4; 99]] (inject 99 [1;2;3;4]);;
  assert_equal [["X"; "a"; "b"]; ["a"; "X"; "b"]; ["a"; "b"; "X"]] (inject "X" ["a";"b"]);;

let permutations_test test_ctxt =
  assert_equal [[]] (permutations []);;
  assert_equal [[1]] (permutations [1]);;
  assert_equal [[1; 2]; [2; 1]] (permutations [1;2]);;
  assert_equal [[1; 2; 3]; [2; 1; 3]; [2; 3; 1]; [1; 3; 2]; [3; 1; 2]; [3; 2; 1]] (permutations [1;2;3]);;
  assert_equal [["a"; "b"]; ["b"; "a"]] (permutations ["a";"b"]);;

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
