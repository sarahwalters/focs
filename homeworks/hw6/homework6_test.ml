open OUnit2;;
open Homework6;;

(* DEFINITIONS *)
let trans (x,y) = x^"/"^(string_of_int y);;
let delta x = match x with
  | (("a",1),"0") -> (("b",2),"0",0)
  | (("a",1),"1") -> (("c",3),"1",0)
  | (("b",2),"0") -> (("c",3),"0",0)
  | (("b",2),"1") -> (("a",1),"1",0)
  | (("c",3),"0") -> (("a",1),"0",1)
  | (("c",3),"1") -> (("b",2),"1",1)
  | (_,sym) -> (("a",1),sym,1);;
let states = [("a",1);("b",2);("c",3)];;
let new_delta = transformDelta states delta trans;;

let add1_trans = (fun (x,y,z) -> x^"|"^(string_of_int y)^"|"^(string_of_int z));;
let add1_transformed = transform add1 add1_trans;;

(* TESTS *)
let triples_test test_ctxt =
  assert_equal [] (triples [] [] []);;
  assert_equal [] (triples [] ["a";"b"] [100;101]);;
  assert_equal [] (triples [1;2] [] [100;101]);;
  assert_equal [] (triples [1;2] ["a";"b"] []);;
  assert_equal [(1, "a", 100)] (triples [1] ["a"] [100]);;
  assert_equal [(1, 3, 5); (1, 3, 6); (1, 4, 5); (1, 4, 6); (2, 3, 5); (2, 3, 6); (2, 4, 5); (2, 4, 6)] (triples [1;2] [3;4] [5;6]);;

let quads_test test_ctxt =
  assert_equal [] (quads [] [] [] []);;
  assert_equal [] (quads [] ["a";"b"] [100;101] [true;false]);;
  assert_equal [] (quads [1;2] [] [100;101] [true;false]);;
  assert_equal [] (quads [1;2] ["a";"b"] [] [true;false]);;
  assert_equal [] (quads [1;2] ["a";"b"] [100;101] []);;
  assert_equal [(1, "a", 100, true)] (quads [1] ["a"] [100] [true]);;
  assert_equal [(1, "a", 100, true); (1, "a", 100, false); (1, "a", 101, true); (1, "a", 101, false); (1, "b", 100, true); (1, "b", 100, false); (1, "b", 101, true); (1, "b", 101, false); (2, "a", 100, true); (2, "a", 100, false); (2, "a", 101, true); (2, "a", 101, false); (2, "b", 100, true); (2, "b", 100, false); (2, "b", 101, true); (2, "b", 101, false)] (quads [1;2] ["a";"b"] [100;101] [true;false]);;

let range_test test_ctxt =
  assert_equal [] (range (-1));;
  assert_equal [0] (range 0);;
  assert_equal [3;2;1;0] (range 3);;
  assert_equal [9;8;7;6;5;4;3;2;1;0] (range 9);;

let transformStates_test test_ctxt =
  assert_equal ["a/1"; "b/2"; "c/3"] (transformStates [("a",1);("b",2);("c",3)] trans);;
  assert_equal [] (transformStates [] trans);;

let transformDelta_test test_ctxt =
  assert_equal ("b/2", "0", 0) (new_delta ("a/1","0"));;
  assert_equal ("c/3", "0", 0) (new_delta("b/2","0"));;
  assert_equal ("a/1", "0", 1) (new_delta("c/3","0"));;

let transform_test test_ctxt =
  assert_equal (List.length add1.states) (List.length add1_transformed.states);;
  assert_equal add1.input_alphabet add1_transformed.input_alphabet;;
  assert_equal add1.tape_alphabet add1_transformed.tape_alphabet;;
  assert_equal add1.left_marker add1_transformed.left_marker;;
  assert_equal add1.blank add1_transformed.blank;;
  assert_equal "start|-1|-1" add1_transformed.start;;
  assert_equal "acc|-1|-1" add1_transformed.accept;;
  assert_equal "rej|-1|-1" add1_transformed.reject;;

  assert_equal true (run add1_transformed "00#01");;
  assert_equal true (run add1_transformed "011#100");;
  assert_equal false (run add1_transformed "011#010");;

let permutation_test test_ctxt =
  assert_equal true (run permutation "#");;
  assert_equal true (run permutation "a#a");;
  assert_equal true (run permutation "kk#kk");;
  assert_equal true (run permutation "hello#elhol");;
  assert_equal true (run permutation "obb#bob");;
  assert_equal true (run permutation "germany#mnayrge");;

  assert_equal false (run permutation "");;
  assert_equal false (run permutation "a#");;
  assert_equal false (run permutation "#a");;
  assert_equal false (run permutation "aaa#aaaa");;
  assert_equal false (run permutation "aaaa#aaa");;
  assert_equal false (run permutation "hello#hell");;
  assert_equal false (run permutation "hell#hello");;
  assert_equal false (run permutation "aab#bba");;
  assert_equal false (run permutation "a#a#a");;
  assert_equal false (run permutation "hello");;

let copies_test test_ctxt =
  assert_equal true (run (copies 1) "");;
  assert_equal true (run (copies 1) "1000");;
  assert_equal true (run (copies 1) "0101010101");;
  assert_equal false (run (copies 1) "0#1");;
  assert_equal false (run (copies 1) "10#10");;
  assert_equal false (run (copies 1) "10#10#10");;

  assert_equal true (run (copies 2) "#");;
  assert_equal true (run (copies 2) "1000#1000");;
  assert_equal true (run (copies 2) "00#00");;
  assert_equal true (run (copies 2) "101#101");;
  assert_equal false (run (copies 2) "");;
  assert_equal false (run (copies 2) "##");;
  assert_equal false (run (copies 2) "0#1");;
  assert_equal false (run (copies 2) "0#0#0");;
  assert_equal false (run (copies 2) "00#01");;

  assert_equal true (run (copies 3) "##");;
  assert_equal true (run (copies 3) "01#01#01");;
  assert_equal true (run (copies 3) "1001#1001#1001");;
  assert_equal false (run (copies 3) "");;
  assert_equal false (run (copies 3) "#");;
  assert_equal false (run (copies 3) "###");;
  assert_equal false (run (copies 3) "0#0");;
  assert_equal false (run (copies 3) "10#10#10#10");;

let suite =
  "suite">:::
    ["triples_test">::triples_test;
     "quads_test">::quads_test;
     "range_test">::range_test;
     "transformStates_test">::transformStates_test;
     "transformDelta_test">::transformDelta_test;
     "transform_test">::transform_test;
     "permutation_test">::permutation_test;
     "copies_test">::copies_test]

let () =
   run_test_tt_main suite
