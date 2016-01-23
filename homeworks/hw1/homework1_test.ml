#use "homework1.ml"

let gcd_test =
   gcd (1,1) = 1 &&
   gcd (1,3) = 1 &&
   gcd (2,4) = 2 &&
   gcd (4,2) = 2 &&
   gcd (4,6) = 2 &&
   gcd (9,12) = 3 &&
   gcd (60,70) = 10

let is_coprime_test =
   is_coprime (1,2) = true &&
   is_coprime (2,3) = true &&
   is_coprime (2,4) = false &&
   is_coprime (10,20) = false &&
   is_coprime (9,16) = true

let euler_test =
   euler 1 = 1 &&
   euler 2 = 1 &&
   euler 3 = 2 &&
   euler 4 = 2 &&
   euler 10 = 4 &&
   euler 20 = 8 &&
   euler 5555 = 4000

let coprimes_test =
   coprimes 1 = [1] &&
   coprimes 2 = [1] &&
   coprimes 3 = [1; 2] &&
   coprimes 4 = [1; 3] &&
   coprimes 10 = [1; 3; 7; 9] &&
   coprimes 20 = [1; 3; 7; 9; 11; 13; 17; 19]

let append_test =
   append ([],[]) = [] &&
   append ([1],[]) = [1] &&
   append ([],[1]) = [1] &&
   append ([1],[1]) = [1;1] &&
   append ([1;2;3],[4;5;6]) = [1;2;3;4;5;6] &&
   append (["a"], ["b"]) = ["a";"b"]

let flatten_test =
   flatten [] = [] &&
   flatten [[1;2;3]] = [1;2;3] &&
   flatten [[1;2;3];[4;5;6]] = [1;2;3;4;5;6] &&
   flatten [[1;2;3];[4;5;6];[7;8]] = [1;2;3;4;5;6;7;8] &&
   flatten [[1;2;3];[];[7;8]] = [1;2;3;7;8] &&
   flatten [["a"];["b"]] = ["a";"b"]

let last_test =
   last [1] = 1 &&
   last [1;2] = 2 &&
   last [1;2;3;4;5;6;7;8] = 8 &&
   last ["a";"b";"c"] = "c"

let nth_test =
   nth (0,["a";"b";"c"]) = "a" &&
   nth (1,["a";"b";"c"]) = "b" &&
   nth (2,["a";"b";"c"]) = "c"

let separate_test =
   separate [] = ([], []) &&
   separate [(1,2)] = ([1], [2]) &&
   separate [(1,2);(3,4)] = ([1; 3], [2; 4]) &&
   separate [(1,2);(3,4);(5,6)] = ([1; 3; 5], [2; 4; 6]) &&
   separate [(1,"a");(2,"b");(3,"c")] = ([1; 2; 3], ["a"; "b"; "c"])

let setIn_test =
   setIn (1,[]) = false &&
   setIn (1,[2;3]) = false &&
   setIn (1,[1;2;3]) = true &&
   setIn (1,[3;4;4;1;1;]) = true &&
   setIn ("a",["b";"a";"b"]) = true

let setSub_test =
   setSub ([],[]) = true &&
   setSub ([],[1;1;1]) = true &&
   setSub ([1],[1;1;1]) = true &&
   setSub ([1;1;],[1;1;1]) = true &&
   setSub ([1;1],[1;2;3]) = true &&
   setSub ([1;1;],[2;3]) = false &&
   setSub ([1],[]) = false &&
   setSub (["a"],["a";"b"]) = true

let setEqual_test =
   setEqual ([],[]) = true &&
   setEqual ([1],[1]) = true &&
   setEqual ([1],[1;1;1]) = true &&
   setEqual ([1;1;1],[1;1]) = true &&
   setEqual ([1;2],[1;2;3]) = false &&
   setEqual ([1;2],[2;1]) = true &&
   setEqual ([1;1;2],[2;2;1]) = true &&
   setEqual (["a";"b"],["b";"a"]) = true

let setUnion_test =
   setEqual (setUnion ([],[]), []) = true &&
   setEqual (setUnion ([],[1;1]), [1]) = true &&
   setEqual (setUnion ([1;2],[]), [2;1]) = true &&
   setEqual (setUnion ([1;2;3],[4;5;6]), [1;2;3;4;5;6]) = true &&
   setEqual (setUnion ([1;2],[2;3;3]), [1;2;3]) = true &&
   setEqual (setUnion ([1;2],[2;1]), [1;2]) = true &&
   setEqual (setUnion ([1],[2]), [1]) = false &&
   setEqual (setUnion ([1],[2]), [2]) = false &&
   setEqual (setUnion (["a"],["b"]), ["a";"b"]) = true

let setInter_test =
   setEqual (setInter ([],[]), []) = true &&
   setEqual (setInter ([1;2],[1]), [1]) = true &&
   setEqual (setInter ([1;2],[2;3]), [2]) = true &&
   setEqual (setInter ([1;2;3],[3;3;2;2]), [2;3]) = true &&
   setEqual (setInter ([],[1;2;3]), []) = true &&
   setEqual (setInter ([1;2;3],[]), []) = true &&
   setEqual (setInter ([1;2],[2]), [1]) = false &&
   setEqual (setInter ([1;2],[2;3]), [1;3]) = false &&
   setEqual (setInter (["a";"b"],["c";"b"]), ["b"]) = true

let setSize_test =
   setSize [] = 0 &&
   setSize [1] = 1 &&
   setSize [1;2;3] = 3 &&
   setSize [1;1;1;2;2;2;3;3;3;4;4;4] = 4 &&
   setSize [1;2;3;2;1] = 3 &&
   setSize ["a";"a";"b"] = 2


let all_tests_passed =
   if (gcd_test &&
      is_coprime_test &&
      euler_test &&
      coprimes_test &&
      append_test &&
      flatten_test &&
      last_test &&
      nth_test &&
      separate_test &&
      setIn_test &&
      setSub_test &&
      setEqual_test &&
      setUnion_test &&
      setInter_test &&
      setSize_test)
   then "All tests passed."
   else "Tests failed. See boolean _test values above."
