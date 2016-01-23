#use "homework1.ml"

let gcd_test =
   if not (
      gcd (1,1) = 1 &&
      gcd (1,3) = 1 &&
      gcd (2,4) = 2 &&
      gcd (4,2) = 2 &&
      gcd (4,6) = 2 &&
      gcd (9,12) = 3 &&
      gcd (60,70) = 10)
   then failwith "gcd_test failed"
   else true

let is_coprime_test =
   if not (
      is_coprime (1,2) = true &&
      is_coprime (2,3) = true &&
      is_coprime (2,4) = false &&
      is_coprime (10,20) = false &&
      is_coprime (9,16) = true)
   then failwith "is_coprime_test failed"
   else true

let euler_test =
   if not (
      euler 1 = 1 &&
      euler 2 = 1 &&
      euler 3 = 2 &&
      euler 4 = 2 &&
      euler 10 = 4 &&
      euler 20 = 8 &&
      euler 5555 = 4000)
   then failwith "euler_test failed"
   else true

let coprimes_test =
   if not (
      coprimes 1 = [1] &&
      coprimes 2 = [1] &&
      coprimes 3 = [1; 2] &&
      coprimes 4 = [1; 3] &&
      coprimes 10 = [1; 3; 7; 9] &&
      coprimes 20 = [1; 3; 7; 9; 11; 13; 17; 19])
   then failwith "coprimes_test failed"
   else true

let append_test =
   if not (
      append ([],[]) = [] &&
      append ([1],[]) = [1] &&
      append ([],[1]) = [1] &&
      append ([1],[1]) = [1;1] &&
      append ([1;2;3],[4;5;6]) = [1;2;3;4;5;6] &&
      append (["a"], ["b"]) = ["a";"b"])
   then failwith "append_test failed"
   else true

let flatten_test =
   if not (
      flatten [] = [] &&
      flatten [[1;2;3]] = [1;2;3] &&
      flatten [[1;2;3];[4;5;6]] = [1;2;3;4;5;6] &&
      flatten [[1;2;3];[4;5;6];[7;8]] = [1;2;3;4;5;6;7;8] &&
      flatten [[1;2;3];[];[7;8]] = [1;2;3;7;8] &&
      flatten [["a"];["b"]] = ["a";"b"])
   then failwith "flatten_test failed"
   else true

let last_test =
   if not (
      last [1] = 1 &&
      last [1;2] = 2 &&
      last [1;2;3;4;5;6;7;8] = 8 &&
      last ["a";"b";"c"] = "c")
   then failwith "last_test failed"
   else true

let nth_test =
   if not (
      nth (0,["a";"b";"c"]) = "a" &&
      nth (1,["a";"b";"c"]) = "b" &&
      nth (2,["a";"b";"c"]) = "c")
   then failwith "nth_test failed"
   else true

let separate_test =
   if not (
      separate [] = ([], []) &&
      separate [(1,2)] = ([1], [2]) &&
      separate [(1,2);(3,4)] = ([1; 3], [2; 4]) &&
      separate [(1,2);(3,4);(5,6)] = ([1; 3; 5], [2; 4; 6]) &&
      separate [(1,"a");(2,"b");(3,"c")] = ([1; 2; 3], ["a"; "b"; "c"]))
   then failwith "separate_test failed"
   else true

let setIn_test =
   if not (
      setIn (1,[]) = false &&
      setIn (1,[2;3]) = false &&
      setIn (1,[1;2;3]) = true &&
      setIn (1,[3;4;4;1;1;]) = true &&
      setIn ("a",["b";"a";"b"]) = true)
   then failwith "setIn_test failed"
   else true

let setSub_test =
   if not (
      setSub ([],[]) = true &&
      setSub ([],[1;1;1]) = true &&
      setSub ([1],[1;1;1]) = true &&
      setSub ([1;1;],[1;1;1]) = true &&
      setSub ([1;1],[1;2;3]) = true &&
      setSub ([1;1;],[2;3]) = false &&
      setSub ([1],[]) = false &&
      setSub (["a"],["a";"b"]) = true)
   then failwith "setSub_test failed"
   else true

let setEqual_test =
   if not (
      setEqual ([],[]) = true &&
      setEqual ([1],[1]) = true &&
      setEqual ([1],[1;1;1]) = true &&
      setEqual ([1;1;1],[1;1]) = true &&
      setEqual ([1;2],[1;2;3]) = false &&
      setEqual ([1;2],[2;1]) = true &&
      setEqual ([1;1;2],[2;2;1]) = true &&
      setEqual (["a";"b"],["b";"a"]) = true)
   then failwith "setEqual_test failed"
   else true

let setUnion_test =
   if not (
      setEqual (setUnion ([],[]), []) = true &&
      setEqual (setUnion ([],[1;1]), [1]) = true &&
      setEqual (setUnion ([1;2],[]), [2;1]) = true &&
      setEqual (setUnion ([1;2;3],[4;5;6]), [1;2;3;4;5;6]) = true &&
      setEqual (setUnion ([1;2],[2;3;3]), [1;2;3]) = true &&
      setEqual (setUnion ([1;2],[2;1]), [1;2]) = true &&
      setEqual (setUnion ([1],[2]), [1]) = false &&
      setEqual (setUnion ([1],[2]), [2]) = false &&
      setEqual (setUnion (["a"],["b"]), ["a";"b"]) = true)
   then failwith "setUnion_test failed"
   else true

let setInter_test =
   if not (
      setEqual (setInter ([],[]), []) = true &&
      setEqual (setInter ([1;2],[1]), [1]) = true &&
      setEqual (setInter ([1;2],[2;3]), [2]) = true &&
      setEqual (setInter ([1;2;3],[3;3;2;2]), [2;3]) = true &&
      setEqual (setInter ([],[1;2;3]), []) = true &&
      setEqual (setInter ([1;2;3],[]), []) = true &&
      setEqual (setInter ([1;2],[2]), [1]) = false &&
      setEqual (setInter ([1;2],[2;3]), [1;3]) = false &&
      setEqual (setInter (["a";"b"],["c";"b"]), ["b"]) = true)
   then failwith "setInter_test failed"
   else true

let setSize_test =
   if not (
      setSize [] = 0 &&
      setSize [1] = 1 &&
      setSize [1;2;3] = 3 &&
      setSize [1;1;1;2;2;2;3;3;3;4;4;4] = 4 &&
      setSize [1;2;3;2;1] = 3 &&
      setSize ["a";"a";"b"] = 2)
   then failwith "setSize_test failed"
   else true


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
   else "Tests failed."
