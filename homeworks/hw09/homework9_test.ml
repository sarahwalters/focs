open OUnit2;;
open Homework9;;

let sqrt v = newton (fun x -> x *. x -. v) (fun x -> 2.0 *. x) 1.0;;
let d1 = derivative (fun x -> x *. x) 4.0;;
let d2 = derivative (fun x-> x *. x *. x +. x *. x +. 2.0 *. x) 2.0;;

let scale_test test_ctxt =
  assert_equal [0; 2; 4; 6; 8; 10; 12; 14; 16; 18] (prefix 10 (scale 2 nats));;
  assert_equal [0; 6; 12; 18; 24; 30; 36; 42; 48; 54] (prefix 10 (scale 3 evens));;
  assert_equal [4; 12; 20; 28; 36; 44; 52; 60; 68; 76] (prefix 10 (scale 4 odds));;
  assert_equal [0; 0; 0; 0; 0; 0; 0; 0; 0; 0] (prefix 10 (scale 0 nats));;

let mult_test test_ctxt =
  assert_equal [0; 1; 4; 9; 16; 25; 36; 49; 64; 81] (prefix 10 (mult nats nats));;
  assert_equal [0; 2; 8; 18; 32; 50; 72; 98; 128; 162] (prefix 10 (mult nats evens));;
  assert_equal [0; 3; 10; 21; 36; 55; 78; 105; 136; 171] (prefix 10 (mult nats odds));;
  assert_equal [0; 4; 8; 12; 16; 20; 24; 28; 32; 36] (prefix 10 (mult (cst 4) nats));;
  assert_equal [0; 0; 0; 0; 0; 0; 0; 0; 0; 0] (prefix 10 (mult (cst 0) nats));;

let unzip_test test_ctxt =
  assert_equal [66; 66; 66; 66; 66; 66; 66; 66; 66; 66] (prefix 10 (let (s1,s2) = unzip(cst(66,99)) in s1));;
  assert_equal [99; 99; 99; 99; 99; 99; 99; 99; 99; 99] (prefix 10 (let (s1,s2) = unzip(cst(66,99)) in s2));;
  assert_equal [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] (prefix 10 (let (s1,s2) = unzip(zip nats evens) in s1));;
  assert_equal [0; 2; 4; 6; 8; 10; 12; 14; 16; 18] (prefix 10 (let (s1,s2) = unzip(zip nats evens) in s2));;
  assert_equal [0; 2; 4; 6; 8; 10; 12; 14; 16; 18] (prefix 10 (let (s1,s2) = unzip(map (fun (x,y) -> (y,x)) (zip nats evens)) in s1));;
  assert_equal [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] (prefix 10 (let (s1,s2) = unzip(map (fun (x,y) -> (y,x)) (zip nats evens)) in s2));;

let fold_test test_ctxt =
  assert_equal [0; 1; 3; 6; 10; 15; 21; 28; 36; 45] (prefix 10 (fold (fun a r -> a+r) (cst 0) nats));;
  assert_equal [10; 12; 16; 22; 30; 40; 52; 66; 82; 100] (prefix 10 (fold (fun a r -> a+r) (cst 10) evens));;
  assert_equal [0; 1; 1; 2; 2; 3; 3; 4; 4; 5] (prefix 10 (fold (fun a r -> abs(a-r)) (cst 0) nats));;
  assert_equal [0; 0; 0; 0; 0; 0; 0; 0; 0; 0] (prefix 10 (fold (fun a r -> a*r) (cst 0) nats));;
  assert_equal [0; 0; 0; 0; 0; 0; 0; 0; 0; 0] (prefix 10 (fold (fun a r -> a*r) (cst 1) nats));;
  assert_equal [1; 3; 15; 105; 945; 10395; 135135; 2027025; 34459425; 654729075] (prefix 10 (fold (fun a r -> a*r) (cst 1) odds));;

let running_max_test test_ctxt =
  assert_equal [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] (prefix 10 (running_max nats));;
  assert_equal [1; 3; 5; 7; 9; 11; 13; 15; 17; 19] (prefix 10 (running_max odds));;
  assert_equal [0; 0; 0; 0; 0; 0; 0; 0; 0; 0] (prefix 10 (running_max (map (fun x -> (-x)) nats)));;
  assert_equal [0; 0; 2; 2; 4; 4; 6; 6; 8; 8] (prefix 10 (running_max (map (fun x -> if x mod 2 = 0 then x else -x) nats)));;
  assert_equal [0; 1; 1; 1; 1; 1; 2; 2; 2; 2; 2; 2; 2; 2; 2; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 4; 4] (prefix 30 (running_max ampl));;

let stutter_test test_ctxt =
  assert_equal [0; 0; 1; 1; 2; 2; 3; 3; 4; 4] (prefix 10 (stutter nats));;
  assert_equal ["a0"; "a0"; "a1"; "a1"; "a2"; "a2"; "a3"; "a3"; "a4"; "a4"] (prefix 10 (stutter s_a));;
  assert_equal [("a0", "b0"); ("a0", "b0"); ("a1", "b1"); ("a1", "b1"); ("a2", "b2"); ("a2", "b2"); ("a3", "b3"); ("a3", "b3"); ("a4", "b4"); ("a4", "b4")] (prefix 10 (stutter (zip s_a s_b)));;
  assert_equal [0; 0; 0; 0; 1; 1; 1; 1; 2; 2; 2; 2; 3; 3; 3; 3; 4; 4; 4; 4] (prefix 20 (stutter (stutter nats)));;

let arctan_test test_ctxt =
  assert_equal [0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.] (prefix 10 (arctan 0.0));;
  assert_equal [1.; 0.666666666666666741; 0.866666666666666696; 0.723809523809523903; 0.834920634920635063; 0.744011544011544124; 0.820934620934621107; 0.754267954267954455; 0.813091483679719174; 0.760459904732350811] (prefix 10 (arctan 1.0));;
  assert_equal [0.5; 0.458333333333333315; 0.464583333333333293; 0.46346726190476184; 0.463684275793650735; 0.463639886589105266; 0.463649276613143702; 0.463647242107935342; 0.463647690895848952; 0.463647590509078777] (prefix 10 (arctan 0.5));;
  assert_equal [4.; 2.66666666666666696; 3.46666666666666679; 2.89523809523809561; 3.33968253968254025; 2.97604617604617649; 3.28373848373848443; 3.01707181707181782; 3.25236593471887669; 3.04183961892940324] (prefix 10 (scalef 4.0 (arctan 1.0)));;
  assert_equal 3.14259165433954424 (nth 1000 (scalef 4.0 (arctan 1.0)));;

let newton_test test_ctxt =
  assert_equal [1.; 0.666666666666666741; 0.666666666666666741; 0.666666666666666741; 0.666666666666666741; 0.666666666666666741; 0.666666666666666741; 0.666666666666666741; 0.666666666666666741; 0.666666666666666741] (prefix 10 (newton (fun x -> 3.0 *. x -. 2.0) (fun x -> 3.0) 1.0));;
  assert_equal [1.; 2.5; 2.05; 2.00060975609756087; 2.00000009292229475; 2.00000000000000222; 2.; 2.; 2.; 2.] (prefix 10 (sqrt 4.0));;
  assert_equal [1.; 5.; 3.4; 3.0235294117647058; 3.00009155413138; 3.00000000139698386; 3.; 3.; 3.; 3.] (prefix 10 (sqrt 9.0));;
  assert_equal [1.; 1.5; 1.41666666666666674; 1.41421568627450989; 1.41421356237468987; 1.41421356237309515; 1.41421356237309492; 1.41421356237309515; 1.41421356237309492; 1.41421356237309515] (prefix 10 (sqrt 2.0));;
  assert_equal [1.; 2.; 1.75; 1.73214285714285721; 1.7320508100147276; 1.73205080756887719; 1.73205080756887742; 1.73205080756887719; 1.73205080756887742; 1.73205080756887719] (prefix 10 (sqrt 3.0));;
  assert_equal [1.; 72.5; 37.2431034482758605; 20.5547955554420376; 13.7802299905638; 12.11499150672641; 12.0005457307424379; 12.0000000124086874; 12.; 12.] (prefix 10 (sqrt 144.0));;

let derivative_test test_ctxt =
  assert_equal [9.; 8.5; 8.33333333333332504; 8.25; 8.20000000000000284; 8.16666666666668561; 8.14285714285715656; 8.125; 8.11111111111107519; 8.09999999999998721] (prefix 10 d1);;
  assert_equal 8.00249376558566539 (nth 400 d1);;
  assert_equal 8.00099900099941408 (nth 1000 d1);;
  assert_equal [9.; 8.5; 8.33333333333332504; 8.25; 8.20000000000000284; 8.16666666666668561; 8.14285714285715656; 8.125; 8.11111111111107519; 8.09999999999998721] (prefix 10 (derivative (fun x -> x *. x +. 10.0) 4.0));;
  assert_equal [3.; 3.; 3.; 3.; 3.00000000000000711; 3.; 3.00000000000001421; 3.; 2.99999999999998934; 2.99999999999998934] (prefix 10 (derivative (fun x -> 3.0 *. x) 4.0));;
  assert_equal [26.; 21.75; 20.4444444444444606; 19.8125; 19.4400000000000261; 19.1944444444444144; 19.0204081632652766; 18.890625; 18.7901234567901305; 18.7100000000000222] (prefix 10 d2);;
  assert_equal 18.0348506225091967 (nth 200 d2);;
  assert_equal 18.0069940049968906 (nth 1000 d2);;

let limit_test test_ctxt =
  assert_equal [0.00316455696202531641; 0.00315457413249211347; 0.00314465408805031463; 0.00313479623824451398; 0.003125; 0.00311526479750778807; 0.00310559006211180103; 0.0030959752321981426; 0.00308641975308641958; 0.00307692307692307692] (prefix 10 (limit 0.00001 (map (fun n -> 1.0 /. (float (n+1))) nats)));;
  assert_equal [3.141592653623555; 3.14159265358860251; 3.14159265358983619; 3.14159265358979223; 3.141592653589794; 3.141592653589794; 3.141592653589794; 3.141592653589794; 3.141592653589794; 3.141592653589794] (prefix 10 (limit 0.0000000001 pi));;
  assert_equal [3.16227766016837952; 3.16227766016837908; 3.16227766016837952; 3.16227766016837908; 3.16227766016837952; 3.16227766016837908; 3.16227766016837952; 3.16227766016837908; 3.16227766016837952; 3.16227766016837908] (prefix 10 (limit 0.0000000001 (newton (fun x -> x *. x -. 10.0) (fun x -> 2.0 *. x) 1.0)));;
  assert_equal [8.00031625552450265; 8.00031615555879583; 8.00031605562637; 8.00031595577122445; 8.00031585596800454; 8.00031575622316637; 8.00031565656377097; 8.00031555696997; 8.00031545740871763; 8.00031535793515] (prefix 10 (limit 0.0000001 (derivative (fun x -> x *. x) 4.0)));;

let rev_prefixes_test test_ctxt =
  assert_equal [["a0"]; ["a1"; "a0"]; ["a2"; "a1"; "a0"]; ["a3"; "a2"; "a1"; "a0"]; ["a4"; "a3"; "a2"; "a1"; "a0"]; ["a5"; "a4"; "a3"; "a2"; "a1"; "a0"]; ["a6"; "a5"; "a4"; "a3"; "a2"; "a1"; "a0"]; ["a7"; "a6"; "a5"; "a4"; "a3"; "a2"; "a1"; "a0"]; ["a8"; "a7"; "a6"; "a5"; "a4"; "a3"; "a2"; "a1"; "a0"]; ["a9"; "a8"; "a7"; "a6"; "a5"; "a4"; "a3"; "a2"; "a1"; "a0"]] (prefix 10 (rev_prefixes s_a));;

let prefixes_test test_ctxt =
  assert_equal [["a0"]; ["a0"; "a1"]; ["a0"; "a1"; "a2"]; ["a0"; "a1"; "a2"; "a3"]; ["a0"; "a1"; "a2"; "a3"; "a4"]; ["a0"; "a1"; "a2"; "a3"; "a4"; "a5"]; ["a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"]; ["a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7"]; ["a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7"; "a8"]; ["a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7"; "a8"; "a9"]] (prefix 10 (prefixes s_a));;

let stripes_test test_ctxt =
  assert_equal [[("a0", "b0")]; [("a0", "b1"); ("a1", "b0")];
               [("a0", "b2"); ("a1", "b1"); ("a2", "b0")];
               [("a0", "b3"); ("a1", "b2"); ("a2", "b1"); ("a3", "b0")];
               [("a0", "b4"); ("a1", "b3"); ("a2", "b2"); ("a3", "b1"); ("a4", "b0")];
               [("a0", "b5"); ("a1", "b4"); ("a2", "b3"); ("a3", "b2"); ("a4", "b1");
                ("a5", "b0")];
               [("a0", "b6"); ("a1", "b5"); ("a2", "b4"); ("a3", "b3"); ("a4", "b2");
                ("a5", "b1"); ("a6", "b0")];
               [("a0", "b7"); ("a1", "b6"); ("a2", "b5"); ("a3", "b4"); ("a4", "b3");
                ("a5", "b2"); ("a6", "b1"); ("a7", "b0")];
               [("a0", "b8"); ("a1", "b7"); ("a2", "b6"); ("a3", "b5"); ("a4", "b4");
                ("a5", "b3"); ("a6", "b2"); ("a7", "b1"); ("a8", "b0")];
               [("a0", "b9"); ("a1", "b8"); ("a2", "b7"); ("a3", "b6"); ("a4", "b5");
                ("a5", "b4"); ("a6", "b3"); ("a7", "b2"); ("a8", "b1"); ("a9", "b0")]]
               (prefix 10 (stripes s_a s_b));;

let flatten_test test_ctxt =
  assert_equal [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] (prefix 10 (flatten (map (fun n -> [n]) nats)));;
  assert_equal [0; 1; 1; 2; 2; 3; 3; 4; 4; 5] (prefix 10 (flatten (map (fun n -> [n;n+1]) nats)));;
  assert_equal [0; 1; 2; 1; 2; 3; 2; 3; 4; 3] (prefix 10 (flatten (map (fun n -> [n;n+1;n+2]) nats)));;
  assert_equal [1; 2; 3; 2; 3; 4; 4; 5; 6; 5] (prefix 10 (flatten (map (fun n -> if n mod 3 = 0 then [] else [n;n+1;n+2]) nats)));;

let pairs_test test_ctxt =
  assert_equal [("a0", "b0"); ("a0", "b1"); ("a1", "b0"); ("a0", "b2"); ("a1", "b1");
                ("a2", "b0"); ("a0", "b3"); ("a1", "b2"); ("a2", "b1"); ("a3", "b0");
                ("a0", "b4"); ("a1", "b3"); ("a2", "b2"); ("a3", "b1"); ("a4", "b0");
                ("a0", "b5"); ("a1", "b4"); ("a2", "b3"); ("a3", "b2"); ("a4", "b1")]
               (prefix 20 (pairs s_a s_b));;

let suite =
  "suite">:::
    ["scale_test">::scale_test;
     "mult_test">::mult_test;
     "unzip_test">::unzip_test;
     "fold_test">::fold_test;
     "running_max_test">::running_max_test;
     "stutter_test">::stutter_test;
     "arctan_test">::arctan_test;
     "newton_test">::newton_test;
     "derivative_test">::derivative_test;
     "limit_test">::limit_test;
     "rev_prefixes_test">::rev_prefixes_test;
     "prefixes_test">::prefixes_test;
     "stripes_test">::stripes_test;
     "flatten_test">::flatten_test;
     "pairs_test">::pairs_test]

let () =
   run_test_tt_main suite
