open OUnit2;;
open Homework10;;

let mapFn = fun x -> x * x;;
let unmapped1 = Node(2,Empty,Empty);;
let mapped1 = Node(4,Empty,Empty);;
let unmapped2 = (Node(2,Node(3,Empty,Empty),Node(4,Empty,Empty)));;
let mapped2 = Node(4,Node(9,Empty,Empty),Node(16,Empty,Empty));;
let mappedSample = Node(100,
                    Node(9,
                      Node(49,Empty,Empty),
                      Node(25,Empty,Empty)),
                    Node(36,
                      Node(9801,
                        Empty,
                        Node(4356,Empty,Empty)),
                      Empty));;

let sample1 = (Node(1,Empty,Empty));;
let sample2 = (Node(1,Node(2,Empty,Empty),Node(3,Empty,Empty)));;

let lookup1 = (Node (5,Empty,Empty));;
let lookup2 = (Node (5,Node(2,Empty,Empty),Node(8,Empty,Empty)));;

let size_test test_ctxt =
  assert_equal 0 (size Empty);;
  assert_equal 1 (size sample1);;
  assert_equal 7 (size sample);;

let height_test test_ctxt =
  assert_equal 0 (height Empty);;
  assert_equal 1 (height sample1);;
  assert_equal 4 (height sample);;

let sum_test test_ctxt =
  assert_equal 0 (sum Empty);;
  assert_equal 1 (sum sample1);;
  assert_equal 196 (sum sample);;

let fringe_test test_ctxt =
  assert_equal [] (fringe Empty);;
  assert_equal [1] (fringe sample1);;
  assert_equal [2; 3] (fringe sample2);;
  assert_equal [7; 5; 66] (fringe sample);;

let map_test test_ctxt =
  assert_equal Empty (map mapFn Empty);;
  assert_equal mapped1 (map mapFn unmapped1);;
  assert_equal mapped2 (map mapFn unmapped2);;
  assert_equal mappedSample (map mapFn sample);;

let fold_test test_ctxt =
  assert_equal 0 (fold (fun v l r -> v+l+r) Empty 0);;
  assert_equal 1 (fold (fun v l r -> v+l+r) sample1 0);;
  assert_equal 21 (fold (fun v l r -> v+l+r) sample1 10);;
  assert_equal 6 (fold (fun v l r -> v+l+r) sample2 0);;
  assert_equal 196 (fold (fun v l r -> v+l+r) sample 0);;

let preorder_test test_ctxt =
  assert_equal [] (preorder Empty);;
  assert_equal [1] (preorder sample1);;
  assert_equal [1; 2; 3] (preorder sample2);;
  assert_equal [10; 3; 7; 5; 6; 99; 66] (preorder sample);;

let postorder_test test_ctxt =
  assert_equal [] (postorder Empty);;
  assert_equal [1] (postorder sample1);;
  assert_equal [2; 3; 1] (postorder sample2);;
  assert_equal [7; 5; 3; 66; 99; 6; 10] (postorder sample);;

let inorder_test test_ctxt =
  assert_equal [] (inorder Empty);;
  assert_equal [1] (inorder sample1);;
  assert_equal [2; 1; 3] (inorder sample2);;
  assert_equal [7; 3; 5; 10; 99; 66; 6] (inorder sample);;

let bst_insert_test test_ctxt = 
  assert_equal [] (inorder Empty);; (* TODO *)

let bst_lookup_test test_ctxt =
  assert_equal false (bst_lookup Empty 1);;
  assert_equal false (bst_lookup lookup1 1);;
  assert_equal true (bst_lookup lookup1 5);;
  assert_equal false (bst_lookup lookup2 1);;
  assert_equal true (bst_lookup lookup2 2);;
  assert_equal true (bst_lookup lookup2 8);;

let bstify_test test_ctxt = 
  assert_equal [] (inorder Empty);; (* TODO *)

let avl_insert_test test_ctxt = 
  assert_equal [] (inorder Empty);; (* TODO *)

let suite =
  "suite">:::
    ["size_test">::size_test;
     "height_test">::height_test;
     "sum_test">::sum_test;
     "fringe_test">::fringe_test;
     "map_test">::map_test;
     "fold_test">::fold_test;
     "preorder_test">::preorder_test;
     "postorder_test">::postorder_test;
     "inorder_test">::inorder_test;
     "bst_insert_test">::bst_insert_test;
     "bst_lookup_test">::bst_lookup_test;
     "bstify_test">::bstify_test;
     "avl_insert_test">::avl_insert_test]

let () =
   run_test_tt_main suite
