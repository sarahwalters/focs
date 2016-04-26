(*

HOMEWORK 10

Name: Sarah Walters

Email: sarah.walters@students.olin.edu

Remarks, if any: Took ~3hr, mostly on AVL insert

*)


(*
 *
 * Please fill in this file with your solutions and submit it
 *
 * The functions below are stubs that you should replace with your
 * own implementation.
 *
 * Always make sure you can #use this file before submitting it.
 * Do that in a _fresh_ OCaml shell
 * It has to load without any errors.
 *
 *)


type 'a bintree =
  | Empty
  | Node of 'a * 'a bintree * 'a bintree


let sample = Node(10,Node(3,Node(7,Empty,Empty),
                            Node(5,Empty,Empty)),
                     Node(6,Node(99,Empty,
                                 Node(66,Empty,Empty)),
                          Empty))


(* Printing an integer binary tree *)

let pbt bt =
  let rec loop bt depth =
    match bt with
    | Empty -> ()
    | Node(n,left,right) ->
	(loop right (depth^"    ");
         print_endline (depth^(string_of_int n));
         loop left (depth^"    ")) in
  loop bt ""


(* Q1 *)

let rec size t =
  match t with
  | Empty -> 0
  | Node(n,left,right) ->
      1 + size left + size right


let rec sum t =
  match t with
  | Empty -> 0
  | Node(n,left,right) ->
      n + sum left + sum right


let rec height t =
  match t with
  | Empty -> 0
  | Node(n,left,right) ->
      1 + max (height left) (height right)


let rec fringe t =
  match t with
  | Empty -> []
  | Node(n,left,right) ->
      if (left = Empty && right = Empty) then [n]
      else (fringe left) @ (fringe right)


let rec map f t =
  match t with
  | Empty -> Empty
  | Node(n,left,right) ->
      let mapped_left = map f left in
      let mapped_right = map f right in
        Node(f n, mapped_left, mapped_right)


let rec fold f t b =
  match t with
  | Empty -> b
  | Node(n,left,right) ->
      let folded_left = fold f left b in
      let folded_right = fold f right b in
        f n folded_left folded_right


let preorder t =
  fold (fun v l r -> [v] @ l @ r) t []


let postorder t =
  fold (fun v l r -> l @ r @ [v]) t []


let inorder t =
  fold (fun v l r -> l @ [v] @ r) t []


let rec bst_insert t x =
  match t with
  | Empty -> Node(x, Empty, Empty)
  | Node(n,left,right) ->
      if (x <= n) then
        let new_left = bst_insert left x in
          Node(n, new_left, right)
      else
        let new_right = bst_insert right x in
          Node(n, left, new_right)


(* helper function, used by test suite *)
let rec is_bst t =
  match t with
  | Empty -> true
  | Node(n,left,right) ->
      let left_check =
        match left with
        | Empty -> true
        | Node(l,_,_) -> (l <= n) && (is_bst left)
      in let right_check =
        match right with
        | Empty -> true
        | Node(r,_,_) -> (n < r) && (is_bst right)
      in left_check && right_check


let rec bst_lookup t x =
  match t with
  | Empty -> false
  | Node(n,left,right) ->
      if (x < n) then bst_lookup left x
      else if (x > n) then bst_lookup right x
      else true


let bstify t = (* this is naive *)
  let rec bstify_arr arr =
    match arr with
    | [] -> Empty
    | x::[] -> Node(x, Empty, Empty)
    | x::xs -> bst_insert (bstify_arr xs) x
  in bstify_arr (inorder t)


(* AVL tree rotate diagrams from http://www.geeksforgeeks.org/avl-tree-set-1-insertion/ *)
(*
         z                                      y
        / \                                   /   \
       y   T4      Right Rotate              x      z
      / \          - - - - - - - - ->      /  \    /  \
     x   T3                               T1  T2  T3  T4
    / \
  T1   T2
*)
let avl_leftleft z sub_y t4 =
  match sub_y with (* sub_y is the subtree rooted at y *)
  | Empty -> failwith "Left-left: first left rotate failed"
  | Node(y,sub_x,t3) ->
      match sub_x with (* sub_x is the subtree rooted at x *)
      | Empty -> failwith "Left-left: second left rotate failed"
      | Node(x,t1,t2) -> Node(y, Node(x,t1,t2), Node(z,t3,t4))

(*
     z                              x
    / \                            /  \
   y   T4  Left Right Rotate     y      z
  / \      - - - - - - - - ->   / \    / \
T1   x                        T1  T2 T3  T4
    / \
  T2   T3
*)
let avl_leftright z sub_y t4 =
  match sub_y with
  | Empty -> failwith "Left-right: left rotate failed"
  | Node(y,t1,sub_x) ->
      match sub_x with
      | Empty -> failwith "Left-right: right rotate failed"
      | Node(x,t2,t3) -> Node(x, Node(y,t1,t2), Node(z,t3,t4))

(*
  z                                y
 /  \                            /   \
T1   y     Left Rotate          z      x
    /  \   - - - - - - - ->    / \    / \
   T2   x                     T1  T2 T3  T4
       / \
     T3  T4
*)
let avl_rightright z t1 sub_y =
  match sub_y with
  | Empty -> failwith "Right-right: first right rotate failed"
  | Node(y,t2,sub_x) ->
      match sub_x with
      | Empty -> failwith "Right-right: second right rotate failed"
      | Node(x,t3,t4) -> Node(y, Node(z,t1,t2), Node(x,t3,t4))

(*
   z                               x
  / \                             /  \
T1   y   Right Left Rotate      z      y
    / \  - - - - - - - - ->    / \    / \
   x   T4                    T1  T2  T3  T4
  / \
T2   T3
*)
let avl_rightleft z t1 sub_y =
  match sub_y with
  | Empty -> failwith "Right-left: right rotate failed"
  | Node(y,sub_x,t4) ->
      match sub_x with
      | Empty -> failwith "Right-left: left rotate failed"
      | Node(x,t2,t3) -> Node(x, Node(z,t1,t2), Node(y,t3,t4))

let rec avl_insert t x =
  match t with
  | Empty -> Node(x, Empty, Empty)
  | Node(n,left,right) ->
      if (x <= n) then (* inserting left -- left left & right left rotate cases *)
        let new_left = avl_insert left x in
          let lh = height new_left in
          let rh = height right in
            if (lh - rh > 1) then avl_leftleft n new_left right
            else if (rh - lh > 1) then avl_rightleft n new_left right
            else Node(n, new_left, right)
      else (* inserting right -- left right & right right rotate cases *)
        let new_right = avl_insert right x in
          let lh = height left in
          let rh = height new_right in
            if (lh - rh > 1) then avl_leftright n left new_right
            else if (rh - lh > 1) then avl_rightright n left new_right
            else Node(n, left, new_right)
