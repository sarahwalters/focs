(***************************************************

HOMEWORK 4

Name: Sarah Walters

Email: sarah.walters@students.olin.edu

Remarks, if any: Took ~2h

***************************************************)



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


(*
 * String <-> characters utility functions:
 *
 *   explode : string -> char list
 *      returns the list of characters making up a string
 *
 *   implode : char list -> string
 *      concatenates the list of characters into a string
 *
 *)

let explode str =
  let rec acc index result =
    if (index<0) then result
    else acc (index-1) ((String.get str index)::result) in
  acc (String.length(str)-1) []

let implode cs =
  List.fold_right (fun a r -> (String.make 1 a)^r) cs ""


(*
 * Set utility functions
 *
 *   setIn : 'a list -> 'a -> bool
 *      returns true if element in list, false otherwise
 *)

let rec setIn lst elt =
  match lst with
  | [] -> false
  | head::tail -> if (head = elt) then true else setIn tail elt

(*
 *  The type of a DETERMINISTIC finite automaton
 *
 *  Note that the delta here is _actually_ a function
 *  from states and symbols to states
 *
 *)

type 'a dfa = { states: 'a list;
		alphabet: char list;
		delta: 'a -> char -> 'a;
		start : 'a;
		accepting : 'a list }


(*
 *  A sample DFA that accepts all strings over
 *  {a,b} with a multiple-of-3 number of a's
 *
 *)

let dfaThreeA = {
  states = ["start";"one";"two"];
  alphabet = ['a';'b'];
  delta = (fun q a ->
             match (q,a) with
	       ("start",'a') -> "one"
	     | ("one",'a') -> "two"
	     | ("two",'a') -> "start"
	     | ("start",'b') -> "start"
	     | ("one",'b') -> "one"
	     | ("two",'b') -> "two");
  start = "start";
  accepting = ["start"]
}




(* QUESTION 1 *)


let isAccepting dfa s =
  setIn dfa.accepting s


let rec steps dfa q syms =
  match syms with
  | [] -> q
  | sym::syms' -> steps dfa (dfa.delta q sym) syms'


let acceptDFA dfa input =
  isAccepting dfa (steps dfa dfa.start (explode input))


(* This function loops through all the strings
 * of length up to n, and prints those that are accepted by the
 * finite automaton.
 *
 * This is basically the same as in the last homework
 *)

let langDFA dfa n =
  let rec expt a n = if n <= 0 then 1 else a*(expt a (n-1)) in
  let rec take n default l =
    if n <= 0 then []
    else (match l with
          | [] -> default::(take (n-1) default l)
          | x::xs -> x::(take (n-1) default xs)) in
  let to_base_n base size n =
    let rec loop n =
      if n <= 0 then []
      else if n mod base = 0 then 0::(loop (n / base))
      else (n mod base)::(loop ((n - n mod base) / base))  in
    take size 0 (loop n)  in
  let to_string alphabet size n =
    let base = List.length alphabet in
    let num_base = to_base_n base size n in
    implode (List.map (fun i -> List.nth alphabet i) num_base) in
  if n < 0 then ()
  else
    let print_str s = if s = "" then print_string "  <epsilon>\n"
                      else print_string ("  "^s^"\n")  in
    let rec loop i =
      if i <= n then
	let ts = to_string dfa.alphabet i  in
  	let bound = expt (List.length dfa.alphabet) i in
  	let rec loop2 j =
  	  if j < bound then (if acceptDFA dfa (ts j)
                               then print_str (ts j)
                             else ();
  			     loop2 (j+1))
  	  else ()  in
  	(loop2 0; loop (i+1))
      else ()  in
      loop 0





(* QUESTION 2 *)

let at_least n p xs =
  List.length (List.filter p xs) >= n


let max_positive xs =
  List.fold_right (fun x res -> if (x > res && x > 0) then x else res) xs 0


let map_funs fs x =
  List.map (fun f -> f x) fs


let map_cross fs xs =
  List.fold_right (fun x res -> map_funs fs x @ res) xs []


let all_pairings xs ys =
  List.fold_right (fun x res -> (List.map (fun y -> (x,y)) ys) @ res) xs []


(* QUESTION 3 *)

let rec except_last xs = (* is this helper ok? *)
  match xs with
  | [] -> []
  | head::[] -> []
  | head::tail -> head :: except_last tail


let prefixes xs =
  List.fold_right (fun x res -> except_last (List.hd res) :: res) xs [xs]


let suffixes xs =
  List.fold_right (fun x res -> (x :: List.hd res) :: res) xs [[]]


let inject a xs =
  List.fold_right (fun (p, s) res -> (p @ [a] @ s) :: res) (List.combine (prefixes xs) (suffixes xs)) []


let permutations xs =
  List.fold_right (fun x res -> (List.fold_right (fun perm perms -> (inject x perm) @ perms) res [])) xs [[]]
