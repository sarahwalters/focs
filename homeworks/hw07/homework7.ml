(*

HOMEWORK 7

Name: Sarah Walters

Email: sarah.walters@students.olin.edu

Remarks, if any: Took ~5h
I answered the last question three times. The final version (powers2) I'm happy with; the others
I'm including in case you're curious or in case I want them for reference in the future.

The first draft (powers2limited) generates sequentially larger nonterminal exponents,
then translates to terminal a's at the end. It's fast, but it can't generate arbitrarily long
strings of a's.

The second draft (powers2turing) simulates a Turing machine. In theory it works -- I know it works
for "a" and "aa", but it's too slow to run in a reasonable amount of time on longer input strings.
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


(*
 * Type for grammars
 *
 *)

type grammar = {
  nonterminals: string list;
  terminals: string list;
  rules: (string * string) list;
  startsym : string
}


(*
 * Some sample (context-free) grammars
 *
 *)

let anbn = {
  nonterminals = ["S"];
  terminals = ["a";"b"];
  rules = [("S","");
           ("S","aSb")];
  startsym = "S"
}

let anbm = {
  nonterminals = ["S";"T";"B"];
  terminals = ["a";"b"];
  rules = [ ("S","TB");
            ("T","");
            ("T","aTb");
            ("B","");
            ("B","Bb")];
  startsym = "S"
}


(*
 * Here's a grammar that is _not_ context-free
 *
 * It's also harder to generate its strings
 *
 *)

let anbncn = {
  nonterminals = ["S";"A";"B";"C";"X"];
  terminals = ["a";"b";"c"];
  rules = [ ("S","");
            ("S","ABC");
            ("bX","Xb");
            ("AX","a");
            ("aX","Xa");
            ("XC","c");
            ("Xc","cX");
            ("B","XbBX");
            ("B","");
            ("A","AA");
            ("C","CC")];
  startsym = "S"
}




(* abbreviations *)

let map = List.map
let len = String.length
let sub = String.sub


(*
 * Utility functions
 *
 *)


(* check is lhs is a prefix of str *)

let prefix lhs str =
  lhs = (sub str 0 (len lhs))


(* replace prefix lhs of str with rhs *)

let replace lhs str rhs =
  let l = len lhs in
  rhs ^ (sub str l (len str - l))


(* try to apply rule (lhs,rhs) to str (assuming prefix prf) *)

let apply_rule prf (lhs,rhs) str =
  if len str < len lhs
    then []
  else if prefix lhs str
    then [prf^(replace lhs str rhs)]
  else []


(* try to apply every rule in rs to str *)

let rec apply_rules rs str =
  let rec loop prefix str =
    if str = "" then []
    else let rest = loop (prefix^(sub str 0 1)) (sub str 1 (len str -1))  in
       (List.fold_left (fun res r -> res@(apply_rule prefix r str)) [] rs)@rest  in
  loop "" str


(*
 * Perform an iteratively deepening depth-first search of the rewrite
 * tree
 *
 *)

module StringSet = Set.Make(String)

let dfs_path maxdepth maxwidth grammar target =
  let lt = len target  in
  let rec loop q seen =
    if q = []
      then []
    else let ((path,d)::q) = q in
         let (str::_) = path in
   if len str > maxwidth
     then loop q seen
         else if len str = lt && str = target
     then path
   else if StringSet.mem str seen
     then loop q seen
   else if d > maxdepth
     then loop q (StringSet.add str seen)
   else (* let _ = (print_string str; print_newline()) in *)
        let new_strs = apply_rules grammar.rules str in
        let new_strs_d = map (fun x -> (x::path,d+1)) new_strs in
        let q = (new_strs_d)@q in
        loop q (StringSet.add str seen) in
  loop [([grammar.startsym],0)] StringSet.empty

let idfs_path maxdepth grammar target =
  let rec loop n =
    let _ = Printf.printf "Searching (depth %02d, max width %d)" n n in
    let _ = print_newline ()  in
    if n > maxdepth
      then []
    else match dfs_path n n grammar target with
         | [] -> loop (n+1)
   | path -> path  in
  loop 1


(*
 * Check if a grammar is well-formed
 *
 *)

let checkGrammar grammar =
  let _ = List.iter (fun x -> if String.length x != 1 then failwith ("symbol "^x^" not a single character") else ()) grammar.nonterminals  in
  let _ = List.iter (fun x -> if String.length x != 1 then failwith ("symbol "^x^" not a single character") else ()) grammar.terminals  in
  let _ = List.iter (fun (p,q) -> if String.length p < 1 then failwith "rule with empty left-hand side" else ()) grammar.rules  in
  let _ = if List.mem grammar.startsym grammar.nonterminals then () else failwith "start symbol not a nonterminal"  in
  ()



(*
 * Try to generate a string for a given grammar
 *
 *)

let generate md grammar str =
  let _ = checkGrammar grammar in
  let print pre str = (print_string pre;
                       print_string str;
           print_newline ())  in
  let rec rev_print path =
    match path with
    | [] -> ()
    | [s] -> print "   " s
    | s::ss -> (rev_print ss; print "-> " s)  in
  let path = idfs_path md grammar str  in
  let _ = rev_print path  in
  path != []



(*
 * QUESTION 1
 *
 *)


let palindromes = {
  nonterminals = ["S"];
  terminals = ["a"; "b"; "c"];
  rules = [("S", "");
           ("S", "aSa");
           ("S", "bSb");
           ("S", "cSc");
           ("S", "a");
           ("S", "b");
           ("S", "c")];
  startsym = "S"
}


let ambncmn = {
  nonterminals = ["S"; "X"];
  terminals = ["a"; "b"; "c"];
  rules = [("S", "aSc");
           ("S", "X");
           ("X", "");
           ("X", "bXc")];
  startsym = "S"
}


let amcmnbn = {
  nonterminals = ["S"; "A"; "B"];
  terminals = ["a"; "b"; "c"];
  rules = [("S", "AB");
           ("A", "");
           ("B", "");
           ("A", "aAc");
           ("B", "cBb")];
  startsym = "S"
}


let ambncm = {
  nonterminals = ["S"; "B"];
  terminals = ["a"; "b"; "c"];
  rules = [("S", "aSc");
           ("S", "B");
           ("B", "Bb");
           ("B", "")];
  startsym = "S"
}


let eqnum = {
  nonterminals = ["S"];
  terminals = ["d"; "e"];
  rules = [("S", "");
           ("S", "dSe"); (* rules include all 6 permutations of the symbols S, e, and d *)
           ("S", "eSd");
           ("S", "deS");
           ("S", "edS");
           ("S", "Sde");
           ("S", "Sed")];
  startsym = "S"
}



(*
 * QUESTION 2
 *
 *)

(* Type for DFAs *)

type 'a dfa = {
  states: 'a list;
  alphabet: char list;
  delta: 'a -> char -> 'a;
  start : 'a;
  accepting : 'a list
}


(* A dfa that accepts all strings with a multiple of three
 * number of as *)

let dfaThreeA = {
  states = ["S";"1";"2"];
  alphabet = ['a';'b'];
  delta = (fun q a ->
            match (q,a) with
              ("S",'a') -> "1"
            | ("1",'a') -> "2"
            | ("2",'a') -> "S"
            | ("S",'b') -> "S"
            | ("1",'b') -> "1"
            | ("2",'b') -> "2"
            | _ -> "");
  start = "S";
  accepting = ["S"]
}



let dfaGrammar dfa =
  let transitionRules =
    List.fold_right (fun state res ->
      (List.map (fun letter ->
        let q = dfa.delta state letter in
          (state, Char.escaped letter ^ q)
      ) dfa.alphabet) @ res
    ) dfa.states []
  in let acceptRules =
    List.map (fun accState -> (accState, "")) dfa.accepting
  in {
    nonterminals = dfa.states;
    terminals = List.map (fun c -> Char.escaped c) dfa.alphabet;
    rules = transitionRules @ acceptRules;
    startsym = dfa.start;
  }



(*
 * QUESTION 3
 *
 *)


(* ADDITION
   Sequence of rewrites for xx+xxx=xxxxx:
   S-> XSx
    -> xSx
    -> xXSxx
    -> xxSxx
    -> xxXSxxx
    -> xxXXSxxxx
    -> xxXXXSxxxxx
    -> xxXXX+=xxxxx
    -> xxXX+X=xxxxx
    -> xxX+XX=xxxxx
    -> xx+XXX=xxxxx
    -> xx+xXX=xxxxx
    -> xx+xxX=xxxxx
    -> xx+xxx=xxxxx

   Sequence of rewrites for xxx+xx=xxxxx:
   S-> XSx
    -> xSx
    -> xXSxx
    -> xxSxx
    -> xxXSxxx
    -> xxxSxxx
    -> xxxXSxxxx
    -> xxxXXSxxxxx
    -> xxxXX+=xxxxx
    -> xxxX+X=xxxxx
    -> xxx+XX=xxxxx
    -> xxx+xX=xxxxx
    -> xxx+xx=xxxxx
*)
let addition = {
  nonterminals = ["S"; "X"];
  terminals = ["x"; "+"; "="];
  rules = [("S", "XSx");
           ("S", "+=");
           ("X+", "+X");
           ("X", "x")];
  startsym = "S"
}


(* POWERS2 FINAL
   sequence of rewrites for aaaa:
   S-> BXaE
    -> BXXaE
    -> XXaE
    -> XaaXE
    -> aaXaXE
    -> aaaaXXE
    -> aaaaXE
    -> aaaa

   sequence of rewrites for aaaaaaaa:
   S-> BXaE
    -> BXXaE
    -> BXXXaE
    -> XXXaE
    -> XXaaXE
    -> XaaXaXE
    -> aaXaXaXE
    -> aaaaXXaXE
    -> aaaaXaaXXE
    -> aaaaaaXaXXE
    -> aaaaaaaaXXXE
    -> aaaaaaaaXXE
    -> aaaaaaaaXE
    -> aaaaaaaa
*)
let powers2 = {
  nonterminals = ["B";"X";"E";"S"];
  terminals = ["a"];
  rules = [("S", "BXaE");
           ("S", "a");
           ("Xa", "aaX");
           ("XE", "");
           ("B", "BX");
           ("B", "");
           ("XXE", "XE")];
  startsym = "S"
}


(* POWERS2 ATTEMPT 1
   sequence of rewrites for aaaa:
   0-> 1
    -> 2
    -> aaaa

   sequence of rewrites for aaaaaaaa:
   0-> 1
    -> 2
    -> 3
    -> aaaaaaaa
*)
let rec pow b n = (* computes b^n for n > 0*)
  if (n < 0) then failwith "n not in range"
  else if (n = 0) then 1
  else b * pow b (n-1)

let rec repeatA n =
  if (n = 0) then ""
  else "a" ^ repeatA (n-1)

let powers2limited =
  let numbers = [0;1;2;3;4;5;6;7;8]
  in let transitionRules = List.map (fun number -> (string_of_int number, string_of_int (number + 1))) numbers (* do transition work in the exponent domain *)
  in let terminalRules = List.map (fun number -> (string_of_int number, repeatA (pow 2 number))) numbers (* convert to nonterminals at end *)
  in {
    nonterminals = List.map (fun number -> string_of_int number) numbers;
    terminals = ["a"];
    rules = transitionRules @ terminalRules;
    startsym = "0"
  }


(* POWERS2 ATTEMPT 2
   sequence of rewrites for aa:
   A-> SB
    -> S[>,>]B
    -> [>,>]NB
    -> [>,>]N[a,a]B
    -> [>,>][a,a]1B
    -> [>,>][a,a]1[a,a]B
    -> [>,>][a,a][a,X]EB
    -> [>,>][a,a][a,X]E[_,_]B
    -> [>,>][a,a]R[a,X][_,_]B
    -> [>,>]R[a,a][a,X][_,_]B
    -> R[>,>][a,a][a,X][_,_]B
    -> [>,>]N[a,a][a,X][_,_]B
    -> [>,>][a,a]1[a,X][_,_]B
    -> [>,>][a,a][a,X]1[_,_]B
    -> [>,>][a,a][a,X]D[_,_]B
    -> [>,>][a,a]DaD[_,_]B
    -> [>,>]DaDaD[_,_]B
    -> DaDaD[_,_]B
    -> aDaD[_,_]B
    -> aaD[_,_]B
    -> aaD_DB
    -> aa_DB
    -> aaDB
    -> aaB
    -> aaC
    -> aa
*)
let powers2turing = {
  nonterminals = ["A";"B";"C";"S";"N";"1";"E";"O";"R";"D";"[";"]";">";","];
  terminals = ["a"];
  rules = [(* setup rules *)
           ("A", "SB");
           ("B", "[>,>]B");
           ("B", "[a,a]B");
           ("B", "[_,_]B");
           ("B", "C");
           ("C", "");

           (* TM transitions *)
           ("S[>,>]", "[>,>]N");

           ("N[a,a]", "[a,a]1");
           ("N[a,X]", "[a,X]N");

           ("1[a,a]", "[a,X]E");
           ("1[a,X]", "[a,X]1");
           ("1[_,_]", "D[_,_]");

           ("E[a,a]", "[a,a]O");
           ("E[a,X]", "[a,X]E");
           ("[a,X]E[_,_]", "R[a,X][_,_]");

           ("O[a,X]", "[a,X]O");
           ("O[a,a]", "[a,X]E");

           ("[a,a]R[a,X]", "R[a,a][a,X]");
           ("[>,>]R[a,a]", "R[>,>][a,a]");
           ("R[>,>]", "[>,>]N");

           (* finish rules *)
           ("D[_,_]", "D_D");
           ("_", "");
           ("D","");
           ("[a,a]D", "DaD");
           ("[a,X]D", "DaD");
           ("[>,>]D", "D")
          ];
  startsym = "A"
}
