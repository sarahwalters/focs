(*

HOMEWORK 2

Name: Sarah Walters

Email: sarah.walters@students.olin.edu

Remarks, if any: Took ~2h

*)


(*
 *
 * Please fill in this file with your solutions and submit it
 *
 * The functions below are stubs that you should replace with your
 * own implementation.
 *
 * Always make sure you can #use this file before submitting it.
 * It has to load without any errors.
 *
 *)

(* HELPERS *)

let rec join (list1, list2) =
   match list1 with
      | [] -> list2
      | head::tail -> head :: join (tail, list2)


let rec setIn (e,xs) =
   match xs with
      | [] -> false
      | first::rest ->
         if (e = first) then true
         else setIn (e, rest)


let rec unique (xs) =
   match xs with
      | [] -> []
      | head::tail ->
         let unique_tail = unique (tail) in
            if setIn (head, unique_tail) then unique_tail
            else head::unique_tail


let rec setSub (xs,ys) =
   match xs with
      [] -> true |
      first::rest ->
         let thisElt = setIn (first, ys) in
            if (rest = []) then thisElt
            else thisElt && setSub (rest, ys)


let setEqual (xs,ys) =
   setSub (xs, ys) && setSub (ys, xs)



(* QUESTION 1 *)


let rec prepend (letter, lang) =
   match lang with
      | [] -> []
      | head::tail -> (letter^head) :: prepend (letter, tail)


let rec concatenate (alphabet, lang) =
   match alphabet with
      | [] -> []
      | head::tail -> join (prepend (head, lang), concatenate (tail, lang))


let rec all_strings (alphabet, n) =
   if (n = 0) then [""]
   else "" :: concatenate (alphabet, all_strings (alphabet, n-1))



(* QUESTION 2 *)

let rec restrict (xs,n) =
   match xs with
      | [] -> []
      | head::tail ->
         let restricted_tail = restrict(tail, n) in
            if (String.length head <= n) then head :: restricted_tail
            else restricted_tail


let langUnion (xs,ys,n) =
   unique (join (restrict (xs, n), restrict (ys, n)))


let langConcat (xs,ys,n) =
   unique (restrict (concatenate (xs, ys), n))


let langStar (xs,n) =
   unique (restrict (all_strings (xs, n), n))



(* QUESTION 3 *)


(* some helper code -- vaguely advanced OCaml in here, but barely *)

type re = Empty | Unit | Letter of string | Plus of re * re | Times of re * re | Star of re

let lang (s,n) =
  let fromChar c = String.make 1 c in
  let explode s =
    let rec loop i result =
      if i < 0 then result
      else loop (i-1) (s.[i]::result) in
    loop (String.length s - 1) []  in
  (* Grammar:
   *
   * R ::= R1 + R
   *       R1
   *
   * R1 ::= R2 R1
   *        R2
   *
   * R2 ::= R3*
   *        R3
   *
   * R3 ::= a
   *        1
   *        0
   *        ( R )
   *)
  let isalpha = function 'A'..'Z'|'a'..'z' -> true | _ -> false in
  let expect c cs =
    match cs with
      f::cs when f = c -> Some cs
    | _ -> None in
  let expect_alpha cs =
    match cs with
      f::cs when isalpha f -> Some (f,cs)
    | _ -> None  in
  let rec parse_R cs =
    match parse_R1 cs with
      None -> None
    | Some (r1,cs) ->
        (match expect '+' cs with
           None -> Some (r1,cs)
         | Some cs ->
             (match parse_R cs with
                None -> None
              | Some (r2,cs) -> Some (Plus(r1,r2),cs)))
  and parse_R1 cs =
    match parse_R2 cs with
      None -> None
    | Some (r1,cs) ->
        (match parse_R1 cs with
           None -> Some (r1,cs)
         | Some (r2,cs) -> Some (Times(r1,r2),cs))
  and parse_R2 cs =
    match parse_R3 cs with
      None -> None
    | Some (r1,cs) ->
        (match expect '*' cs with
           None -> Some (r1,cs)
         | Some cs -> Some (Star(r1),cs))
  and parse_R3 cs =
    match expect_alpha cs with
      Some (a,cs) -> Some (Letter(fromChar(a)),cs)
    | None ->
        (match expect '1' cs with
           Some cs -> Some (Unit, cs)
         | None ->
             (match expect '0' cs with
                Some cs -> Some (Empty,cs)
              | None -> parse_parens cs))
  and parse_parens cs =
    match expect '(' cs with
      None -> None
    | Some cs ->
        (match parse_R cs with
           None -> None
         | Some (r,cs) ->
             (match expect ')' cs with
                None -> None
              | Some cs -> Some (r,cs)))  in
  let parse s =
    let cs = explode s in
    match parse_R cs with
      Some (re,[]) -> re
    | _ -> failwith ("Cannot parse "^s)  in
  let rec eval re =
    match re with
      Empty -> []
    | Unit -> [""]
    | Letter (a) -> [a]
    | Plus (r1,r2) -> langUnion(eval r1,eval r2,n)
    | Times (r1,r2) -> langConcat(eval r1,eval r2,n)
    | Star r -> langStar(eval r,n)  in
    eval (parse s)

let dump l =
  List.iter (fun s -> match s with "" -> print_string "  <empty>\n"
                                 | s -> print_string ("  "^s^"\n")) l



(* Placeholder for your regular expression. Replace "0" by your actual answer *)

let regexp_a = "(a+b)(a+b)(a+b)"

let regexp_b = "((a+b)(a+b)(a+b))*"

let regexp_c = "b*ab*"

let regexp_d = "((b*ab*ab*)+b)*a((b*ab*ab*)+b)*"

let regexp_e = "(a*+(ba)*)*"
