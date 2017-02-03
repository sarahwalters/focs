(*

HOMEWORK 2

Name:

Email:

Remarks, if any:

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





(* Q1: Set functions *)

let rec inS e xs =
  match xs with
  | [] -> false
  | x::xs' -> if (compare e x == 0) then true else (inS e xs')


let rec subsetS xs ys =
  match xs with
  | [] -> true
  | x::xs' -> if (inS x ys) then (subsetS xs' ys) else false


let equalS xs ys =
  subsetS xs ys && subsetS ys xs


let rec unionS xs ys =
  match xs with
  | [] -> ys
  | x::xs' ->
    let rest = unionS xs' ys in
      if (inS x ys || inS x rest) then rest else x::rest


let rec interS xs ys =
  match xs with
  | [] -> []
  | x::xs' ->
    let rest = interS xs' ys in
      if (inS x ys) then x::rest else rest


let rec sizeS xs =
  match xs with
  | [] -> 0
  | x::xs' ->
    let rest = sizeS xs' in
      if (inS x xs') then rest else (rest + 1)




(* Q2: Language functions *)

let rec atMost n xs =
  match xs with
  | [] -> []
  | x::xs' ->
    let rest = atMost n xs' in
      if (String.length x <= n) then x::rest else rest


let unionL n xs ys =
  let xsLim = atMost n xs in
  let ysLim = atMost n ys in
    unionS xsLim ysLim


let rec concatL n xs ys =
  match xs with
  | [] -> []
  | x::xs' ->
    (match ys with
      | [] -> []
      | y::ys' ->
        let rest = (unionS (concatL n xs ys') (concatL n xs' ys)) in
        let xy = (x^y) in
          if (String.length xy <= n) then xy::rest else rest
    )


let rec starL n xs =
  if (n == 0) then
    [""]
  else
    let smaller = starL (n - 1) xs in
      unionL n (concatL n xs smaller) smaller



(* Q3: regular expressions *)

type re =
    Empty
  | Unit
  | Letter of string
  | Plus of re * re
  | Times of re * re
  | Star of re

let lang n s =
  let fromChar c = String.make 1 c in
  let explode s =
    let rec loop i result =
      if i < 0 then result
      else loop (i-1) (s.[i]::result) in
    loop (String.length s - 1) []  in
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
    | Plus (r1,r2) -> unionL n (eval r1) (eval r2)
    | Times (r1,r2) -> concatL n (eval r1) (eval r2)
    | Star r -> starL n (eval r)  in
  eval (parse s)

let show l =
  let rec loop l seen =
    match l with
    | [] -> ()
    | s::rest -> if List.mem s seen
                    then loop rest seen
                  else (match s with
		        | "" -> (print_string "  <empty string>\n";
				 loop rest (""::seen))
			| s -> (print_string ("  "^s^"\n");
				loop rest (s::seen))) in
  loop l []



let regexp_a = "(d+e)(d+e)(d+e)(d+e)"

let regexp_b = "(d+e)((d+e)(d+e))*"

let regexp_c = "e*de*de*"

let regexp_d = "e*de*(de*d)*e*"

let regexp_e = "e*(dee)*e*"

