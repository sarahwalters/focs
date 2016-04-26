(*

HOMEWORK 1

Name: Sarah Walters

Email: sarah.walters@students.olin.edu

Remarks, if any: Took me ~1.5hrs

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




(* Question 1 *)

let rec gcd (a,b) =
   if (a = 0) then b
   else if (b = 0) then a
   else if (a > b) then gcd (a-b, b)
   else gcd (a, b-a)


let is_coprime (a,b) =
   gcd (a, b) = 1


let rec euler_helper (x, n) =
   if (x = 0) then 0
   else if (is_coprime (x, n)) then
      1 + euler_helper (x-1, n)
   else
      euler_helper (x-1, n)


let euler (n) =
   euler_helper (n, n)


let rec coprimes_helper (x, n) =
   if (x = 0) then []
   else if (is_coprime (x, n)) then
      coprimes_helper (x-1, n) @ [x]
   else
      coprimes_helper (x-1, n)


let coprimes (n) =
   coprimes_helper (n, n)



(* Question 2 *)

let rec append (xs,ys) =
   match xs with
      [] -> ys |
      first::rest -> first::append (rest, ys)


let rec flatten (xss) =
   match xss with
      [] -> [] |
      first::rest -> append (first, flatten (rest))


let rec last (xs) =
   match xs with
      [] -> failwith "empty list" |
      first::rest ->
         if (rest = []) then first
         else last (rest)


let rec nth (n,xs) =
   match xs with
      [] -> failwith "out of bounds" |
      first::rest ->
         if (n = 0) then first
         else nth (n-1, rest)


let rec separate (xs) =
   match xs with
      [] -> ([],[]) |
      first::rest ->
         match first with (m, n) ->
            match separate (rest) with (ms, ns) ->
               (m::ms, n::ns)

(* Question 3 *)

let rec setIn (e,xs) =
   match xs with
      [] -> false |
      first::rest ->
         if (e = first) then true
         else setIn (e, rest)


let rec setSub (xs,ys) =
   match xs with
      [] -> true |
      first::rest ->
         let thisElt = setIn (first, ys) in
            if (rest = []) then thisElt
            else thisElt && setSub (rest, ys)


let setEqual (xs,ys) =
   setSub (xs, ys) && setSub (ys, xs)


let rec setUnion (xs,ys) =
   match xs with
      [] -> ys |
      first::rest ->
         if (setIn (first, ys)) then setUnion (rest, ys)
         else setUnion (rest, first::ys)


let rec setInter (xs,ys) =
   match xs with
      [] -> [] |
      first::rest ->
         if (setIn (first, ys)) then first::setInter (rest, ys)
         else setInter (rest, ys)


let rec setSize_helper (xs, uniqueElts, uniqueCount) =
   match xs with
      [] -> uniqueCount |
      first::rest ->
         if (setIn (first, uniqueElts)) then
            setSize_helper (rest, uniqueElts, uniqueCount)
         else
            setSize_helper (rest, first::uniqueElts, uniqueCount + 1)

let setSize (xs) =
   setSize_helper (xs, [], 0)
