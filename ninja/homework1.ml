(*

HOMEWORK 1

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



(* Question 1 *)


let rec expt a b =
  if b < 0 then
    failwith "invalid power"
  else if b == 0 then
    1
  else
    a * (expt a (b - 1))


let rec fastexpt a b =
  if b < 0 then
    failwith "invalid power"
  else if b == 0 then
    1
  else if b mod 2 == 0 then
    let root = (fastexpt a (b / 2)) in
      root * root
  else
    a * (fastexpt a (b - 1))


let rec tetra a b =
  if b < 0 then
    failwith "invalid tetrapower"
  else if b == 0 then
    1
  else
    fastexpt a (tetra a (b - 1))


let rec choose n k =
  if n < 0 || k < 0 then
    failwith "invalid choose"
  else if k == 0 then
    1
  else
    (n + 1 - k) * (choose n (k - 1)) / k (* careful about integer division! *)


(* Question 2 *)


let rec doubleUp xs =
  match xs with
  | [] -> []
  | x :: xs' -> x :: x :: doubleUp xs'


let rec everyOther xs =
  match xs with
  | [] -> []
  | [x1] -> [x1]
  | x1 :: x2 :: xs' -> x1 :: everyOther xs'


let rec concatenate xs ys =
  match xs with
  | [] -> ys
  | x :: xs' -> x :: concatenate xs' ys


let rec concatenateAll xss =
  match xss with
  | [] -> []
  | xs :: xss' -> concatenate xs (concatenateAll xss')


let rec nth n xs =
  match xs with
  | [] -> failwith "index out of bounds"
  | x :: xs' -> if n == 0 then x else nth (n - 1) xs'


let rec last xs =
  match xs with
  | [] -> failwith "no last element of empty list"
  | x :: [] -> x
  | x :: xs' -> last xs'



(* QUESTION 3 *)


let rec addV v w =
  match v with
  | [] -> (
    match w with
    | [] -> []
    | _ -> failwith "cannot add vectors of different lengths"
  )
  | a :: v' -> (
    match w with
    | [] -> failwith "cannot add vectors of different lengths"
    | b :: w' -> (a + b) :: addV v' w'
  )


let rec scaleV a v =
  match v with
  | [] -> []
  | x :: v' -> (a * x) :: scaleV a v'


let rec inner v w =
  match v with
  | [] -> (
    match w with
    | [] -> 0
    | _ -> failwith "cannot take inner product of vectors of different lengths"
  )
  | a :: v' -> (
    match w with
    | [] -> failwith "cannot take inner product of vectors of different lengths"
    | b :: w' -> (a * b) + inner v' w'
  )


let rec outer v w =
  match w with
  | [] -> []
  | a :: w' -> (scaleV a v) :: outer v w'


(* QUESTION 4 *)

let rec addM m n =
  match m with
  | [] -> (
    match n with
    | [] -> []
    | _ ->  failwith "cannot add matrices of different sizes"
  )
  | mRow :: m' -> (
    match n with
    | [] -> failwith "cannot add matrices of different sizes"
    | nRow :: n' -> (addV mRow nRow) :: (addM m' n')
  )


let rec scaleM a m =
  match m with
  | [] -> []
  | mRow :: m' -> (scaleV a mRow) :: (scaleM a m')


let rec multM m n =
  match m with
  | [] -> []
  | mRow :: m' -> (mult1M mRow n) :: (multM m' n)


let rec firstCol m =
  match m with
  | [] -> []
  | row :: m' -> (
    match row with
    | [] -> []
    | h :: t -> h :: firstCol m'
  )


let rec restCols m =
  match m with
  | [] -> []
  | row :: m' -> (
    match row with
    | [] -> []
    | h :: t -> t :: restCols m'
  )

let rec mult1M v m =
  let fc = firstCol m in
    match fc with
    | [] -> []
    | _ ->
      let rc = restCols m in
        (inner v fc) :: mult1M v rc
