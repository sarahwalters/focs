(*

HOMEWORK 6

Name: Sarah Walters

Email: sarah.walters@students.olin.edu

Remarks, if any: Took ~4h
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
 * String <-> characters utility functions:
 *
 *   explode : string -> string list
 *      returns the list of characters making up a string
 *
 *)

let explode str =
  let rec acc index result =
    if (index<0) then result
    else acc (index-1) ((String.sub str index 1)::result) in
  acc (String.length(str)-1) []


(*
 * Type for deterministic Turing machines
 *
 * Parameterized by type for states
 *
 *)

type symbol = string

type 'a tm = { states : 'a list;
         input_alphabet : symbol list;
         tape_alphabet : symbol list;
         left_marker : symbol;
         blank : symbol;
         delta : ('a * symbol) -> ('a * symbol * int);   (* 0 = Left, 1 = Right *)
         start : 'a;
         accept : 'a;
         reject : 'a }

type 'a config = { state : 'a;
       before: symbol list;
       after: symbol list }


(*
 *   Code to run a string tm machine
 *
 *)

let run m w =

  let printConfig m config value =
    let mw =
      List.fold_right (fun a r -> max (String.length a) r) m.states 0 in
    let _ =
      print_string (String.sub (config.state^(String.make mw ' ')) 0 mw) in
    let print_syms = List.iter (Printf.printf " %s ")  in
    let _ = print_string "  "  in
    let _ = print_syms config.before  in
    let _ = (match config.after with
             | [] -> Printf.printf "[%s]" m.blank
       | a::v' -> let _ = Printf.printf "[%s]" a  in
         print_syms v') in
    let _ = print_newline ()  in
    value  in

  let acceptConfig m config = (config.state=m.accept) in

  let rejectConfig m config = (config.state=m.reject) in

  let haltConfig m c = (acceptConfig m c) || (rejectConfig m c) in

  let startConfig m w =
    { state=m.start;before = [];after = m.left_marker::(explode w)} in

  let rec last u =
    match u with
    | [] -> failwith "Moving Left from leftmost tape position"
    | [a] -> ([],a)
    | x::xs -> let (u',r) = last xs  in (x::u',r)   in

  let step m config =
    if (haltConfig m config) then config
    else let (a,v') = match config.after with
                      | [] -> (m.blank,[])
          | a::v' -> (a,v')  in
         let (q',b,dir) = m.delta(config.state,a) in
   if dir = 0  (* left *)
   then let (u',c) = last config.before in
          {state=q';before=u';after=c::b::v'}
   else {state=q';before=config.before@[b];after=v'} in

  let rec loop c =
    let _ = printConfig m c c in
    if  (acceptConfig m c) then true
    else if (rejectConfig m c) then false
    else loop (step m c)  in

  loop (startConfig m w)


let rec pairs xs ys =
  List.fold_right (fun x r -> (List.map (fun y -> (x,y)) ys)@r) xs []




(* QUESTION 1 *)


let triples xs ys zs =
  List.fold_right (fun x resx -> (
    List.fold_right (fun y resy -> (
      List.map (fun z -> (x,y,z)) zs
    ) @ resy) ys []
  ) @ resx) xs []

let quads xs ys zs ws =
  List.fold_right (fun x resx -> (
    List.fold_right (fun y resy -> (
      List.fold_right (fun z resz -> (
        List.map (fun w -> (x,y,z,w)) ws
      ) @ resz) zs []
    ) @ resy) ys []
  ) @ resx) xs []


let rec range n =
  if (n < 0) then [] else n :: range (n-1)


(* QUESTION 2 *)


let transformStates states f =
  List.map f states


let rec find_original states f target =
  match states with
  | [] -> failwith "no state matches target"
  | state::states' ->
      if (target = f state) then state else find_original states' f target


let transformDelta states delta f =
  (fun (pStr, a) ->
    let p = find_original states f pStr in
      let (q, b, d) = delta (p, a) in
        (f q, b, d)
  )


let transform m f =
  { states = transformStates m.states f;
    input_alphabet = m.input_alphabet;
    tape_alphabet = m.tape_alphabet;
    left_marker = m.left_marker;
    blank = m.blank;
    delta = transformDelta m.states m.delta f;
    start = f m.start;
    accept = f m.accept;
    reject = f m.reject }


(*
 * Some sample deterministic Turing machines with structured states
 *
 * anbn is the non-regular language {a^n b^n | n >= 0}
 * add1  accepts strings u#v where v = u+1 in binary
 *
 *)


let anbn = { states = [ ("start",0);
      ("q",1);
      ("q",2);
      ("q",3);
      ("q",4);
      ("acc",0);
      ("rej",0) ];
       input_alphabet = ["a";"b"];
       tape_alphabet = ["a";"b";"X";"/";"|"];
       blank = "/";
       left_marker = "|";
       start = ("start",0);
       accept = ("acc",0);
       reject = ("rej",0);
       delta = (fun inp -> match inp with
                   | (("start",0), "a") -> (("start",0), "a", 1)
           | (("start",0), "b") -> (("q",1), "b", 1)
       | (("start",0), "|") -> (("start",0), "|", 1)
       | (("start",0), "/") -> (("q",2), "/", 1)
       | (("q",1), "b") -> (("q",1), "b", 1)
       | (("q",1), "/") -> (("q",2), "/", 1)
       | (("q",2), "|") -> (("q",3), "|", 1)
       | (("q",2), "a") -> (("q",2), "a", 0)
       | (("q",2), "b") -> (("q",2), "b", 0)
       | (("q",2), "X") -> (("q",2), "X", 0)
       | (("q",2), "/") -> (("q",2), "/", 0)
       | (("q",3), "X") -> (("q",3), "X", 1)
       | (("q",3), "/") -> (("acc",0), "/", 1)
       | (("q",3), "a") -> (("q",4), "X", 1)
       | (("q",4), "a") -> (("q",4), "a", 1)
       | (("q",4), "X") -> (("q",4), "X", 1)
       | (("q",4), "b") -> (("q",2), "X", 1)
       | (("acc",0), s) -> (("acc",0),s,1)
       | (_,c) -> (("rej",0),c,1))}


let add1 =
  { states =    (* spelled out fully so as not to rely on 'triples' *)
[("start", -1, -1); ("start", -1, 0); ("start", -1, 1); ("start", 0, -1);
 ("start", 0, 0); ("start", 0, 1); ("start", 1, -1); ("start", 1, 0);
 ("start", 1, 1); ("check1", -1, -1); ("check1", -1, 0); ("check1", -1, 1);
 ("check1", 0, -1); ("check1", 0, 0); ("check1", 0, 1); ("check1", 1, -1);
 ("check1", 1, 0); ("check1", 1, 1); ("check2", -1, -1); ("check2", -1, 0);
 ("check2", -1, 1); ("check2", 0, -1); ("check2", 0, 0); ("check2", 0, 1);
 ("check2", 1, -1); ("check2", 1, 0); ("check2", 1, 1); ("rewind", -1, -1);
 ("rewind", -1, 0); ("rewind", -1, 1); ("rewind", 0, -1); ("rewind", 0, 0);
 ("rewind", 0, 1); ("rewind", 1, -1); ("rewind", 1, 0); ("rewind", 1, 1);
 ("go-end-1", -1, -1); ("go-end-1", -1, 0); ("go-end-1", -1, 1);
 ("go-end-1", 0, -1); ("go-end-1", 0, 0); ("go-end-1", 0, 1);
 ("go-end-1", 1, -1); ("go-end-1", 1, 0); ("go-end-1", 1, 1);
 ("go-end-2", -1, -1); ("go-end-2", -1, 0); ("go-end-2", -1, 1);
 ("go-end-2", 0, -1); ("go-end-2", 0, 0); ("go-end-2", 0, 1);
 ("go-end-2", 1, -1); ("go-end-2", 1, 0); ("go-end-2", 1, 1);
 ("skip", -1, -1); ("skip", -1, 0); ("skip", -1, 1); ("skip", 0, -1);
 ("skip", 0, 0); ("skip", 0, 1); ("skip", 1, -1); ("skip", 1, 0);
 ("skip", 1, 1); ("scan-1", -1, -1); ("scan-1", -1, 0); ("scan-1", -1, 1);
 ("scan-1", 0, -1); ("scan-1", 0, 0); ("scan-1", 0, 1); ("scan-1", 1, -1);
 ("scan-1", 1, 0); ("scan-1", 1, 1); ("scan-2", -1, -1); ("scan-2", -1, 0);
 ("scan-2", -1, 1); ("scan-2", 0, -1); ("scan-2", 0, 0); ("scan-2", 0, 1);
 ("scan-2", 1, -1); ("scan-2", 1, 0); ("scan-2", 1, 1);
 ("check-done", -1, -1); ("check-done", -1, 0); ("check-done", -1, 1);
 ("check-done", 0, -1); ("check-done", 0, 0); ("check-done", 0, 1);
 ("check-done", 1, -1); ("check-done", 1, 0); ("check-done", 1, 1)];
    input_alphabet = ["0";"1";"#"];
    tape_alphabet = ["0";"1";"#";"X";"_";">"];
    blank = "_";
    left_marker = ">";
    start = ("start",-1,-1);
    accept = ("acc",-1,-1);
    reject = ("rej",-1,-1);
    delta = (fun x -> match x with
    | (("start",-1,-1),">") -> (("check1",-1,-1),">",1)
    | (("check1",-1,-1),"0") -> (("check1",-1,-1),"0",1)
    | (("check1",-1,-1),"1") -> (("check1",-1,-1),"1",1)
    | (("check1",-1,-1),"#") -> (("check2",-1,-1),"#",1)
    | (("check2",-1,-1),"0") -> (("check2",-1,-1),"0",1)
    | (("check2",-1,-1),"1") -> (("check2",-1,-1),"1",1)
    | (("check2",-1,-1),"_") -> (("rewind",-1,1),"_",0)   (* start with a carry of 1! *)

    | (("rewind",-1,carry),">") -> (("go-end-1",-1,carry),">",1)
    | (("rewind",-1,carry),"0") -> (("rewind",-1,carry),"0",0)
    | (("rewind",-1,carry),"1") -> (("rewind",-1,carry),"1",0)
    | (("rewind",-1,carry),"#") -> (("rewind",-1,carry),"#",0)
    | (("rewind",-1,carry),"X") -> (("rewind",-1,carry),"X",0)

    | (("go-end-1",-1,carry),"#") -> (("scan-1",-1,carry),"#",0)
    | (("go-end-1",-1,carry),sym) -> (("go-end-1",-1,carry),sym,1)

    | (("scan-1",-1,carry),"X") -> (("scan-1",-1,carry),"X",0)
    | (("scan-1",-1,carry),"0") -> (("skip",0,carry),"X",1)
    | (("scan-1",-1,carry),"1") -> (("skip",1,carry),"X",1)
    | (("scan-1",-1,0),">") -> (("check-done",-1,-1),">",1)  (* carry should be 0 to be done *)

    | (("skip",v,carry),"#") -> (("go-end-2",v,carry),"#",1)
    | (("skip",v,carry),"X") -> (("skip",v,carry),"X",1)

    | (("go-end-2",v,carry),"_") -> (("scan-2",v,carry),"_",0)
    | (("go-end-2",v,carry),sym) -> (("go-end-2",v,carry),sym,1)

    | (("scan-2",v,carry),"X") -> (("scan-2",v,carry),"X",0)
    | (("scan-2",v,carry),"0") when (v+carry) mod 2 = 0 -> (("rewind",-1,(v+carry) / 2),"X",0)
    | (("scan-2",v,carry),"1") when (v+carry) mod 2 = 1 -> (("rewind",-1,(v+carry) / 2),"X",0)

    | (("check-done",-1,-1),"_") -> (("acc",-1,-1),"_",1)
    | (("check-done",-1,-1),"X") -> (("check-done",-1,-1),"X",1)
    | (("check-done",-1,-1),"#") -> (("check-done",-1,-1),"#",1)

    | (_,sym) -> (("rej",-1,-1),sym,1))}


(* QUESTION 3 *)

let letters = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"]
let permutation =
  { states = [("start","-");
              ("left","-");
              ("rewind","-");
              ("ff2","-");
              ("check","-");
              ("reject","-");
              ("accept","-")] @
              pairs ["ff1"; "find"] letters;
    input_alphabet = letters;
    tape_alphabet = [">";"_";"X";"#"] @ letters;
    start = ("start","-");
    accept = ("accept","-");
    reject = ("reject","-");
    blank = "_";
    left_marker = ">";
    delta = (fun x -> match x with
    | (("start","-"),">") -> (("left","-"),">",1)
    | (("start",id),sym) -> (("reject","-"),sym,1)

    | (("left","-"),">") -> (("reject","-"),">",1)
    | (("left","-"),"_") -> (("reject","-"),"_",1)
    | (("left","-"),"X") -> (("left","-"),"X",1)
    | (("left","-"),"#") -> (("check","-"),"X",1)
    | (("left","-"),letter) -> (("ff1",letter),"X",1)

    | (("ff1",savedLetter),">") -> (("reject","-"),">",1)
    | (("ff1",savedLetter),"_") -> (("reject","-"),"_",1)
    | (("ff1",savedLetter),"X") -> (("reject","-"),"X",1)
    | (("ff1",savedLetter),"#") -> (("find",savedLetter),"#",1)
    | (("ff1",savedLetter),letter) -> (("ff1",savedLetter),letter,1)

    | (("find", savedLetter),">") -> (("reject","-"),">",1)
    | (("find", savedLetter),"_") -> (("reject","-"),"_",1)
    | (("find", savedLetter),"X") -> (("find",savedLetter),"X",1)
    | (("find", savedLetter),"#") -> (("reject","-"),"#",1)
    | (("find", savedLetter),readLetter) ->
        if (savedLetter = readLetter) then (("ff2","-"),"X",1)
        else (("find",savedLetter),readLetter,1)

    | (("ff2","-"),">") -> (("reject","-"),">",1)
    | (("ff2","-"),"_") -> (("rewind","-"),"_",0)
    | (("ff2","-"),"X") -> (("ff2","-"),"X",1)
    | (("ff2","-"),"#") -> (("reject","-"),">",1)
    | (("ff2","-"),letter) -> (("ff2","-"),letter,1)

    | (("rewind","-"),">") -> (("left","-"),">",1)
    | (("rewind","-"),"_") -> (("reject","-"),"_",1)
    | (("rewind","-"),"X") -> (("rewind","-"),"X",0)
    | (("rewind","-"),"#") -> (("rewind","-"),"#",0)
    | (("rewind","-"),letter) -> (("rewind","-"),letter,0)

    | (("check","-"),"X") -> (("check","-"),"X",1)
    | (("check","-"),"_") -> (("accept","-"),"_",1)
    | (("check","-"),sym) -> (("reject","-"),sym,1)

    | ((_,_),sym) -> (("reject","-"),sym,1))}


let copies n =
  if (n < 1) then failwith "Invalid value of n" else
  { states = [("start","-",-1);
              ("accept","-",-1);
              ("reject","-",-1);
              ("left","-",-1);
              ("rewind","-",-1);
              ("done?","-",-1)] @
              (triples ["ff";"check"] ["0";"1";"#"] (range (n-1)));
    input_alphabet = ["0";"1"];
    tape_alphabet = [">";"X";"#";"_";"0";"1"];
    start = ("start","-",-1);
    accept = ("accept","-",-1);
    reject = ("reject","-",-1);
    blank = "_";
    left_marker = ">";
    delta = (fun x -> match x with
    | (("start","-",-1),">") -> (("left","-",-1),">",1)
    | (("start",_,_),readChar) -> (("reject","-",-1),readChar,1)

    | (("left","-",-1),"X") -> (("left","-",-1),"X",1)
    | (("left","-",-1),"0") -> (("ff","0",0),"X",1)
    | (("left","-",-1),"1") -> (("ff","1",0),"X",1)
    | (("left","-",-1),"#") -> (("ff","#",1),"#",1)
    | (("left","-",-1),"_") ->
        if (n = 1) then (("accept","-",-1),"_",1)
        else (("reject","-",-1),"_",1)

    | (("ff",savedChar,num),">") -> (("reject","-",-1),">",1)
    | (("ff",savedChar,num),"X") -> (("reject","-",-1),"X",1)
    | (("ff",savedChar,num),"#") ->
        if (num >= n-1) then (("reject","-",-1),"#",1)
        else if (savedChar = "#") then (("ff","#",num+1),"#",1)
        else (("check",savedChar,num),"#",1)
    | (("ff",savedChar,num),"_") ->
        if (num = n-1) then (("done?","-",-1),"_",0)
        else (("reject","-",-1),"_",1)
    | (("ff",savedChar,num),readChar) -> (("ff",savedChar,num),readChar,1)

    | (("check",savedChar,num),">") -> (("reject","-",-1),">",1)
    | (("check",savedChar,num),"X") -> (("check",savedChar,num),"X",1)
    | (("check",savedChar,num),"#") -> (("reject","-",-1),"#",1)
    | (("check",savedChar,num),"_") -> (("reject","-",-1),"_",1)
    | (("check",savedChar,num),readChar) ->
        if (savedChar = readChar) then (("ff",savedChar,num+1),"X",1)
        else (("reject","-",-1),readChar,1)

    | (("done?","-",-1),">") -> (("accept","-",-1),">",1)
    | (("done?","-",-1),"#") -> (("done?","-",-1),"#",0)
    | (("done?","-",-1),"X") -> (("done?","-",-1),"X",0)
    | (("done?","-",-1),readChar) -> (("rewind","-",-1),readChar,0)

    | (("rewind","-",-1),">") -> (("left","-",-1),">",1)
    | (("rewind","-",-1),readChar) -> (("rewind","-",-1),readChar,0)

    | ((_,_,_),readChar) -> (("reject","-",-1),readChar,1))}
