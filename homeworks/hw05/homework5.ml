(*

HOMEWORK 5

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
 * Helper function
 *
 * Print a configuration (including newline) to standard output
 * and RETURN A VALUE
 *
 *)

let printConfig m config value =
    let mw = List.fold_right (fun a r -> max (String.length a) r) m.states 0 in
    let _ = print_string (String.sub (config.state^(String.make mw ' ')) 0 mw) in
    let print_syms = List.iter (Printf.printf " %s ")  in
    let _ = print_string "  "  in
    let _ = print_syms config.before  in
    let _ = (match config.after with
             | [] -> Printf.printf "[%s]" m.blank
       | a::v' -> let _ = Printf.printf "[%s]" a  in
                  print_syms v') in
    let _ = print_newline ()  in
    value




(* QUESTION 1 *)


let startConfig m w =
  { state = m.start; before = []; after = m.left_marker :: explode w }


let acceptConfig m config =
  m.accept = config.state


let rejectConfig m config =
  m.reject = config.state


let haltConfig m c =
  acceptConfig m c || rejectConfig m c


let step m config =
  let cell_char = if config.after = [] then m.blank else List.hd config.after in
  let after_tail = if config.after = [] then [] else List.tl config.after in
    let (next_state, write, tapehead_dir) = m.delta (config.state, cell_char) in
      if (tapehead_dir = 0) then (* MOVE TAPEHEAD LEFT *)
        match List.rev config.before with
        | [] -> failwith "Nothing to the left of the tapehead. Cannot step left."
        | last::rev_tail ->
            (* Change first element of after to write, and move last element of before to beginning of after *)
            let next_before = List.rev rev_tail in
            let next_after = last :: write :: after_tail in
              {state = next_state; before = next_before; after = next_after}

      else if (tapehead_dir = 1) then (* MOVE TAPEHEAD RIGHT *)
        match config.after with
        | [] -> {state = next_state; before = config.before; after = [m.blank]} (* Step right at end of input *)
        | head::tail -> {state = next_state; before = config.before @ [write]; after = tail} (* Remove last symbol of before and add write to beginning of after *)

      else failwith "Invalid tapehead direction"

let rec run_helper m config =
  let halt = haltConfig m config in
    if (printConfig m config halt) then acceptConfig m config
    else run_helper m (step m config)

let rec run m w =
  run_helper m (startConfig m w)



(*
 * Some sample deterministic Turing machines
 *
 * asbs is the regular language {a^m b^n | m,n >= 0}
 * anbn is the non-regular language {a^n b^n | n >= 0}
 * anbncn is the non-regular language {a^n b^n c^n | n >= 0}
 *
 *)

let asbs = { states = ["start"; "q1"; "acc"; "rej"];
       input_alphabet = ["a";"b"];
       tape_alphabet = ["a";"b";"_";">"];
       blank = "_";
       left_marker = ">";
       start = "start";
       accept = "acc";
       reject = "rej";
       delta = (fun inp -> match inp with
                   | ("start", "a") -> ("start", "a", 1)
                   | ("start", "b") -> ("q1", "b", 1)
                   | ("start", ">") -> ("start", ">", 1)
                   | ("start", "_") -> ("acc", "_", 1)
                   | ("q1", "b") -> ("q1", "b", 1)
                   | ("q1", "_") -> ("acc", "_", 1)
                   | ("acc", "a") -> ("acc", "a", 1)
                   | ("acc", "b") -> ("acc", "b", 1)
                   | ("acc", ">") -> ("acc", ">", 1)
                   | ("acc", "_") -> ("acc", "_", 1)
                   | (_,c) -> ("rej",c,1))}

let anbn = { states = ["start"; "q1"; "q2"; "q3"; "q4"; "acc"; "rej"];
       input_alphabet = ["a";"b"];
       tape_alphabet = ["a";"b";"X";"/";"|"];
       blank = "/";
       left_marker = "|";
       start = "start";
       accept = "acc";
       reject = "rej";
       delta = (fun inp -> match inp with
                   | ("start", "a") -> ("start", "a", 1)
                   | ("start", "b") -> ("q1", "b", 1)
                   | ("start", "|") -> ("start", "|", 1)
                   | ("start", "/") -> ("q2", "/", 1)
                   | ("q1", "b") -> ("q1", "b", 1)
                   | ("q1", "/") -> ("q2", "/", 1)
                   | ("q2", "|") -> ("q3", "|", 1)
                   | ("q2", "a") -> ("q2", "a", 0)
                   | ("q2", "b") -> ("q2", "b", 0)
                   | ("q2", "X") -> ("q2", "X", 0)
                   | ("q2", "/") -> ("q2", "/", 0)
                   | ("q3", "X") -> ("q3", "X", 1)
                   | ("q3", "/") -> ("acc", "/", 1)
                   | ("q3", "a") -> ("q4", "X", 1)
                   | ("q4", "a") -> ("q4", "a", 1)
                   | ("q4", "X") -> ("q4", "X", 1)
                   | ("q4", "b") -> ("q2", "X", 1)
                   | ("acc", "a") -> ("acc", "a", 1)
                   | ("acc", "b") -> ("acc", "b", 1)
                   | ("acc", "|") -> ("acc", "|", 1)
                   | ("acc", "X") -> ("acc", "X", 1)
                   | ("acc", "/") -> ("acc", "/", 1)
                   | (_,c) -> ("rej",c,1))}


let anbncn = { states = ["start";"q1";"q2";"q3";"q4";"q5";"q6";"acc";"rej"];
         input_alphabet = ["a";"b";"c"];
         tape_alphabet = ["a";"b";"c";"X";"_";">"];
         blank = "_";
         left_marker = ">";
         start = "start";
         accept = "acc";
         reject = "rej";
         delta = (fun inp -> match inp with
                  | ("start", "a") -> ("start", "a", 1)
                  | ("start", "b") -> ("q1", "b", 1)
                  | ("start", "c") -> ("q6", "c", 1)
                  | ("start", ">") -> ("start", ">", 1)
                  | ("start", "_") -> ("q2", "_", 1)
                  | ("q1", "b") -> ("q1", "b", 1)
                  | ("q1", "c") -> ("q6", "c", 1)
                  | ("q1", "_") -> ("q2", "_", 1)
                  | ("q2", ">") -> ("q3", ">", 1)
                  | ("q2", "a") -> ("q2", "a", 0)
                  | ("q2", "b") -> ("q2", "b", 0)
                  | ("q2", "c") -> ("q2", "c", 0)
                  | ("q2", "_") -> ("q2", "_", 0)
                  | ("q2", "X") -> ("q2", "X", 0)
                  | ("q3", "X") -> ("q3", "X", 1)
                  | ("q3", "_") -> ("acc", "_", 1)
                  | ("q3", "a") -> ("q4", "X", 1)
                  | ("q4", "a") -> ("q4", "a", 1)
                  | ("q4", "X") -> ("q4", "X", 1)
                  | ("q4", "b") -> ("q5", "X", 1)
                  | ("q5", "b") -> ("q5", "b", 1)
                  | ("q5", "X") -> ("q5", "X", 1)
                  | ("q5", "c") -> ("q2", "X", 1)
                  | ("q6", "c") -> ("q6", "c", 1)
                  | ("q6", "_") -> ("q2", "_", 1)
                  | ("acc", "a") -> ("acc", "a", 1)
                  | ("acc", "b") -> ("acc", "b", 1)
                  | ("acc", "c") -> ("acc", "c", 1)
                  | ("acc", ">") -> ("acc", ">", 1)
                  | ("acc", "X") -> ("acc", "X", 1)
                  | ("acc", "_") -> ("acc", "_", 1)
                  | (_,c) -> ("rej", c,1))}



(* QUESTION 2 *)

(* THESE ARE PLACEHOLDERS - THEY DEFINE EMPTY TURING MACHINES *)
(* REPLACE BY YOUR OWN DEFINITIONS *)


let tm_q2_a = { states = ["start"; "leftEnd"; "cFastForward"; "dFastForward"; "cRightEnd"; "dRightEnd"; "rewind"; "accept"; "reject"];
    input_alphabet = ["c";"d"];
    tape_alphabet = ["c";"d";"X";">";"_"];
    blank = "_";
    left_marker = ">";
    start = "start";
    accept = "accept";
    reject = "reject";
    delta = (fun inp -> match inp with
             | ("start", "c") -> ("reject", "c", 1)
             | ("start", "d") -> ("reject", "d", 1)
             | ("start", "X") -> ("reject", "X", 1)
             | ("start", ">") -> ("leftEnd", ">", 1)
             | ("start", "_") -> ("reject", "_", 1)

             | ("leftEnd", "c") -> ("cFastForward", "X", 1)
             | ("leftEnd", "d") -> ("dFastForward", "X", 1)
             | ("leftEnd", "X") -> ("leftEnd", "X", 1)
             | ("leftEnd", ">") -> ("reject", ">", 1)
             | ("leftEnd", "_") -> ("accept", "_", 1)

             | ("cFastForward", "c") -> ("cFastForward", "c", 1)
             | ("cFastForward", "d") -> ("cFastForward", "d", 1)
             | ("cFastForward", "X") -> ("cRightEnd", "X", 0)
             | ("cFastForward", ">") -> ("reject", ">", 1)
             | ("cFastForward", "_") -> ("cRightEnd", "_", 0)

             | ("cRightEnd", "c") -> ("rewind", "X", 1)
             | ("cRightEnd", "d") -> ("reject", "d", 1)
             | ("cRightEnd", "X") -> ("accept", "X", 1)
             | ("cRightEnd", ">") -> ("reject", ">", 1)
             | ("cRightEnd", "_") -> ("reject", "_", 1)

             | ("rewind", "c") -> ("rewind", "c", 0)
             | ("rewind", "d") -> ("rewind", "d", 0)
             | ("rewind", "X") -> ("rewind", "X", 0)
             | ("rewind", ">") -> ("leftEnd", ">", 1)
             | ("rewind", "_") -> ("rewind", "_", 0)

             | ("dFastForward", "c") -> ("dFastForward", "c", 1)
             | ("dFastForward", "d") -> ("dFastForward", "d", 1)
             | ("dFastForward", "X") -> ("dRightEnd", "X", 0)
             | ("dFastForward", ">") -> ("reject", ">", 1)
             | ("dFastForward", "_") -> ("dRightEnd", "_", 0)

             | ("dRightEnd", "c") -> ("reject", "c", 1)
             | ("dRightEnd", "d") -> ("rewind", "X", 1)
             | ("dRightEnd", "X") -> ("accept", "X", 1)
             | ("dRightEnd", ">") -> ("reject", ">", 1)
             | ("dRightEnd", "_") -> ("reject", "_", 1))}


let tm_q2_b = { states = ["start"; "bs"; "as"; "rewindPhase1"; "leftEnd"; "rightEnd"; "back1"; "back2"; "back3"; "rewindPhase2"; "accept"; "reject"];
    input_alphabet = ["a"; "b"];
    tape_alphabet = ["a"; "b"; "X"; "_"; ">"];
    blank = "_";
    left_marker = ">";
    start = "start";
    accept = "accept";
    reject = "reject";
    delta = (fun inp -> match inp with
             | ("start", "a") -> ("reject", "c", 1)
             | ("start", "b") -> ("reject", "d", 1)
             | ("start", "X") -> ("reject", "X", 1)
             | ("start", ">") -> ("bs", ">", 1)
             | ("start", "_") -> ("reject", "_", 1)

             | ("bs", "a") -> ("as", "a", 1)
             | ("bs", "b") -> ("bs", "b", 1)
             | ("bs", "X") -> ("reject", "X", 1)
             | ("bs", ">") -> ("reject", ">", 1)
             | ("bs", "_") -> ("as", "_", 1)

             | ("as", "a") -> ("as", "a", 1)
             | ("as", "b") -> ("reject", "b", 1)
             | ("as", "X") -> ("reject", "X", 1)
             | ("as", ">") -> ("reject", ">", 1)
             | ("as", "_") -> ("rewindPhase1", "_", 0)

             | ("rewindPhase1", "a") -> ("rewindPhase1", "a", 0)
             | ("rewindPhase1", "b") -> ("rewindPhase1", "b", 0)
             | ("rewindPhase1", "X") -> ("reject", "X", 1)
             | ("rewindPhase1", ">") -> ("leftEnd", ">", 1)
             | ("rewindPhase1", "_") -> ("reject", "_", 1)

             | ("leftEnd", "a") -> ("reject", "a", 1)
             | ("leftEnd", "b") -> ("rightEnd", "X", 1)
             | ("leftEnd", "X") -> ("leftEnd", "X", 1)
             | ("leftEnd", ">") -> ("reject", ">", 1)
             | ("leftEnd", "_") -> ("accept", "_", 1)

             | ("rightEnd", "a") -> ("rightEnd", "a", 1)
             | ("rightEnd", "b") -> ("rightEnd", "b", 1)
             | ("rightEnd", "X") -> ("back1", "X", 0)
             | ("rightEnd", ">") -> ("reject", ">", 1)
             | ("rightEnd", "_") -> ("back1", "_", 0)

             | ("back1", "a") -> ("back2", "X", 0)
             | ("back1", "b") -> ("reject", "b", 1)
             | ("back1", "X") -> ("reject", "X", 1)
             | ("back1", ">") -> ("reject", ">", 1)
             | ("back1", "_") -> ("reject", "_", 1)

             | ("back2", "a") -> ("back3", "X", 0)
             | ("back2", "b") -> ("reject", "b", 1)
             | ("back2", "X") -> ("reject", "X", 1)
             | ("back2", ">") -> ("reject", ">", 1)
             | ("back2", "_") -> ("reject", "_", 1)

             | ("back3", "a") -> ("rewindPhase2", "X", 0)
             | ("back3", "b") -> ("reject", "d", 1)
             | ("back3", "X") -> ("reject", "X", 1)
             | ("back3", ">") -> ("reject", ">", 1)
             | ("back3", "_") -> ("reject", "_", 1)

             | ("rewindPhase2", "a") -> ("rewindPhase2", "a", 0)
             | ("rewindPhase2", "b") -> ("rewindPhase2", "b", 0)
             | ("rewindPhase2", "X") -> ("rewindPhase2", "X", 0)
             | ("rewindPhase2", ">") -> ("leftEnd", ">", 1)
             | ("rewindPhase2", "_") -> ("reject", "_", 1))}




(* QUESTION 3 *)


let binaryAddition = { states = ["start"; "ff1"; "lsb1"; "cont1_#"; "cont1_0"; "cont1_1"; "ff2_#"; "ff2_0"; "ff2_1"; "lsb2_#"; "lsb2_0"; "lsb2_1"; "contDone"; "cont2_0"; "cont2_1"; "cont2_2"; "done"; "ff3_0"; "ff3_1"; "ff3_2"; "lsb3_0"; "lsb3_1"; "lsb3_2"; "sub1pt1"; "sub1pt2"; "rewind"; "accept"; "reject"];
           input_alphabet = ["0"; "1"; "#"];
           tape_alphabet = ["0"; "1"; "X"; ">"; "_"];
           blank = "_";
           left_marker = ">";
           start = "start";
           accept = "accept";
           reject = "reject";
           delta = (fun inp -> match inp with
                    | ("start", "0") -> ("reject", "0", 1)
                    | ("start", "1") -> ("reject", "1", 1)
                    | ("start", "#") -> ("reject", "#", 1)
                    | ("start", "X") -> ("reject", "X", 1)
                    | ("start", ">") -> ("ff1", ">", 1)
                    | ("start", "_") -> ("reject", "_", 1)

                    | ("ff1", "0") -> ("ff1", "0", 1)
                    | ("ff1", "1") -> ("ff1", "1", 1)
                    | ("ff1", "#") -> ("lsb1", "#", 0)
                    | ("ff1", "X") -> ("ff1", "X", 1)
                    | ("ff1", ">") -> ("reject", ">", 1)
                    | ("ff1", "_") -> ("reject", "_", 1)

                    | ("lsb1", "0") -> ("cont1_0", "X", 1)
                    | ("lsb1", "1") -> ("cont1_1", "X", 1)
                    | ("lsb1", "#") -> ("reject", "#", 1)
                    | ("lsb1", "X") -> ("lsb1", "X", 0)
                    | ("lsb1", ">") -> ("cont1_#", ">", 1)
                    | ("lsb1", "_") -> ("reject", "_", 1)

                    | ("cont1_#", "0") -> ("reject", "0", 1)
                    | ("cont1_#", "1") -> ("reject", "1", 1)
                    | ("cont1_#", "#") -> ("ff2_#", "#", 1)
                    | ("cont1_#", "X") -> ("cont1_#", "X", 1)
                    | ("cont1_#", ">") -> ("reject", ">", 1)
                    | ("cont1_#", "_") -> ("reject", "_", 1)

                    | ("cont1_0", "0") -> ("reject", "0", 1)
                    | ("cont1_0", "1") -> ("reject", "1", 1)
                    | ("cont1_0", "#") -> ("ff2_0", "#", 1)
                    | ("cont1_0", "X") -> ("cont1_0", "X", 1)
                    | ("cont1_0", ">") -> ("reject", ">", 1)
                    | ("cont1_0", "_") -> ("reject", "_", 1)

                    | ("cont1_1", "0") -> ("reject", "0", 1)
                    | ("cont1_1", "1") -> ("reject", "1", 1)
                    | ("cont1_1", "#") -> ("ff2_1", "#", 1)
                    | ("cont1_1", "X") -> ("cont1_1", "X", 1)
                    | ("cont1_1", ">") -> ("reject", ">", 1)
                    | ("cont1_1", "_") -> ("reject", "_", 1)

                    | ("ff2_#", "0") -> ("ff2_#", "0", 1)
                    | ("ff2_#", "1") -> ("ff2_#", "1", 1)
                    | ("ff2_#", "#") -> ("lsb2_#", "#", 0)
                    | ("ff2_#", "X") -> ("ff2_#", "X", 1)
                    | ("ff2_#", ">") -> ("reject", ">", 1)
                    | ("ff2_#", "_") -> ("reject", "_", 1)

                    | ("ff2_0", "0") -> ("ff2_0", "0", 1)
                    | ("ff2_0", "1") -> ("ff2_0", "1", 1)
                    | ("ff2_0", "#") -> ("lsb2_0", "#", 0)
                    | ("ff2_0", "X") -> ("ff2_0", "X", 1)
                    | ("ff2_0", ">") -> ("reject", ">", 1)
                    | ("ff2_0", "_") -> ("reject", "_", 1)

                    | ("ff2_1", "0") -> ("ff2_1", "0", 1)
                    | ("ff2_1", "1") -> ("ff2_1", "1", 1)
                    | ("ff2_1", "#") -> ("lsb2_1", "#", 0)
                    | ("ff2_1", "X") -> ("ff2_1", "X", 1)
                    | ("ff2_1", ">") -> ("reject", ">", 1)
                    | ("ff2_1", "_") -> ("reject", "_", 1)

                    | ("lsb2_#", "0") -> ("cont2_0", "X", 1)
                    | ("lsb2_#", "1") -> ("cont2_1", "X", 1)
                    | ("lsb2_#", "#") -> ("contDone", "#", 1)
                    | ("lsb2_#", "X") -> ("lsb2_#", "X", 0)
                    | ("lsb2_#", ">") -> ("reject", ">", 1)
                    | ("lsb2_#", "_") -> ("reject", "_", 1)

                    | ("lsb2_0", "0") -> ("cont2_0", "X", 1)
                    | ("lsb2_0", "1") -> ("cont2_1", "X", 1)
                    | ("lsb2_0", "#") -> ("cont2_0", "#", 1)
                    | ("lsb2_0", "X") -> ("lsb2_0", "X", 0)
                    | ("lsb2_0", ">") -> ("reject", ">", 1)
                    | ("lsb2_0", "_") -> ("reject", "_", 1)

                    | ("lsb2_1", "0") -> ("cont2_1", "X", 1)
                    | ("lsb2_1", "1") -> ("cont2_2", "X", 1)
                    | ("lsb2_1", "#") -> ("cont2_1", "#", 1)
                    | ("lsb2_1", "X") -> ("lsb2_1", "X", 0)
                    | ("lsb2_1", ">") -> ("reject", ">", 1)
                    | ("lsb2_1", "_") -> ("reject", "_", 1)

                    | ("contDone", "0") -> ("reject", "0", 1)
                    | ("contDone", "1") -> ("reject", "1", 1)
                    | ("contDone", "#") -> ("done", "#", 1)
                    | ("contDone", "X") -> ("contDone", "X", 1)
                    | ("contDone", ">") -> ("reject", ">", 1)
                    | ("contDone", "_") -> ("reject", "_", 1)

                    | ("cont2_0", "0") -> ("reject", "0", 1)
                    | ("cont2_0", "1") -> ("reject", "1", 1)
                    | ("cont2_0", "#") -> ("ff3_0", "#", 1)
                    | ("cont2_0", "X") -> ("cont2_0", "X", 1)
                    | ("cont2_0", ">") -> ("reject", ">", 1)
                    | ("cont2_0", "_") -> ("reject", "_", 1)

                    | ("cont2_1", "0") -> ("reject", "0", 1)
                    | ("cont2_1", "1") -> ("reject", "1", 1)
                    | ("cont2_1", "#") -> ("ff3_1", "#", 1)
                    | ("cont2_1", "X") -> ("cont2_1", "X", 1)
                    | ("cont2_1", ">") -> ("reject", ">", 1)
                    | ("cont2_1", "_") -> ("reject", "_", 1)

                    | ("cont2_2", "0") -> ("reject", "0", 1)
                    | ("cont2_2", "1") -> ("reject", "1", 1)
                    | ("cont2_2", "#") -> ("ff3_2", "#", 1)
                    | ("cont2_2", "X") -> ("cont2_2", "X", 1)
                    | ("cont2_2", ">") -> ("reject", ">", 1)
                    | ("cont2_2", "_") -> ("reject", "_", 1)

                    | ("done", "0") -> ("done", "0", 1)
                    | ("done", "1") -> ("reject", "1", 1)
                    | ("done", "#") -> ("reject", "#", 1)
                    | ("done", "X") -> ("done", "X", 1)
                    | ("done", ">") -> ("reject", ">", 1)
                    | ("done", "_") -> ("accept", "_", 1)

                    | ("ff3_0", "0") -> ("ff3_0", "0", 1)
                    | ("ff3_0", "1") -> ("ff3_0", "1", 1)
                    | ("ff3_0", "#") -> ("reject", "#", 1)
                    | ("ff3_0", "X") -> ("ff3_0", "X", 1)
                    | ("ff3_0", ">") -> ("reject", ">", 1)
                    | ("ff3_0", "_") -> ("lsb3_0", "_", 0)

                    | ("ff3_1", "0") -> ("ff3_1", "0", 1)
                    | ("ff3_1", "1") -> ("ff3_1", "1", 1)
                    | ("ff3_1", "#") -> ("reject", "#", 1)
                    | ("ff3_1", "X") -> ("ff3_1", "X", 1)
                    | ("ff3_1", ">") -> ("reject", ">", 1)
                    | ("ff3_1", "_") -> ("lsb3_1", "_", 0)

                    | ("ff3_2", "0") -> ("ff3_2", "0", 1)
                    | ("ff3_2", "1") -> ("ff3_2", "1", 1)
                    | ("ff3_2", "#") -> ("reject", "#", 1)
                    | ("ff3_2", "X") -> ("ff3_2", "X", 1)
                    | ("ff3_2", ">") -> ("reject", ">", 1)
                    | ("ff3_2", "_") -> ("lsb3_2", "_", 0)

                    | ("lsb3_0", "0") -> ("rewind", "X", 1)
                    | ("lsb3_0", "1") -> ("reject", "1", 1)
                    | ("lsb3_0", "#") -> ("rewind", "#", 1)
                    | ("lsb3_0", "X") -> ("lsb3_0", "X", 0)
                    | ("lsb3_0", ">") -> ("reject", ">", 1)
                    | ("lsb3_0", "_") -> ("reject", "_", 1)

                    | ("lsb3_1", "0") -> ("reject", "0", 1)
                    | ("lsb3_1", "1") -> ("rewind", "X", 1)
                    | ("lsb3_1", "#") -> ("reject", "#", 1)
                    | ("lsb3_1", "X") -> ("lsb3_1", "X", 0)
                    | ("lsb3_1", ">") -> ("reject", ">", 1)
                    | ("lsb3_1", "_") -> ("reject", "_", 1)

                    | ("lsb3_2", "0") -> ("sub1pt1", "X", 0)
                    | ("lsb3_2", "1") -> ("reject", "1", 1)
                    | ("lsb3_2", "#") -> ("reject", "#", 1)
                    | ("lsb3_2", "X") -> ("lsb3_2", "X", 0)
                    | ("lsb3_2", ">") -> ("reject", ">", 1)
                    | ("lsb3_2", "_") -> ("reject", "_", 1)

                    | ("sub1pt1", "0") -> ("sub1pt1", "0", 0)
                    | ("sub1pt1", "1") -> ("sub1pt2", "0", 1)
                    | ("sub1pt1", "#") -> ("reject", "#", 1)
                    | ("sub1pt1", "X") -> ("reject", "X", 1)
                    | ("sub1pt1", ">") -> ("reject", ">", 1)
                    | ("sub1pt1", "_") -> ("reject", "_", 1)

                    | ("sub1pt2", "0") -> ("sub1pt2", "1", 1)
                    | ("sub1pt2", "1") -> ("reject", "1", 1)
                    | ("sub1pt2", "#") -> ("reject", "#", 1)
                    | ("sub1pt2", "X") -> ("rewind", "X", 1)
                    | ("sub1pt2", ">") -> ("reject", ">", 1)
                    | ("sub1pt2", "_") -> ("reject", "_", 0)

                    | ("rewind", "0") -> ("rewind", "0", 0)
                    | ("rewind", "1") -> ("rewind", "1", 0)
                    | ("rewind", "#") -> ("rewind", "#", 0)
                    | ("rewind", "X") -> ("rewind", "X", 0)
                    | ("rewind", ">") -> ("ff1", ">", 1)
                    | ("rewind", "_") -> ("rewind", "_", 0))}


let powers2 = {
	states = ["start"; "none"; "one"; "odd"; "even"; "rewind"; "acc"; "rej"];
    input_alphabet = ["a"];
    tape_alphabet = [">"; "a"; "X"; "_"];
    blank = "_";
    left_marker = ">";
    start = "start";
    accept = "acc";
    reject = "rej";
    delta = (fun inp -> match inp with
             | ("start", ">") -> ("none", ">", 1)
             | ("start", "a") -> ("rej", "a", 1)
             | ("start", "X") -> ("rej", "X", 1)
             | ("start", "_") -> ("rej", "_", 1)

             | ("none", ">") -> ("rej", ">", 1)
             | ("none", "a") -> ("one", "a", 1)
             | ("none", "X") -> ("none", "X", 1)
             | ("none", "_") -> ("rej", "_", 1)

             | ("one", ">") -> ("rej", ">", 1)
             | ("one", "a") -> ("even", "X", 1)
             | ("one", "X") -> ("one", "X", 1)
             | ("one", "_") -> ("acc", "_", 1)

             | ("even", ">") -> ("rej", ">", 1)
             | ("even", "a") -> ("odd", "a", 1)
             | ("even", "X") -> ("even", "X", 1)
             | ("even", "_") -> ("rewind", "_", 0)

             | ("odd", ">") -> ("rej", ">", 1)
             | ("odd", "a") -> ("even", "X", 1)
             | ("odd", "X") -> ("odd", "X", 1)
             | ("odd", "_") -> ("rej", "_", 1)

             | ("rewind", ">") -> ("none", ">", 1)
             | ("rewind", "a") -> ("rewind", "a", 0)
             | ("rewind", "X") -> ("rewind", "X", 0)
             | ("rewind", "_") -> ("rej", "_", 1)
            )
}
