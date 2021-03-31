(* midterm-project_strings.ml *)
(* Introduction to Computer Science (YSC1212), Sem2, 2020-2021 *)
(* Aditya Singhania <adityasinghania@yale-nus.edu.sg> *)
(* Version of Fri 10 Mar 2021, fixed inductive definitions *)
(* was: *)
(* Version of Mon 1 Mar 2021 *)

(* ********** *)

(* nat_parafold_right *)
let nat_parafold_right zero_case succ_case n_given =
 (* nat_parafold_right : 'a -> (int -> 'a -> 'a) -> int -> 'a *)
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 0
    then zero_case
    else let n' = n - 1
         in let ih = visit n'
            in succ_case n' ih    (* <-- succ_case takes two arguments *)
  in visit n_given;;

(* ********** *)

(* Question 1 *)

(* a unit test for our future string_append function *)
let test_string_append candidate =
      (* empty string: *)
  let b0 = (candidate "" "" = "")
      (* some intuitive cases: *)
  and b1 = (candidate "abc" "def" = "abcdef")
  and b2 = (candidate "AbC" "dEf" = "AbCdEf")
  and b3 = (candidate "hello, " "world!" = "hello, world!")
  and b4 = (candidate "\\" "\"" = "\\\"")
  and b5 = (candidate " " " " = "  ")
      (* random case *)
  and b6 = (let a = String.init 10 (fun _ -> char_of_int (Random.int 128))
            and b = String.init 10 (fun _ -> char_of_int (Random.int 128))
            in candidate a b = a ^ b)
  (* etc. *)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6;;

let string_append_v1 s1 s2 =
  String.init (String.length s1 + String.length s2)
  (fun i -> if i < (String.length s1)
            then String.get s1 i
            else String.get s2 (i - (String.length s1)));;

(* testing function with unit test *)
let () = assert (test_string_append string_append_v1);;

(* traced version of String.length *)
let string_length s1 =
  let () = Printf.printf "string length %s -> \n" s1
  in String.length s1;;

let string_append_traced_v1 s1 s2 =
  String.init (string_length s1 + string_length s2)
  (fun i -> if i < (string_length s1)
            then String.get s1 i
            else String.get s2 (i - (string_length s1)));;

(* in this version, we can see that the String.length is
   called many times on s1, and once on s2

   we can improve this function to only call String.length twice, in total! *)

let string_append_v2 s1 s2 =
  let s1_l = String.length s1
  and s2_l = String.length s2 in
    String.init
    (s1_l + s2_l)
    (fun i -> if i < s1_l
              then String.get s1 i
              else String.get s2 (i - s1_l));;

let string_append_traced_v2 s1 s2 =
  let s1_l = string_length s1
  and s2_l = string_length s2 in
    String.init
    (s1_l + s2_l)
    (fun i -> if i < s1_l
              then String.get s1 i
              else String.get s2 (i - s1_l));;

(* testing function with unit test *)
let () = assert (test_string_append string_append_v2);;

(* in this version, we can see that the String.length is
   only called twice: once on s1, and once on s2 *)

let string_append_v3 s1 s2 =
  let s1_l = String.length s1
  and s2_l = String.length s2 in
    if s1_l = 0
    then s2
    else if s2_l = 0
    then s1
    else
      String.init
      (s1_l + s2_l)
      (fun i -> if i < s1_l
                then String.get s1 i
                else String.get s2 (i - s1_l));;

(* testing function with unit test *)
let () = assert (test_string_append string_append_v3);;

let string_append_v4 s1 s2 =
  let s1_l = String.length s1 in
    if s1_l = 0
    then s2
    else let s2_l = String.length s2 in
         if s2_l = 0
         then s1
         else
           String.init
           (s1_l + s2_l)
           (fun i -> if i < s1_l
                     then String.get s1 i
                     else String.get s2 (i - s1_l));;

(* testing function with unit test *)
let () = assert (test_string_append string_append_v4);;

let string_append_v5 s1 s2 =
  if s1 = ""
  then s2
  else if s2 = ""
  then s1
  else let s1_l = String.length s1
       and s2_l = String.length s2 in
         String.init
         (s1_l + s2_l)
         (fun i -> if i < s1_l
                   then String.get s1 i
                   else String.get s2 (i - s1_l));;

(* testing function with unit test *)
let () = assert (test_string_append string_append_v5);;

(* ********** *)

(* Question 9 (optional) *)

let test_warmup candidate =
  (candidate 'a' 'b' 'c' = "abc");;

let warmup_v0 c0 c1 c2 =
  let c0_string = String.make 1 (c0)
  and c1_string = String.make 1 (c1)
  and c2_string = String.make 1 (c2)
  in string_append_v4 (string_append_v4 c0_string c1_string) c2_string;;

(* testing function with unit test *)
let () = assert (test_warmup warmup_v0);;

let warmup_v1 c0 c1 c2 =
  String.init 3 (fun i -> if i = 0
                          then c0
                          else if i = 1
                          then c1
                          else c2);;

(* testing function with unit test *)
let () = assert (test_warmup warmup_v1);;

(* ********** *)

(* Question 2 *)

let show_char c =
 (* show_char : char -> string *)
  "'" ^ (if c = '\\' then "\\\\" else if c = '\'' then "\\\'" else String.make 1 c) ^ "'";;

let a_char c =
 (* a_char : char -> char *)
  let () = Printf.printf "processing %s...\n" (show_char c)
  in c;;

(* String.map (fun c -> (a_char c)) "Yal'";; *)

(*

  output:

  index 0
  processing 'Y'...
  index 1
  processing 'a'...
  index 2
  processing 'l'...
  index 3
  processing '\''...
  - : string = "Yal'"

*)

(* hence, we see that characters are accessed from left to right *)

let string_map_up_v0 f s =
  let s_length = String.length s in
    let rec visit n =
       if n = 0
       then String.make 1 (f (String.get s 0))
       else let n' = n - 1 in
              let ih = visit n' in
                ih ^ (String.make 1 (f (String.get s n))) in
       visit (s_length - 1);;


(* a function to test our new string_map_up function *)
let test_string_map_up str_map_up f s =
  (str_map_up f s = String.map f s);;

(* testing function with testing function *)
let () = assert (test_string_map_up
                 string_map_up_v0
                 (fun c -> char_of_int (int_of_char c + 1))
                 "abc");;

(* the function seems to work *)

(* and, if we trace it, we can see it accesses characters upwards *)
(*
# string_map_up_v0 (fun c -> a_char c) "abc";;
processing 'a'...
processing 'b'...
processing 'c'...
- : string = "abc"
 *)

(* if we really think about it, this is consistent with what we learnt in
   question 05 of the project about the underlying determinism of ocaml!

   in the expression "let x1 = e1 in e0", the definien (let x1 = e1) is
   evaluated first. then, the expression e0 is evaluated!

   hence, in the following code snippet:

        let ih = visit n' in
          ih ^ (String.make 1 (f (String.get s n))) in
       visit (s_length - 1);;

   we first evaluate "let x1 = e1", that is "let ih = visit n' ".

   to evaluate "let ih = visit n' ", we must first evaluate visit n'

   hence, when we evaluate "visit (s_length - 1)", we run the same expression
   as above, "let ih = visit n' ". for the expresion above, we must evaluate
   visit n' in an environment where n' denotes n - 1, or in this case
   s_length - 2.

   this chain continues all the way down, until the very last character,
   that is visit 0. hence, visit 0 is what is evauated before anything else!

   visit 0 evalutes the first character of the given string.
   it passes this value to visit 1, which subsequently evalutes the second
   character of the string and so on until we reach visit (s_length - 1).

   hence, the function first accesses the character at the 0th indice, then the
   1st indice, then 2nd and so on until it reaches s_length - 1

   and so we see how the functions maps "up"!

 *)

(* however, this will not work with empty strings!
   hence, below is a version that accounts for empty strings. *)

let string_map_up_v1 f s =
  let s_length = String.length s in
    if s_length = 0
    then s
    else let rec visit n =
         if n = 0
         then String.make 1 (f (String.get s 0))
         else let n' = n - 1 in
                let ih = visit n' in
                 ih ^ (String.make 1 (f (String.get s n))) in
         visit (s_length - 1);;


(* testing function with testing function *)
let () = assert (test_string_map_up
                 string_map_up_v1
                 (fun c -> char_of_int (int_of_char c + 1))
                 "");;

(* the new function now works fine with empty strings! *)

(* testing function with testing function *)
let () = assert (test_string_map_up
                 string_map_up_v1
                 (fun c -> char_of_int (int_of_char c + 1))
                 "abc");;

(* of course, the new function also passes our earlier test,
   and the direction of character access is the same as well! *)

(*
# string_map_up_v1 (fun c -> a_char c) "abc";;
processing 'a'...
processing 'b'...
processing 'c'...
- : string = "abc"
 *)

(*

  However, having a separate if condition to deal with the base case seems a
  bit odd!

  We should NOT have to define our base case seperately

  Harumph.

  Maybe we need to rethink our function to truly define it inductively.

  base case: n = s_length
  we must return an empty string!

  inductive step: n' = n - 1
  apply given function to character at that index and concatenate!

*)

let string_map_up_v2 f s =
  let s_length = String.length s in
    let rec visit n =
      if n = 0
      then ""
      else let n' = n - 1 in
             let ih = visit n' in
               ih ^ (String.make 1 (f (String.get s n'))) in
    visit s_length;;

(* testing function with testing function *)
let () = assert (test_string_map_up
                 string_map_up_v2
                 (fun c -> char_of_int (int_of_char c + 1))
                 "");;

(* testing function with testing function *)
let () = assert (test_string_map_up
                 string_map_up_v2
                 (fun c -> char_of_int (int_of_char c + 1))
                 "abc");;

(* Eureka! *)
(* andddd.. drumroll... to trace it! *)

(*
# string_map_up_v2 (fun c -> a_char c) "abc";;
processing 'a'...
processing 'b'...
processing 'c'...
- : string = "abc"
 *)

(* YAY! *)

(* maybe with nat_parafold_right too? *)
let string_map_up_v3 f s =
  nat_parafold_right ""
                     (fun n' ih -> ih ^ (String.make 1 (f (String.get s n'))))
                     (String.length s);;

(* testing function with testing function *)
let () = assert (test_string_map_up
                 string_map_up_v3
                 (fun c -> char_of_int (int_of_char c + 1))
                 "abc");;

(* testing function with testing function *)
let () = assert (test_string_map_up
                 string_map_up_v3
                 (fun c -> char_of_int (int_of_char c + 1))
                 "");;

(*
# string_map_up_v3 (fun c -> a_char c) "abc";;
processing 'a'...
processing 'b'...
processing 'c'...
- : string = "abc"
 *)

(* and hence, we were able to define string_map_up inductively *)

(* Onward to map_down! *)

(* a function to test our potential string_map_down function *)
let test_string_map_down str_map_down f s =
  (str_map_down f s = String.map f s);;
(* let me let you on in a little secret:
   it's really the same function as test_string_map_up

   :-)

   (it doesn't check for the traces anyways, and it compares it to
    String.map!) *)

let string_map_down_v0 f s =
  let s_length = String.length s in
    if s_length = 0
    then s
    else let rec visit n =
         if n = s_length - 1
         then String.make 1 (f (String.get s (s_length - 1)))
         else let n' = n + 1 in
                let ih = visit n' in
                  (String.make 1 (f (String.get s n))) ^ ih in
         visit 0;;

(* testing function with testing function *)
let () = assert (test_string_map_down
                 string_map_down_v0
                 (fun c -> char_of_int (int_of_char c + 1))
                 "abc");;

(* the function seems to work *)

(* testing function with testing function *)
let () = assert (test_string_map_down
                 string_map_down_v0
                 (fun c -> char_of_int (int_of_char c + 1))
                 "");;

(* of course, the new function also works with empty strings,
   as we used what we learnt from the previous exercise       *)

(* and to trace it to see the direction of character access: *)
(*
# string_map_down_v0 (fun c -> a_char c) "abc";;
processing 'c'...
processing 'b'...
processing 'a'...
- : string = "abc"
 *)

(* if we really think about it, this is consistent with what we learnt in
   question 05 of the project about the underlying determinism of ocaml!

   in the expression "let x1 = e1 in e0", the definien (let x1 = e1) is
   evaluated first. then, the expression e0 is evaluated!

   hence, in the following code snippet:

                let ih = visit n' in
                  (String.make 1 (f (String.get s n))) ^ ih in
         visit 0;;

   we first evaluate "let x1 = e1", that is "let ih = visit n' ".

   to evaluate "let ih = visit n' ", we must first evaluate visit n'

   hence, when we evaluate "visit 0" we run the same expression
   as above, "let ih = visit n' ". for the expresion above, we must evaluate
   visit n' in an environment where n' denotes n + 1, or in this case
   1.

   this chain continues all the way down, until the very last character,
   that is visit s_length - 1. hence, visit s_length - 1
   is what is evauated before anything else!

   visit s_length - 1 evalutes the last character of the given string.
   it passes this value to visit s_length -2, which then evalutes the second
   last character of the string and so on until we reach visit 0.

   hence, the function first accesses the character at the indice s_length - 1,
   then the one at indice s_length - 2, then s_length - 3 and so on until it
   reaches 0!

   and so we see how the functions maps "down"!

 *)

(*

  Wait a second.....
  "learnt from our previous exercise"
  LIES !!!
  We are repeating our mistake!

  The fact that we are still using a separate if condition
  for strings of length 0 suggests that our definition is not truly
  inductively defined. We should NOT have to define our base case seperately

  Harumph.

  We really haven't learnt, have we?

  Maybe we need to rethink our function to truly define it inductively.

  base case: n = 0
  we must return an empty string!

  inductive step: n = n - 1
  apply given function to character at that index and concatenate!

*)

let string_map_down_v1 f s =
  let s_length = String.length s in
    let rec visit n =
      if n = s_length
      then ""
      else let n' = n + 1 in
              (String.make 1 (f (String.get s n))) ^ visit n' in
    visit 0;;

(* testing function with testing function *)
let () = assert (test_string_map_down
                 string_map_down_v1
                 (fun c -> char_of_int (int_of_char c + 1))
                 "abc");;

(* testing function with testing function *)
let () = assert (test_string_map_down
                 string_map_down_v1
                 (fun c -> char_of_int (int_of_char c + 1))
                 "");;

(* and to trace it: *)
(*
# string_map_down_v1 (fun c -> a_char c) "abc";;
processing 'c'...
processing 'b'...
processing 'a'...
- : string = "abc"
 *)

(* finally! this functions seems satisfactory (for now)! *)



(* nat_parafold_right_down *)
let nat_parafold_right_down n_case succ_case n_given =
 (* nat_parafold_right_down : 'a -> (int -> 'a -> 'a) -> int -> 'a *)
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = n_given
    then n_case
    else let n' = n + 1
         in let ih = visit n'
            in succ_case n' ih
  in visit 0;;

(* with this new definition of nat_parafold_right_down, we can implement string_map_down *)
let string_map_down_v2 f s =
  nat_parafold_right_down ""
                          (fun n' ih -> (String.make 1 (f (String.get s (n' - 1)))) ^ ih)
                          (String.length s);;

(* testing function with testing function *)
let () = assert (test_string_map_down
                 string_map_down_v2
                 (fun c -> char_of_int (int_of_char c + 1))
                 "");;

(* testing function with testing function *)
let () = assert (test_string_map_down
                 string_map_down_v2
                 (fun c -> char_of_int (int_of_char c + 1))
                 "abc");;

(* and yes! this passes our earlier unit test *)

(* aaaannddd... the trace! of course! *)
(*
# string_map_down_v2 (fun c -> a_char c) "abc";;
processing 'c'...
processing 'b'...
processing 'a'...
- : string = "abc"
*)

(* ********** *)

(* Question 3 (optional) *)

(* new function, same story *)

let show_int n =
 (* show_int : int -> string *)
  if n < 0
  then "(" ^ string_of_int n ^ ")"
  else string_of_int n;;

(* String.mapi
  (fun i c -> let () = Printf.printf "index %s\n" (show_int i) in (a_char c))
  "Yal'";; *)
(* output:

  index 0
  processing 'Y'...
  index 1
  processing 'a'...
  index 2
  processing 'l'...
  index 3
  processing '\''...
  - : string = "Yal'"

*)

(* hence, we see that characters are accessed from left to right,
   with 0 as the first character's index and the index being incremented
   for each additional character *)

(* now, we can just use nat_parafold_right and nat_parafold_right_down! *)

let string_mapi_up f s =
  nat_parafold_right ""
                     (fun n' ih -> ih ^ String.make 1 (f n' (String.get s n')))
                     (String.length s);;

(* or, if you would prefer, inlined: *)
(* let string_mapi_up f s =
  let s_length = String.length s in
    let rec visit n =
      if n = 0
      then ""
      else let n' = n - 1 in
           visit n' ^ (String.make 1 (f n' (String.get s n'))) in
      visit s_length;; *)

(* a function to test our new string_mapi_up function *)
let test_string_mapi_up f s =
  (string_mapi_up f s = String.mapi f s);;

(* testing function with testing function (and an empty string) *)
let () = assert (test_string_mapi_up
                 (fun i c -> char_of_int (int_of_char c + i))
                 "");;

(* testing function with testing function *)
let () = assert (test_string_mapi_up
                 (fun i c -> char_of_int (int_of_char c + i))
                 "abcdef");;

(* the most important bit: the trace! *)
(* string_mapi_up
  (fun i c -> let () = Printf.printf "index %s\n" (show_int i) in (a_char c))
  "abc";; *)

(*
index 0
processing 'a'...
index 1
processing 'b'...
index 2
processing 'c'...
- : string = "abc"
*)

(* and it works! and why would it not? *)
(* the function iterates over the indices from left to right *)

(* onward to string_mapi_down *)
let string_mapi_down f s =
  nat_parafold_right_down ""
                          (fun n' ih ->
                            String.make 1 (f (n' - 1) (String.get s (n' - 1))) ^ ih)
                          (String.length s);;

(* or, if you would prefer, inlined: *)
(* let string_mapi_down f s =
  let s_length = String.length s in
    let rec visit n =
      if n = s_length
      then ""
      else let n' = n + 1 in
             (String.make 1 (f n (String.get s n))) ^ visit n' in
      visit 0;; *)


(* a function to test our new string_mapi_down function *)
let test_string_mapi_down f s =
  (string_mapi_down f s = String.mapi f s);;

(* testing function with testing function (and an empty string) *)
let () = assert (test_string_mapi_down
                 (fun i c -> char_of_int (int_of_char c + i))
                 "");;

(* testing function with testing function *)
let () = assert (test_string_mapi_down
                 (fun i c -> char_of_int (int_of_char c + i))
                 "abcdef");;

(* the most important bit: the trace! *)
(* string_mapi_down
  (fun i c -> let () = Printf.printf "index %s\n" (show_int i) in (a_char c))
  "abc";; *)

(*
index 2
processing 'c'...
index 1
processing 'b'...
index 0
processing 'a'...
- : string = "abc"
*)

(* and it works! and why would it not? *)
(* the function iterates over the indices from right to left *)

(* ********** *)

(* Question 4 *)

(* a unit test for our future string_reverse function *)
let test_string_reverse string_reverse =
      (* empty string: *)
  let b0 = (string_reverse "" = "")
      (* some intuitive cases: *)
  and b1 = (string_reverse "abc" = "cba")
  and b2 = (string_reverse "AbC" = "CbA")
  and b3 = (string_reverse "hello, " = " ,olleh")
  and b4 = (string_reverse "\\\"" = "\"\\")
  and b5 = (string_reverse " aba " = " aba ")
  in b0 && b1 && b2 && b3 && b4 && b5;;

let string_reverse_mapi s =
  String.mapi
  (fun i _ -> let s_length = String.length s in
                String.get s (s_length - i - 1))
  s;;

(* testing function with unit test *)
let () = assert (test_string_reverse string_reverse_mapi);;

let string_reverse_rec_v0 s =
  let s_length = String.length s in
    if s_length = 0
    then s
    else
      let rec visit n =
        if n = 0
        then String.make 1 (String.get s 0)
        else let n' = n - 1 in
               (String.make 1 (String.get s n)) ^ visit n' in
        visit (s_length - 1);;

(* testing function with unit test *)
let () = assert (test_string_reverse string_reverse_rec_v0);;
(* it passes the unit test. however;
   CONSTANT VIGILANCE! IT SEEMS SO THAT OUR LESSON HAS NOT BEEN LEARNT
   we are still using a mechanism to deal with strings of length 0 rather than
   truly defining our function inductively. *)

let string_reverse_rec_v1 s =
  let s_length = String.length s in
    let rec visit n =
      if n = 0
      then ""
      else let n' = n - 1 in
             (String.make 1 (String.get s n')) ^ visit n' in
      visit s_length;;

(* testing function with unit test *)
let () = assert (test_string_reverse string_reverse_rec_v1);;
(* this updated version now is truly inductively defined,
   and we can define it using nat_parafold_right as well! *)

(* moreso, we can specify what direction we want characters to be accessed! *)

(* characters accessed from left to right *)
let string_reverse_rec_up_traced s =
  nat_parafold_right ""
                     (fun n' ih -> (String.make 1 (a_char (String.get s n'))) ^ ih)
                     (String.length s);;

(* the trace: *)
(*
# string_reverse_rec_up_traced "abc";;
processing 'a'...
processing 'b'...
processing 'c'...
- : string = "cba"
*)

(* and now untraced *)
let string_reverse_rec_up s =
  nat_parafold_right ""
                     (fun n' ih -> String.make 1 (String.get s n') ^ ih)
                     (String.length s);;

let string_reverse_rec_down_traced s =
  nat_parafold_right_down ""
                          (fun n' ih -> ih ^ String.make 1 (a_char (String.get s (pred n'))))
                          (String.length s);;

(* the trace: *)
(*
# string_reverse_rec_down_traced "abc";;
processing 'c'...
processing 'b'...
processing 'a'...
- : string = "cba"
 *)

(* and now untraced *)
let string_reverse_rec_down s =
  nat_parafold_right_down ""
                          (fun n' ih -> ih ^ String.make 1 (String.get s (pred n')))
                          (String.length s);;

(* and both implementations, of course, pass our unit test! *)
let () = assert (test_string_reverse string_reverse_rec_up);;
let () = assert (test_string_reverse string_reverse_rec_down);;

(* No Harumph, Hurrah! *)

(* ********** *)

(* Question 5 *)

let unit_test_string_reverse_for_question_5 string_reverse =
      (* empty string: *)
  let b0 = (string_reverse "" = "")
      (* some intuitive cases: *)
  and b1 = (string_reverse "abc" = "cba")
  and b2 = (string_reverse "AbC" = "CbA")
  and b3 = (string_reverse "hello, " = " ,olleh")
  and b4 = (string_reverse "\\\"" = "\"\\")
  and b5 = (string_reverse " aba " = " aba ")
      (* relation between s1, s2 and s1 ^ s2 *)
  and b6 = (let a = String.init 10 (fun _ -> char_of_int (Random.int 128))
            and b = String.init 10 (fun _ -> char_of_int (Random.int 128))
            in (string_reverse b ^ string_reverse a) = string_reverse (a ^ b))
  (* etc. *)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6;;

(* testing function with unit test *)
let () = assert (unit_test_string_reverse_for_question_5 string_reverse_mapi);;
let () = assert (unit_test_string_reverse_for_question_5 string_reverse_rec_v1);;
let () = assert (unit_test_string_reverse_for_question_5 string_reverse_rec_up);;
let () = assert (unit_test_string_reverse_for_question_5 string_reverse_rec_down);;
(* both functions pass our new unit test as well *)

(* ********** *)

(* Question 6 *)

(* unit test for our future make palindrome function *)
let test_make_palindrome make_palindrome =
      (* empty string: *)
  let b0 = (make_palindrome 0 = "")
      (* definition of palindrome *)
  and b1 = (let a = make_palindrome (Random.int 50)
            in a = string_reverse_mapi a)
  (* etc. *)
  in b0 && b1;;

(* only works on even numbers *)
let make_palindrome_v0 n =
  let a = String.init (n/2) (fun _ -> char_of_int (int_of_char 'a' + Random.int 26))
  in (a ^ string_reverse_mapi a);;

(* works on odd and even numbers *)
let make_palindrome_v1 n =
  let a = String.init (n/2) (fun _ -> char_of_int (int_of_char 'a' + Random.int 26))
  in if (n mod 2) = 1
     then (a ^
           String.init 1 (fun _ -> char_of_int (int_of_char 'a' + Random.int 26)) ^
           string_reverse_mapi a)
     else (a ^ string_reverse_mapi a);;

(*

  output:

  odd number
  # make_palindrome_v1 7;;
  - : string = "gxrzrxg"

  even number
  # make_palindrome_v1 8;;
  - : string = "bvzwwzvb"

*)
let () = assert (test_make_palindrome make_palindrome_v1);;

(* creates only one string (of length n) *)
let make_palindrome_v2 n =
  String.init
  n
  (fun i -> if i < (n/2)
           then char_of_int (int_of_char 'a' + i)
           else if i > (n/2) && n mod 2 = 0
           then char_of_int (int_of_char 'a' + i - 1 - (i mod (n/2)) * 2)
           else if i > (n/2)
           then char_of_int (int_of_char 'a' + i - 2 - (i mod ((n + 1)/2)) * 2)
           else if n mod 2 = 0
           then char_of_int (int_of_char 'a' + i - 1)
           else char_of_int (int_of_char 'a' + i));;

(*

  output:

  odd number
  # make_palindrome_v2 5;;
  - : string = "abcba"

  even number
  # make_palindrome_v2 6;;
  - : string = "abccba"

*)

let () = assert (test_make_palindrome make_palindrome_v2);;

(* creates slightly more random palindromes for odd numbers *)
let make_palindrome_v3 n =
  String.init
  n
  (fun i -> if i < (n/2)
           then char_of_int (int_of_char 'a' + i)
           else if i > (n/2) && n mod 2 = 0
           then char_of_int (int_of_char 'a' + i - 1 - (i mod (n/2)) * 2)
           else if i > (n/2)
           then char_of_int (int_of_char 'a' + i - 2 - (i mod ((n + 1)/2)) * 2)
           else if n mod 2 = 0
           then char_of_int (int_of_char 'a' + i - 1)
           else char_of_int (int_of_char 'a' + Random.int 26));;

(*

  output:

  odd number
  # make_palindrome_v3 5;;
  - : string = "abpba"

  even number
  # make_palindrome_v3 6;;
  - : string = "abccba"

*)

let () = assert (test_make_palindrome make_palindrome_v3);;

(* least intensive, in terms of space and calculations *)
let make_palindrome_v99 n =
  String.make n 'a';;
(* this still fits the specifications of the question :-) *)

(*
  output:

  even number
  # make_palindrome_v99 10;;
  - : string = "aaaaaaaaaa"

  odd number
  # make_palindrome_v99 9;;
  - : string = "aaaaaaaaa"

*)

let () = assert (test_make_palindrome make_palindrome_v99);;

(* ********** *)

(* Question 7 *)

(* unit test for our future palindrome predicate *)
let test_palindromep palindromep =
      (* empty string: *)
  let b0 = (palindromep "")
      (* testing a palindrome *)
  and b1 = (let a = make_palindrome_v3 (Random.int 50)
            in palindromep a)
  in b0 && b1;;

(* using string_reverse_mapi (which uses String.mapi) *)
let palindromep_mapi s =
  string_reverse_mapi s = s;; (* :-) *)

(* alternatively, copy pasting the code *)
let palindromep_mapi_v1 s =
  let s_reversed = String.mapi
                   (fun i _ -> let s_length = String.length s in
                                 String.get s (s_length - i - 1))
                   s in
    s_reversed = s;; (* :-) :-) *)

(* testing function with unit test *)
let () = assert (test_palindromep palindromep_mapi_v1);;

(* using recursion *)
let palindromep_rec s =
  let s_length = String.length s in
    let rec visit n =
      if n = 0
      then true
      (* (String.get s 0) = (String.get s (s_length - 1)) *)
      else let n' = n - 1 in
             (String.get s n' = String.get s (s_length - n)) && visit n' in
      visit s_length;;

(* testing function with unit test *)
let () = assert (test_palindromep palindromep_rec);;

(* however, the following function will also pass the unit test! *)
let palindromep_fake s =
  true;;

(* testing function with unit test *)
let () = assert (test_palindromep palindromep_fake);;

(* hence, to quote djkrista *)
(* about code coverage *)

(* hence, we can improve our unit test to increase code coverage *)
let test_palindromep_v1 palindromep =
      (* empty string: *)
  let b0 = (palindromep "")
      (* testing a palindrome *)
  and b1 = (let a = make_palindrome_v3 (Random.int 50)
            in palindromep a)
      (* testing a string that is not a palindrome *)
  and b2 = (let not_palindrome =
              ("a" ^
              (String.init
               (Random.int 50)
               (fun _ -> char_of_int (int_of_char 'a' + Random.int 26))) ^
              "b")
             in not (palindromep not_palindrome))
  in b0 && b1 && b2;;

(* testing previous functions with unit test *)
let () = assert (test_palindromep_v1 palindromep_mapi);;
let () = assert (test_palindromep_v1 palindromep_mapi_v1);;
let () = assert (test_palindromep_v1 palindromep_rec);;
(* and they still work! *)

(* let () = assert (test_palindromep_v1 palindromep_fake);; *)
(* and of course, our fake function gets caught! *)

(* however, for such a case, we cannot have complete code coeverage *)

(* unlike the following example, where we can have complete code coverage *)
let test_not not_function =
  let b0 = (not_function true = false)
  and b1 = (not_function false = true)
  in b0 && b1;;

let () = assert (test_not not);;

(* hence, since there is full code coverage here, we cannot write a "fake"
   function as the "fake" function will have to fulfill all the criteria
   needed to implement the real function.

   hence, the "fake" function will actually be the same as the real function! *)

(* ********** *)

(* Question 8 (not optional) *)

let reverse_palindrome s =
  s;; (* :-) *)

(* ********** *)

(* Question 10 (optional) *)

let string_map f s =
  String.mapi
  (fun _ c -> f c)
  s;;

(* a function to test our new string_map function *)
let test_string_map f s =
  (string_map f s = String.map f s);;

(* testing function with testing function *)
let () = assert (test_string_map
                (fun c -> char_of_int (int_of_char c + 1))
                "give me a string, any string!");;

(* testing function with testing function and an empty string *)
let () = assert (test_string_map
                 (fun c -> char_of_int (int_of_char c + 1))
                 "");;

(* ********** *)

(* Question 11 (optional) *)

let string_mapi f s =
  String.init
  (String.length s)
  (fun i -> f i (String.get s i));;

(* a function to test our new string_map function *)
let test_string_mapi f s =
  (string_mapi f s = String.mapi f s);;

(* testing function with testing function *)
let () = assert (test_string_mapi
                 (fun i c -> char_of_int (int_of_char c + i))
                 "abcdef");;

(* testing function with testing function and an empty string *)
let () = assert (test_string_mapi
                 (fun i c -> char_of_int (int_of_char c + i))
                 "");;

(*
        OCaml version 4.10.0

# #use "./midterm-project_strings.ml";;
val nat_parafold_right : 'a -> (int -> 'a -> 'a) -> int -> 'a = <fun>
val test_string_append : (string -> string -> string) -> bool = <fun>
val string_append_v0 : string -> string -> string = <fun>
val string_length : string -> int = <fun>
val string_append_traced_v0 : string -> string -> string = <fun>
val string_append_v1 : string -> string -> string = <fun>
val string_append_traced_v1 : string -> string -> string = <fun>
val string_append_v2 : string -> string -> string = <fun>
val string_append_v3 : string -> string -> string = <fun>
val string_append_v4 : string -> string -> string = <fun>
val string_append_v5 : string -> string -> string = <fun>
val test_warmup : (char -> char -> char -> string) -> bool = <fun>
val warmup_v0 : char -> char -> char -> string = <fun>
val warmup_v1 : char -> char -> char -> string = <fun>
val show_char : char -> string = <fun>
val a_char : char -> char = <fun>
val string_map_up_v0 : (char -> char) -> string -> string = <fun>
val test_string_map_up :
  ((char -> char) -> string -> string) -> (char -> char) -> string -> bool =
  <fun>
val string_map_up_v1 : (char -> char) -> string -> string = <fun>
val string_map_up_v2 : (char -> char) -> string -> string = <fun>
val string_map_up_v3 : (char -> char) -> string -> string = <fun>
val test_string_map_down :
  ((char -> char) -> string -> string) -> (char -> char) -> string -> bool =
  <fun>
val string_map_down_v0 : (char -> char) -> string -> string = <fun>
val string_map_down_v1 : (char -> char) -> string -> string = <fun>
val nat_parafold_right_down : 'a -> (int -> 'a -> 'a) -> int -> 'a = <fun>
val string_map_down_v2 : (char -> char) -> string -> string = <fun>
val show_int : int -> string = <fun>
val string_mapi_up : (int -> char -> char) -> string -> string = <fun>
val test_string_mapi_up : (int -> char -> char) -> string -> bool = <fun>
val string_mapi_down : (int -> char -> char) -> string -> string = <fun>
val test_string_mapi_down : (int -> char -> char) -> string -> bool = <fun>
val test_string_reverse : (string -> string) -> bool = <fun>
val string_reverse_mapi : string -> string = <fun>
val string_reverse_rec_v0 : string -> string = <fun>
val string_reverse_rec_v1 : string -> string = <fun>
val string_reverse_rec_up_traced : string -> string = <fun>
val string_reverse_rec_up : string -> string = <fun>
val string_reverse_rec_down_traced : string -> string = <fun>
val string_reverse_rec_down : string -> string = <fun>
val unit_test_string_reverse_for_question_5 : (string -> string) -> bool =
  <fun>
val test_make_palindrome : (int -> string) -> bool = <fun>
val make_palindrome_v0 : int -> string = <fun>
val make_palindrome_v1 : int -> string = <fun>
val make_palindrome_v2 : int -> string = <fun>
val make_palindrome_v3 : int -> string = <fun>
val make_palindrome_v99 : int -> string = <fun>
val test_palindromep : (string -> bool) -> bool = <fun>
val palindromep_mapi : string -> bool = <fun>
val palindromep_mapi_v1 : string -> bool = <fun>
val palindromep_rec : string -> bool = <fun>
val palindromep_fake : 'a -> bool = <fun>
val test_palindromep_v1 : (string -> bool) -> bool = <fun>
val test_not : (bool -> bool) -> bool = <fun>
val reverse_palindrome : 'a -> 'a = <fun>
val string_map : (char -> char) -> string -> string = <fun>
val test_string_map : (char -> char) -> string -> bool = <fun>
val string_mapi : (int -> char -> char) -> string -> string = <fun>
val test_string_mapi : (int -> char -> char) -> string -> bool = <fun>
val end_of_file : string = "midterm-project_strings.ml"

 *)

(* end of midterm-project_strings.ml *)
let end_of_file = "midterm-project_strings.ml";;
