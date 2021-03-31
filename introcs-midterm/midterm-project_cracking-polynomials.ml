(* midterm-project_about-cracking-polynomials.ml *)
(* Introduction to Computer Science (YSC1212), Sem2, 2020-2021 *)
(* Aditya Singhania <adityasinghania@yale-nus.edu.sg> *)
(* Version of Fri 10 Mar 2021, finished crack_0 *)
(* was: *)
(* Version of Wed 24 Feb 2021 *)
(* Version of Sun 14 Feb 2021, with a typo fixed in test_crack_1_once *)

(* Observations (to maybe generalise the elephant in the future):
   n + 1 values needed to solve polynomial of degree n
*)

(* ********** *)

let commiseration = [|"Bummer:";
                      "Bad luck:";
                      "More bad luck:";
                      "Trouble ahead:";
                      "Commiseration city, that's what it is:";
                      "Yes, it's hard to believe, but";
                      "There we go again:";
                      "Truth be told,";
                      "Now that's bizarre:";
                      "Once again,";
                      "Well,";
                      "Unfortunately,";
                      "Argh:";
                      "The proof is not trivial:";
                      "Something is amiss:";
                      "Surprise:";
                      "You need a break:";
                      "I don't want to alarm you, but";
                      "You probably would prefer to receive a random love letter, but";
                      "This is not a pipe dream:";
                    |];;

let random_int () =
  if Random.bool ()
  then Random.int 1000
  else -(Random.int 1000);;

(* ********** *)

let make_polynomial_1 a1 a0 x =
  a1 * x + a0;;

let test_crack_1_once candidate =
  let a1 = random_int ()
  and a0 = random_int ()
  in let p1 = make_polynomial_1 a1 a0
     in let expected_result = (a1, a0)
        and ((a1', a0') as actual_result) = candidate p1
        in if actual_result = expected_result
           then true
           else let () = Printf.printf
                           "%s\ntest_crack_1_once failed for %i * x^1 + %i\nwith %i instead of %i and %i instead of %i\n"
                           (Array.get commiseration (Random.int (Array.length commiseration)))
                           a1 a0 a1' a1 a0' a0
                in false;;

let test_crack_1 candidate =
  test_crack_1_once candidate && test_crack_1_once candidate && test_crack_1_once candidate;;

let crack_1 p1 =
  let a1 = p1 1 - p1 0
  and a0 = p1 0
  in (a1, a0);;

let () = assert (test_crack_1 crack_1);;

let crack_1_v1 p1 =
  let a0 = p1 0
    in let a1 = p1 1 - a0
    in (a1, a0);;
(* with this, we only have to calculate p1 0 once *)

let () = assert (test_crack_1 crack_1);;

(* ********** *)

let make_polynomial_2 a2 a1 a0 x =
  a2 * x * x + a1 * x + a0;;

let test_crack_2_once candidate =
  let a2 = random_int ()
  and a1 = random_int ()
  and a0 = random_int ()
  in let p2 = make_polynomial_2 a2 a1 a0
     in let expected_result = (a2, a1, a0)
        and ((a2', a1', a0') as actual_result) = candidate p2
        in if actual_result = expected_result
           then true
           else let () = Printf.printf
                           "%s\ntest_crack_2_once failed for %i * x^2 + %i * x^1 + %i\nwith %i instead of %i, %i instead of %i, and %i instead of %i\n"
                           (Array.get commiseration (Random.int (Array.length commiseration)))
                           a2 a1 a0 a2' a2 a1' a1 a0' a0
                in false;;

let test_crack_2 candidate =
  test_crack_2_once candidate && test_crack_2_once candidate && test_crack_2_once candidate;;

let crack_2 p2 =
  let a0 = p2 0
  in let a2_add_a1 = p2 1 - a0
     and a2_sub_a1 = p2 ~-1 - a0
     in let a2 = (a2_add_a1 + a2_sub_a1) / 2
        in let a1 = a2_add_a1 - a2
  in (a2, a1, a0);;

let () = assert (test_crack_2 crack_2);;

(* ********** *)

let make_polynomial_0 a0 x =
  a0;;

let test_crack_0_once candidate =
  let a0 = random_int ()
  in let p0 = make_polynomial_0 a0
     in let expected_result = a0
        and (a0' as actual_result) = candidate p0
        in if actual_result = expected_result
           then true
           else let () = Printf.printf
                           "%s\ntest_crack_0_once failed for %i\nwith %i instead of %i\n"
                           (Array.get commiseration (Random.int (Array.length commiseration)))
                           a0 a0' a0'
                in false;;

let test_crack_0 candidate =
  test_crack_0_once candidate && test_crack_0_once candidate && test_crack_0_once candidate;;

(* on first instinct, this is the function that comes to mind *)
let crack_0 p0 =
  p0 0;;

(* and it works! *)
let () = assert (test_crack_0 crack_0);;

(* however, one quickly realises that p0 is a non-strict function!
   hence, we may apply the function to any argument
   and we'll get the same result *)

(* so different implementations like these: *)
let crack_0_v1 p0 =
  p0 "this is a secret message no one will ever see (apart from you)!";;

let crack_0_v2 p0 =
  p0 'a';;

let crack_0_v3 p0 =
  p0 (random_int ());;

let crack_0_v4 p0 =
  p0 "";;

let crack_0_v5 p0 =
  p0 ' ';;

let crack_0_v6 p0 =
  p0 false;;

(* work too! *)
let () = assert (test_crack_0 crack_0_v1);;
let () = assert (test_crack_0 crack_0_v2);;
let () = assert (test_crack_0 crack_0_v3);;
let () = assert (test_crack_0 crack_0_v4);;
let () = assert (test_crack_0 crack_0_v5);;
let () = assert (test_crack_0 crack_0_v6);;


(* even passing in a unit works just fine! *)
let crack_0_v7 p0 =
  p0 ();;

let () = assert (test_crack_0 crack_0_v7);;

(* the magic of non-strict functions *)
(* to me the appear quite similar to declaring a variable *)

(* ********** *)

let make_polynomial_3 a3 a2 a1 a0 x =
  a3 * x * x * x + a2 * x * x + a1 * x + a0;;

let test_crack_3_once candidate =
  let a3 = random_int ()
  and a2 = random_int ()
  and a1 = random_int ()
  and a0 = random_int ()
  in let p3 = make_polynomial_3 a3 a2 a1 a0
     in let expected_result = (a3, a2, a1, a0)
        and ((a3', a2', a1', a0') as actual_result) = candidate p3
        in if actual_result = expected_result
           then true
           else let () = Printf.printf
                           "%s\ntest_crack_3_once failed for %i * x^3 + %i * x^2 + %i * x^1 + %i\nwith %i instead of %i, %i instead of %i, %i instead of %i, and %i instead of %i\n"
                           (Array.get commiseration (Random.int (Array.length commiseration)))
                           a3 a2 a1 a0 a3' a3 a2' a2 a1' a1 a0' a0
                in false;;

let test_crack_3 candidate =
  test_crack_3_once candidate && test_crack_3_once candidate && test_crack_3_once candidate;;

let crack_3 p3 =
  let a0 = p3 0
  in let a3_add_a2_add_a1 = p3 1 - a0
     and min_a3_add_a2_sub_a1 = p3 ~-1 - a0
     in let a2 = (a3_add_a2_add_a1 + min_a3_add_a2_sub_a1) / 2
        in let a3 = (p3 2 - (2 * a3_add_a2_add_a1) - (2 * a2) - a0) / 6
           in let a1 = a3_add_a2_add_a1 - a3 - a2
  in (a3, a2, a1, a0);;

let () = assert (test_crack_3 crack_3);;

(* ********** *)

(* for degree 4 polynomials, I decided to use horner's rule
   because it is more computationally efficient as it has lesser
   multiplication operations *)
let make_polynomial_4 a4 a3 a2 a1 a0 x =
  a0 + x * (a1 + x * (a2 + x * (a3 + x * a4)));;

let test_crack_4_once candidate =
  let a4 = random_int ()
  and a3 = random_int ()
  and a2 = random_int ()
  and a1 = random_int ()
  and a0 = random_int ()
  in let p4 = make_polynomial_4 a4 a3 a2 a1 a0
     in let expected_result = (a4, a3, a2, a1, a0)
        and ((a4', a3', a2', a1', a0') as actual_result) = candidate p4
        in if actual_result = expected_result
           then true
           else let () = Printf.printf
                           "%s\ntest_crack_5_once failed for %i * x^4 + %i * x^3 + %i * x^2 + %i * x^1 + %i\nwith %i instead of %i, %i instead of %i, %i instead of %i, %i instead of %i, and %i instead of %i\n"
                           (Array.get commiseration (Random.int (Array.length commiseration)))
                           a4 a3 a2 a1 a0 a4' a4 a3' a3 a2' a2 a1' a1 a0' a0
                in false;;

let test_crack_4 candidate =
  test_crack_4_once candidate && test_crack_4_once candidate && test_crack_4_once candidate;;

let crack_4 p4 =
  let a0 = p4 0
  in let a4_add_a3_add_a2_add_a1 = p4 1 - a0
     and a4_sub_a3_add_a2_sub_a1 = p4 ~-1 - a0
     in let a4_add_a2 = (a4_add_a3_add_a2_add_a1 + a4_sub_a3_add_a2_sub_a1) / 2
        in let e_12a4_add_8a3_add_2a1 = p4 2 - (4 * a4_add_a2) - a0
           and e_12a4_sub_8a3_sub_2a1 = p4 ~-2 - (4 * a4_add_a2) - a0
           in let a4 = (e_12a4_add_8a3_add_2a1 + e_12a4_sub_8a3_sub_2a1) / 24
              in let a2 = a4_add_a2 - a4
                 in let a3 = (e_12a4_add_8a3_add_2a1 - 12 * a4 - 2 * (a4_add_a3_add_a2_add_a1 - a2 - a4)) / 6
                    in let a1 = a4_add_a3_add_a2_add_a1 - a4 - a3 - a2
  in (a4, a3, a2, a1, a0);;

let () = assert (test_crack_4 crack_4);;

(* ********** *)

(* new degree, same story *)
let make_polynomial_5 a5 a4 a3 a2 a1 a0 x =
  a0 + x * (a1 + x * (a2 + x * (a3 + x * (a4 + x * a5))));;

let test_crack_5_once candidate =
  let a5 = random_int ()
  and a4 = random_int ()
  and a3 = random_int ()
  and a2 = random_int ()
  and a1 = random_int ()
  and a0 = random_int ()
  in let p5 = make_polynomial_5 a5 a4 a3 a2 a1 a0
     in let expected_result = (a5, a4, a3, a2, a1, a0)
        and ((a5', a4', a3', a2', a1', a0') as actual_result) = candidate p5
        in if actual_result = expected_result
           then true
           else let () = Printf.printf
                           "%s\ntest_crack_5_once failed for %i^5 + %i * x^4 + %i * x^3 + %i * x^2 + %i * x^1 + %i\nwith %i instead of %i, %i instead of %i, %i instead of %i, %i instead of %i, %i instead of %i, and %i instead of %i\n"
                           (Array.get commiseration (Random.int (Array.length commiseration)))
                           a5 a4 a3 a2 a1 a0 a5' a5 a4' a4 a3' a3 a2' a2 a1' a1 a0' a0
                in false;;

let test_crack_5 candidate =
  test_crack_5_once candidate && test_crack_5_once candidate && test_crack_5_once candidate;;

let crack_5 p5 =
  let a0 = p5 0
  in let a5_add_a4_add_a3_add_a2_add_a1 = p5 1 - a0
     and min_a5_add_a4_sub_a3_add_a2_sub_a1 = p5 ~-1 - a0
     in let a4_add_a2 = (a5_add_a4_add_a3_add_a2_add_a1 + min_a5_add_a4_sub_a3_add_a2_sub_a1) / 2
        and e_36a5_add_16a4_add_8a3_add_4a2_add_2a1 = p5 2 - a0
        and e_min_36a5_add_16a4_sub_8a3_add_4a2_sub_2a1 = p5 ~-2 - a0
        in let e_32a5_add_12a4_add_8a3_add_2a1 = e_36a5_add_16a4_add_8a3_add_4a2_add_2a1 - 4 * a4_add_a2
           and e_min_32a5_add_12a4_sub_8a3_sub_2a1 = e_min_36a5_add_16a4_sub_8a3_add_4a2_sub_2a1 - 4 * a4_add_a2
           in let a4 = (e_32a5_add_12a4_add_8a3_add_2a1 + e_min_32a5_add_12a4_sub_8a3_sub_2a1) / 24
              in let a2 = a4_add_a2 - a4
                 in let e_243a5_add_81a4_add_27a3_add_9a2_add_3a1 = p5 3 - a0
                    in let e_240a5_add_24a3 = e_243a5_add_81a4_add_27a3_add_9a2_add_3a1 - 3 * a5_add_a4_add_a3_add_a2_add_a1  - 78 * a4 - 6 * a2
                       and e_195a5_add_15a3 = (e_243a5_add_81a4_add_27a3_add_9a2_add_3a1 - 81 * a4 - 9 * a2) -  3 * ((e_32a5_add_12a4_add_8a3_add_2a1 - 12 * a4) / 2)
                       in let a5 = (8 * e_195a5_add_15a3 - 5 * e_240a5_add_24a3 ) / 360
                          in let a3 = (e_195a5_add_15a3  - 195 * a5 ) / 15
                             in let a1 = a5_add_a4_add_a3_add_a2_add_a1 - a5 - a4 - a3 - a2
  in (a5, a4, a3, a2, a1, a0);;

let () = assert (test_crack_5 crack_5);;

(* higher degree to lower degree *)

(* using crack_5 to implement crack_0 *)
let crack_0_using_crack_5 p0 =
  let (a5, a4, a3, a2, a1, a0) = crack_5 p0
    in a0;;

let () = assert (test_crack_0 crack_0_using_crack_5);;

(* using crack_5 to implement crack_1 *)
let crack_1_using_crack_5 p1 =
  let (a5, a4, a3, a2, a1, a0) = crack_5 p1
    in (a1, a0);;

let () = assert (test_crack_1 crack_1_using_crack_5);;

(* using crack_5 to implement crack_2 *)
let crack_2_using_crack_5 p2 =
  let (a5, a4, a3, a2, a1, a0) = crack_5 p2
    in (a2, a1, a0);;

let () = assert (test_crack_2 crack_2_using_crack_5);;

(* using crack_5 to implement crack_3 *)
let crack_3_using_crack_5 p3 =
  let (a5, a4, a3, a2, a1, a0) = crack_5 p3
    in (a3, a2, a1, a0);;

let () = assert (test_crack_3 crack_3_using_crack_5);;

(* using crack_5 to implement crack_4 *)
let crack_4_using_crack_5 p4 =
  let (a5, a4, a3, a2, a1, a0) = crack_5 p4
    in (a4, a3, a2, a1, a0);;

let () = assert (test_crack_4 crack_4_using_crack_5);;

(* ********** *)

(*
        OCaml version 4.10.0

# #use "./midterm-project_cracking-polynomials.ml";;
val commiseration : string array =
  [|"Bummer:"; "Bad luck:"; "More bad luck:"; "Trouble ahead:";
    "Commiseration city, that's what it is:";
    "Yes, it's hard to believe, but"; "There we go again:"; "Truth be told,";
    "Now that's bizarre:"; "Once again,"; "Well,"; "Unfortunately,"; "Argh:";
    "The proof is not trivial:"; "Something is amiss:"; "Surprise:";
    "You need a break:"; "I don't want to alarm you, but";
    "You probably would prefer to receive a random love letter, but";
    "This is not a pipe dream:"|]
val random_int : unit -> int = <fun>
val make_polynomial_1 : int -> int -> int -> int = <fun>
val test_crack_1_once : ((int -> int) -> int * int) -> bool = <fun>
val test_crack_1 : ((int -> int) -> int * int) -> bool = <fun>
val crack_1 : (int -> int) -> int * int = <fun>
val crack_1_v1 : (int -> int) -> int * int = <fun>
val make_polynomial_2 : int -> int -> int -> int -> int = <fun>
val test_crack_2_once : ((int -> int) -> int * int * int) -> bool = <fun>
val test_crack_2 : ((int -> int) -> int * int * int) -> bool = <fun>
val crack_2 : (int -> int) -> int * int * int = <fun>
val make_polynomial_0 : 'a -> 'b -> 'a = <fun>
val test_crack_0_once : (('a -> int) -> int) -> bool = <fun>
val test_crack_0 : (('a -> int) -> int) -> bool = <fun>
val crack_0 : (int -> 'a) -> 'a = <fun>
val crack_0_v1 : (string -> 'a) -> 'a = <fun>
val crack_0_v2 : (char -> 'a) -> 'a = <fun>
val crack_0_v3 : (int -> 'a) -> 'a = <fun>
val crack_0_v4 : (string -> 'a) -> 'a = <fun>
val crack_0_v5 : (char -> 'a) -> 'a = <fun>
val crack_0_v6 : (bool -> 'a) -> 'a = <fun>
val crack_0_v7 : (unit -> 'a) -> 'a = <fun>
val make_polynomial_3 : int -> int -> int -> int -> int -> int = <fun>
val test_crack_3_once : ((int -> int) -> int * int * int * int) -> bool =
  <fun>
val test_crack_3 : ((int -> int) -> int * int * int * int) -> bool = <fun>
val crack_3 : (int -> int) -> int * int * int * int = <fun>
val make_polynomial_4 : int -> int -> int -> int -> int -> int -> int = <fun>
val test_crack_4_once : ((int -> int) -> int * int * int * int * int) -> bool =
  <fun>
val test_crack_4 : ((int -> int) -> int * int * int * int * int) -> bool =
  <fun>
val crack_4 : (int -> int) -> int * int * int * int * int = <fun>
val make_polynomial_5 : int -> int -> int -> int -> int -> int -> int -> int =
  <fun>
val test_crack_5_once :
  ((int -> int) -> int * int * int * int * int * int) -> bool = <fun>
val test_crack_5 :
  ((int -> int) -> int * int * int * int * int * int) -> bool = <fun>
val crack_5 : (int -> int) -> int * int * int * int * int * int = <fun>
val crack_0_using_crack_5 : (int -> int) -> int = <fun>
val crack_1_using_crack_5 : (int -> int) -> int * int = <fun>
val crack_2_using_crack_5 : (int -> int) -> int * int * int = <fun>
val crack_3_using_crack_5 : (int -> int) -> int * int * int * int = <fun>
val crack_4_using_crack_5 : (int -> int) -> int * int * int * int * int =
  <fun>
val end_of_file : string = "midterm-project_about-cracking-polynomials.ml"

 *)

(* end of midterm-project_about-cracking-polynomials.ml *)
let end_of_file = "midterm-project_about-cracking-polynomials.ml";;
