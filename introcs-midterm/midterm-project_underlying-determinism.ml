(* midterm-project_underlying-determinism.ml *)
(* Introduction to Computer Science (YSC1212), Sem2, 2020-2021 *)
(* Aditya Singhania <adityasinghania@yale-nus.edu.sg> *)
(* Version of Mon 1 Mar 2021 *)

(* reference: https://delimited-continuation.github.io/YSC1212/2020-2021_Sem2/week-05_where-do-we-go-from-here.html#food-for-thought *)


(* ********** *)

let show_bool b =
 (* show_bool : bool -> string *)
  if b
  then "true"
  else "false";;

let show_char c =
 (* show_char : char -> string *)
  "'" ^ (if c = '\\' then "\\\\" else if c = '\'' then "\\\'" else String.make 1 c) ^ "'";;

let show_string s =
 (* show_string : string -> string *)
  "\"" ^ s ^ "\"";;

let show_int n =
 (* show_int : int -> string *)
  if n < 0
  then "(" ^ string_of_int n ^ ")"
  else string_of_int n;;

let show_unit () =
 (* show_unit : unit -> string *)
  "()";;

(* ********** *)

let an_int n =
 (* an_int : int -> int *)
  let () = Printf.printf "processing %s...\n" (show_int n)
  in n;;

let an_int_v1 n =
  (* an_int_v1 : int -> int *)
   let () = Printf.printf "processing v1 %s...\n" (show_int n)
   in n;;

let a_bool b =
 (* a_bool : bool -> bool *)
  let () = Printf.printf "processing %s...\n" (show_bool b)
  in b;;

let a_bool_v1 b =
 (* a_bool_v1 : bool -> bool *)
  let () = Printf.printf "processing v1 %s...\n" (show_bool b)
  in b;;

let a_bool_v2 b =
 (* a_bool_v2 : bool -> bool *)
  let () = Printf.printf "processing v2 %s...\n" (show_bool b)
  in b;;

let a_char c =
 (* a_char : char -> char *)
  let () = Printf.printf "processing %s...\n" (show_char c)
  in c;;

let a_string s =
 (* a_string : string -> string *)
  let () = Printf.printf "processing %s...\n" (show_string s)
  in s;;

let a_unit () =
 (* a_unit : unit -> unit *)
  let () = Printf.printf "processing the unit value...\n"
  in ();;

let a_function f =
 (* a_function : ('a -> 'b) -> 'a -> 'b *)
  let () = Printf.printf "processing a function...\n"
  in fun x -> f x;;

(* ********** *)

(* question 02 *)

(* (a_function succ) (an_int 10);; *)

(* output:

  processing 10...
  processing a function...
  - : int = 11

 *)

(* hence, we can see that the argument is evaluated first, and then the function

let's try a function that accepts 2 arguments, perhaps the + function? *)

(* (a_function (+)) (an_int 3) (an_int 5);; *)

(*

  processing 5...
  processing 3...
  processing a function...
  - : int = 8

 *)

(* additionally, arguments are evaluated from right to left *)

(* this fits in perfectly with question 1, where:
  1. the integer on the rigth side of the + operand is evaluated first
  2. the integer on the left side of the + operand is evaluated next
  3. finally the + operand (or, rather, the "+" function) is evaluated in the end *)

(* finally, maybe also a function that accepts 3 arguments? *)

let mul_3 x y z = x * y * z;;
(* (a_function (mul_3)) (an_int 2) (an_int 6) (an_int 3);; *)

(*

  processing 3...
  processing 6...
  processing 2...
  processing a function...
  - : int = 36

 *)

(* here too, the arguments are evaluated from right to left *)

(*

  # mul_3;;
  - : int -> int -> int -> int = <fun>

 *)

(* this is actually a euphimism for what is happening.
   We discovered that in a function application, the argument is evaluated first,
   and then the function itself. And... what a "function with multiple arguments"
   actually is, is simply a function that is applied a function that is applied
   to a function that is applied to an integer. And hence, our rule of fucntion
   application remains consistent, even if we have "multiple arguments" *)

(* this also makes sense, because we cannot apply a function to a value without knowing what that value is!
   (unless, of course, it is a non-strict function; however, even in that case the argument is evaluated first,
    just because we cannot know whether a function is non-strict or not in advance) *)


(* ********** *)

(* question 03 *)

(* let pair_int_int = ((an_int 5), (an_int 2));; *)
(*
processing 2...
processing 5...
val pair_int_int : int * int = (5, 2)
 *)

(* let pair_int_bool = ((an_int 3), (a_bool true));; *)
(*
processing true...
processing 3...
val pair_int_bool : int * bool = (3, true)
 *)

(* let pair_bool_char = ((a_bool false), (a_char 'a'));; *)
(*
processing 'a'...
processing false...
val pair_bool_char : bool * char = (false, 'a')
 *)

(* hence, pairs are processed from right to left *)

(* what about a pair of pairs? a pair pair! *)
(* let pair_pair = (((an_int 3), (a_bool true)), ((a_bool false), (a_char 'a')));; *)
(*
processing 'a'...
processing false...
processing true...
processing 3...
val pair_pair : (int * bool) * (bool * char) = ((3, true), (false, 'a'))
 *)

(* a pair of pairs is consistent with our earlier processing direction!
   it's components, i.e. pairs themsleves, are evaluated from right to left;
   subsequently, the components within these pairs are themselves evaluated
   from right to left! *)

(* how about a triple? a quadruple? a quintiple even? *)

(* let triple = ((an_int 3), (a_bool true), (a_char 'c'));; *)
(*
processing 'c'...
processing true...
processing 3...
val triple : int * bool * char = (3, true, 'c')
 *)

(* let quadruple = ((an_int 3), (a_bool true), (a_bool false), (a_char 'a'));; *)
(*
processing 'a'...
processing false...
processing true...
processing 3...
val quadruple : int * bool * bool * char = (3, true, false, 'a')
 *)

(* let quintuple = ((an_int 3), (a_bool true), (a_bool false), (a_char 'a'), (a_string "ida"));; *)
(*
processing "ida"...
processing 'a'...
processing false...
processing true...
processing 3...
val quintuple : int * bool * bool * char * string =
  (3, true, false, 'a', "ida")
 *)

(* triples, quadruples, and quintuples are also consistent!
   their components are evaluated from right to left. *)

(* hence, tuples in general are evaluated from right to left, regardless of the types of their components, of course *)

(* ********** *)

(* question 04 *)

(* are the following expressions equivalent?
   (fun x1 -> fun x2 -> e0) e1 e2
   (fun (x1, x2) -> e0) (e1, e2)
*)

(*

(* the curried function (the haskell kind, not the indian culinary delight) *)

to evaluate "(fun x1 -> fun x2 -> e0) e1 e2":
(* application associates to the left, hence the above is equivalent to ((fun x1 -> fun x2 -> e0) e1) e2 *)
to evaluate "((fun x1 -> fun x2 -> e0) e1) e2", we must first evaluate e2 (from what we learnt in question 02)
  - if evaluating e2 does not terminate then the entire evaluation does not terminate
  - if evaluating e2 raises an error, then the entire evaluation raises this error
  - if evaluating e2 evaluates to v2 with type t2, then
    - we evaluate ((fun x1 -> fun x2 -> e0) e1)
    - to evaluate ((fun x1 -> fun x2 -> e0) e1), we must first evaluate e1:
      - if evaluating e1 does not terminate then the entire evaluation does not terminate
      - if evaluating e1 raises an error, then the entire evaluation raises this error
      - if evaluating e1 evaluates to v1 with type t1, then
        - evaluating ((fun x1 -> fun x2 -> e0) e1) reduces to evaluating (fun x2 -> e0) in an environment where x1 denotes v1
          - evaluating (fun x2 -> e0) returns a function, then
          - evaluating ((fun x1 -> fun x2 -> e0) e1) e2 reduces to applying "this" function (the function we got from the previous line) to v2
            - applying "this" function reduces to evaluating e0 in an environment where x2 denotes v2.
            - because "this" function was evaluated in an environment where x1 denoted v1, the environment of e0 also contains x1 denoting v1.

*)

(*

(* the uncurried function *)

to evaluate "(fun (x1, x2) -> e0) (e1, e2)", we must first evaluate (e1, e2)
  - to evaluate (e1, e2), we first evaluate e2 (from what we learnt in question 03)
    - if evaluating e2 does not terminate then the entire evaluation does not terminate
    - if evaluating e2 raises an error, then the entire evaluation raises this error
    - if evaluating e2 evaluates to v2 with type t2, then we evaluate e1
      - if evaluating e1 does not terminate then the entire evaluation does not terminate
      - if evaluating e1 raises an error, then the entire evaluation raises this error
      - if evaluating e1 evaluates to v1 with type t1, then
        - the result of (e1, e2) is the pair (v1, v2), then
          - we evaluate (fun (x1, x2) -> e0)
            - evaluating (fun (x1, x2) -> e0) returns a function, then
            - evaluating "(fun (x1, x2) -> e0) (e1, e2)" reduces to applying "this" function (the function we got from the previous line) to (v1, v2)
              - applying the function "this" to (v1, v2) reduces to evaluating e0 in an environment where x1 denotes v1 and x2 denotes v2

*)

(*

to sum up, in both cases:
  - e2 is the component that is evaluated first, and, hence,
    - if e2 diverges, then in both cases the evaluation does not terminate
    - if e2 raises an error, in both cases the evaluation raises that errors
    - if e2 has a side effect, in both cases e2 makes the same side effect
    - if e2 yields a value v2, the value is the same in both cases
  - e1 is the component that is evaluated next, and, hence,
    - if e1 diverges, then in both cases the evaluation does not terminate
    - if e1 raises an error, in both cases the evaluation raises that errors
    - if e1 has a side effect, in both cases e1 makes the same side effect
    - if e1 yields a value v1, the value is the same in both cases
  - finally, if e1 and e2 yielded v1 and v2 respectively, Then
    - e0 is evaluated in an environment where x1 denotes v1 and x2 denotes v2

Hence, each of these expressions give rise to the same evaluation/computation.
Therefore, they are equivalent!

(* this is regardless of whether e0, e1 or e2 are pure or impure *)

*)

(* subsidary questions:

our answer is compatible with question 02 and question 03 as:
  - the argument is evaluated first, before the function itself is applied to it
    - hence, e2 was evaluated first in the first expression, and
    - hence, (e1, e2) was evaluated first in the second expression
  - tuples, as we saw, are evaluated from right to left (hence, e2 was evaluated before e1)

- Assuming that e0 has type t0, x1 has type t1, and x2 has type t2, what are the types of fun x1 -> fun x2 -> e0 and fun (x1, x2) -> e0?

the type of "fun x1 -> fun x2 -> e0" is "t1 -> t2 -> t0"

as seen in the following example:
# fun x1 -> fun x2 -> (x2, x1);;
- : 'a -> 'b -> 'b * 'a = <fun>

where 'a is of type t1, 'b is of type t2 and 'b * 'a is of type t0

and, the type of "fun (x1, x2) -> e0" is "t1 * t2 -> t0"

as seen in the following example:
# fun (x1, x2) -> (x2, x1);;
- : 'a * 'b -> 'b * 'a = <fun>

where 'a is of type t1, 'b is of type t2 and 'b * 'a is of type t0

*)


(* ********** *)

(* question 05 *)

(*

to evaluate "(fun x1 -> e0) e1", we must first evaluate e1 (from what we learnt in question 02)
  - if evaluating e1 does not terminate then the entire evaluation does not terminate
  - if evaluating e1 raises an error, then the entire evaluation raises this error
  - if evaluating e1 evaluates to v1 with type t1, then we evaluate (fun x1 -> e0)
    - evaluating (fun x1 -> e0) returns a function, then
    - evaluating "(fun x1 -> e0) e1" reduces to applying "this" function (the function we got from the previous line) to v1
      - applying the function "this" to v1 reduces to evaluating e0 in an environment where x1 denotes v1

*)

(* let x1 = e1 in e0 *)
(* let x1 = (an_int_v1 8)
  in (an_int x1) + (an_int 1);; *)

(*
processing v1 8...
processing 1...
processing 8...
- : int = 9
*)

(* as we can see, the definien is evaluated first, hence we have the compuation below: *)

(*

to evaluate "let x1 = e1 in e0", we must first evaluate e1 (from what we learnt with the previous output)
  - if evaluating e1 does not terminate then the entire evaluation does not terminate
  - if evaluating e1 raises an error, then the entire evaluation raises this error
  - if evaluating e1 evaluates to v1 with type t1, then
    - evaluating "let x1 = e1 in e0" reduces to evaluating e0 in an environment where x1 denotes v1

*)

(*

to sum up, in both cases:
  - e1 is the component that is evaluated first, and, hence,
    - if e1 diverges, then in both cases the evaluation does not terminate
    - if e1 raises an error, in both cases the evaluation raises that errors
    - if e1 has a side effect, in both cases e1 makes the same side effect
    - if e1 yields a value v1, the value is the same in both cases, and then
      - e0 is evaluated in an environment where x1 denotes v1

hence, each of these expressions give rise to the same evaluation/computation.
therefore, they are equivalent!

(* this is regardless of whether e0, e1 or e2 are pure or impure *)

*)

(* ********** *)

(* question 06 *)

(* let x1 = (an_int 9) and x2 = (an_int 10) and x3 = (an_int 11);; *)
(*
processing 9...
processing 10...
processing 11...
val x1 : int = 9
val x2 : int = 10
val x3 : int = 11
 *)

(* hence, definienses are evaluated from left to right *)

(* this made me question whether the following would work:
let x1 = 40 and x2 = 2 and x3 = x1 + x2;;
this yielded the following error, claiming that x1 was unbound

# let x1 = 40 and x2 = 2 and x3 = x1 + x2;;
Line 1, characters 32-34:
1 | let x1 = 40 and x2 = 2 and x3 = x1 + x2;;
                                    ^^
Error: Unbound value x1

I realised, that this was because x3 was evaluated in an environment where
x1 and x2 were not declared *)

(* ********** *)

(* question 07 *)

(* let x1 = e1 and x2 = e2 in e0 *)
(* let x1 = (an_int 12) and x2 = (an_int 13) in (an_int x1) + (an_int x2);; *)
(*
processing 12...
processing 13...
processing 13...
processing 12...
- : int = 25
 *)

(* (fun (x1, x2) -> e0) (e1, e2) *)
(* (fun (x1, x2) -> (an_int x1) + (an_int x2)) ((an_int 12), (an_int 13));; *)
(*
processing 13...
processing 12...
processing 13...
processing 12...
- : int = 25
 *)

(* hence, the two expressions are not equivalent, as there exists an e1,
   namely "an_int 12", and and e2, namely "an_int 13", which lead to different
   computations, as we can see by the different outputs.

   this is because in the first expression, e1 is evaluated before e2.
   however, in the second expression, e2 is evaluated before e1.
   hence, their order of evaluation is different.

*)

(* ********** *)

(* question 08 *)

(* we use two differnet version of a_bool to find the true direction in which
   boolean conjuncts are evaluated. *)

(* (a_bool true) && (a_bool_v1 true);; *)
(*
processing true...
processing v1 true...
- : bool = true
 *)

(* (a_bool false) && (a_bool_v1 true);; *)
(*
processing false...
- : bool = false
 *)

(* (a_bool true) && (a_bool_v1 false);; *)
(*
processing true...
processing v1 false...
- : bool = false
 *)

(* (a_bool false) && (a_bool_v1 false);; *)
(*
processing false...
- : bool = false
 *)

(* we use three different versions of a_bool to know the true direction
   that conjuncts are evaluated. otherwise, it would be equally valid to claim
   with the output of (a_bool true) && (a_bool true) && (a_bool true), which
   would be "processing true..." three times that the three conjuncts
   are evaluated in random order *)

(* (a_bool true) && (a_bool_v1 true) && (a_bool_v2 true);; *)
(*
processing true...
processing v1 true...
processing v2 true...
- : bool = true
 *)

(* (a_bool true) && (a_bool_v1 true) && (a_bool_v2 false);; *)
(*
processing true...
processing v1 true...
processing v2 false...
- : bool = false
 *)

(* (a_bool false) && (a_bool_v1 true) && (a_bool_v2 true);; *)
(*
# processing false...
- : bool = false
 *)

(* (a_bool false) && (a_bool_v1 false) && (a_bool_v2 false);; *)
(*
# processing false...
- : bool = false
 *)

(* hence, the conjuncts are evaluated from left to right.
   further, if the result can be known by only evaluating the first conjuct,
   the remaining conjuncts will not be evaluated. *)

(* subsidary questions:

  - this design makes sense as we don't need to make all the evaluations if
    can gather the result in advance. for instance, for "false && b1", we
    can instantly evaluate the result to false after reading simply the first
    conjunction. this is because, no matter what the value of b1 is
    (true or false), the expression will evaluate to false.

    this is known as short-circuit evaluation!
    reference: https://delimited-continuation.github.io/YSC1212/2020-2021_Sem2/week-07_on-simplifying-ocaml-expressions.html#index-3

    this means that second conjunction is only evaluated if the first
    conjunction is not able tell us the answer of the expression.

  - this design would also be helpful in two other cases:
    - boolean disjunction (or) (||)

(* (a_bool true) || (a_bool_v1 true);; *)
(*
processing true...
- : bool = true
 *)

(* (a_bool false) || (a_bool_v1 true);; *)
(*
# processing false...
processing v1 true...
- : bool = true
 *)

(* (a_bool true) || (a_bool_v1 false);; *)
(*
# processing true...
- : bool = true
 *)

(* (a_bool false) || (a_bool_v1 false);; *)
(*
# processing false...
processing v1 false...
- : bool = false
 *)

on the side, how about we also try a predefined conjunct?

let b2 = false;;
let to_be = b2;;
to_be || not to_be;;

val b2 : bool = false
# val to_be : bool = false
# - : bool = true

and now, traced.

(a_bool to_be) || not (a_bool_v1 to_be);;

processing false...
processing v1 false...
- : bool = true

(* a pun is here, that is indeed no question *)

      - as we can see, conjuncts are again evaluated from left to right.
        further, similar to earlier, short-circuit evaluation is applied.
        hence, if the first conjunct is true, no matter what the next conjunct(s),
        we know that the expression will evaluate to true. therefore, the result
        can be obtained after only simply evaluating the first conjunct.

    - multiplication with 0

      - we could also potentially use this in multiplication with 0.
      - for instance, 0 * x, will evaluate to 0 no matter what the value of x
        may be.
      - however, this is not how OCaml will go about evaluating 0 * x, as
        explained in question 04.

    - a similar evaluation was also implemented in "string_append_v5", where if
      and only if s1 and s2 are not empty strings is the appendment evaluated.
      hence, in some way, we only make further evaluation if we cannot
      already obtain the answer by our original evaluation.
*)

(* ********** *)

(* question 09 *)

(*

reminders:

- for any expressions e0 and e1, evaluating e0 e1 requires first evaluating e1;

- if evaluating e1 completes, it yields a value v1 (let us say of type t1, for some t1),
  then evaluating e0 e1 requires evaluating e0;

- if evaluating e0 completes, it yields a value v0, which is a function (let us say of type t1 -> t2, for some t2), due to the typing constraints of OCaml;

- evaluating e0 e1 then reduces to applying v0 to v1;

- if this application completes, it yields a value v2 of type t2, due to the typing constraints of OCaml;

- for any expressions e1 and e2, e1 * e2 is syntactic sugar for Int.mul e1 e2, or more precisely for (Int.mul e1) e2 since application associates to the left;

- So evaluating e1 * e2 takes the same steps as evaluating (Int.mul e1) e2:

- evaluating (Int.mul e1) e2 requires first evaluating e2;

- if evaluating e2 completes, it yields a value v2, which is of type int, due to the typing constraints of OCaml
  then evaluating (Int.mul e1) e2 requires evaluating Int.mul e1;

- evaluating Int.mul e1 requires first evaluating e1;

- if evaluating e1 completes, it yields a value v1, which is of type int, due to the typing constraints of OCaml;
  then evaluating Int.mul e1 requires evaluating Int.mul;

- evaluating Int.mul yields the value this variable denotes in the current environment; this value is a function of type int -> int -> int, due to the typing constraints of OCaml;

- evaluating Int.mul e1 then reduces to applying this function to v1; the result is a function of type int -> int, due to the typing constraints of OCaml;

- evaluating (Int.mul e1) e2 then reduces to applying this function to v2; the result is a value of type int, due to the typing constraints of OCaml; more precisely, this value is the result of multiplying v1 by v2, because the function denoted by Int.mul implements the multiplication function.

Here is a simplified version:

- evaluating e1 * e2 requires first evaluating e2;

- if evaluating e2 completes, it yields a value v2, which is of type int, due to the typing constraints of OCaml;
  then evaluating e1 * e2 requires evaluating e1;

- if evaluating e1 completes, it yields a value v1, which is of type int, due to the typing constraints of OCaml;

- evaluating e1 * e2 then reduces to multiplying v1 by v2; the result is a value of type int, due to the typing constraints of OCaml.

*)

(* a. For any pure expression v of type int, would it be valid to simplify v * 0 into 0? *)

(*

- first of all, we note that the expressions "v" and "0" are pure.
- evaluating "v * 0" requires first evaluating 0. evaluating 0 yields 0. then evaluating "v * 0" requires evaluating v.
- since v is a pure expression, evaluating v, a variable of type int, silently yields its value in the current environment, v1, an Integer.
- then, evaluating "v * 0" reduces to multiplying 0 by v1.
- since 0 is absorbing for multiplcation, 0 multiplied by v1 evaluates to 0.
hence, the result of evaluating "v * 0" is 0.

- evaluating "0" yields 0.
- hence, the result of evaluating "0" is 0.

to sum up, in both cases:
- the evaluation silently yields 0.

*)

(* hence, it is valid to simplify v * 0 into 0 as they both are observationally equivalent, given that v is pure, of course. *)

(* what about 0 * v to 0? *)

(*

- first of all, we note that the expressions "v" and "0" are pure.
- evaluating "0 * v" requires first evaluating v.
- since v is a pure expression, evaluating v, a variable of type int, silently yields its value in the current environment, v1, an Integer. then, evaluating "0 * v" requires evaluating 0
- evaluating 0 yields 0.
- then, evaluating "0 * v" reduces to multiplying v1 by 0.
- since 0 is absorbing for multiplcation, 0 multiplied by v1 evaluates to 0.
hence, the result of evaluating "0 * v" is 0.

- evaluating "0" yields 0.
- hence, the result of evaluating "0" is 0.

to sum up, in both cases:
- the evaluation silently yields 0.

*)

(* hence, it is valid to simplify 0 * v into 0 as they both are observationally equivalent, given that v is pure, of course. *)

(* b. For any potentially impure expression e of type int, would it be valid to simplify e * 0 into 0? *)

(* (an_int 14) * 0;; *)
(*
processing 14...
- : int = 0
 *)

(* 0;; *)
(*
- : int = 0
 *)

(* hence, it is invalid to simplify e * 0 to 0, if e is potentially impure; as there exists an e, namely (an_int 14) such that
e * 0 and 0 are not observationally equivalent. *)

(* c. For any pure expression v of type int, would it be valid to simplify v * 1 into v? *)

(*

- first of all, we note that the expressions "v" and "1" are pure.
- evaluating "v * 1" requires first evaluating 1. evaluating 1 yields 1. then evaluating "v * 1" requires evaluating v.
- since v is a pure expression, evaluating v, a variable of type int, silently yields its value in the current environment, v1, an Integer.
- then, evaluating "v * 1" reduces to multiplying 1 by v1.
- the identity property of multiplication states that any number multiplied by 1 keeps its identity, i.e. the number stays the same. hence, 1 multiplied by v1 evaluates to v1.
hence, the result of evaluating "v * 1" is v1.

- since v is a pure expression, evaluating v, a variable of type int, silently yields its value in the current environment, v1, an Integer.
- hence, the result of evaluating "v" is v1.

to sum up, in both cases:
- the evaluation silently yields v1.

*)

(* hence, it is valid to simplify v * 1 into v as they both are observationally equivalent *)

(* d. For any potentially impure expression e of type int, would it be valid to simplify e * 1 into e? *)

(*

- evaluating "e * 1" requires first evaluating 1. evaluating 1 yields 1. then evaluating "e * 1" requires evaluating e.
- since e may be potentially impure, evaluating e either diverges, or raises an error, or yields a value, possibly performing side effects in passing. this value is an integer, due to the typing constaints of Ocaml;
  - if evaluating e does not terminate then the entire evaluation does not terminate
  - if evaluating e raises an error, then the entire evaluation raises this error
  - if evaluating e evaluates to v1, which is of type int, then;
    - evaluating "e * 1" reduces to multiplying 1 by v1.
    - the identity property of multiplication states that any number multiplied by 1 keeps its identity, i.e. the number stays the same. hence, 1 multiplied by v1 evaluates to v1.
    - hence, the result of evaluating "e * 1" is v1.

- evaluating "e" requires evaluting e
- since e may be potentially impure, evaluating e either diverges, or raises an error, or yields a value, possibly performing side effects in passing. this value is an integer, due to the typing constaints of Ocaml;
  - if evaluating e does not terminate (i.e. diverges), then the entire evaluation does not terminate
  - if evaluating e raises an error, then the entire evaluation raises this error
  - if evaluating e evaluates to v1, which is of type int, then;
    - the result of evaluating "e" is v1.

to sum up, in both cases:
- if evaluating e diverges, then evaluating each of the expressions diverges too, which leads us to conclude that in this case, they are observationally equivalent.
- if evaluating e raises an error, then evaluating each of the expressions raises this error, which leads us to conclude that in this case, they are observationally equivalent.
- if evaluating e yields a value, performing side effects in passing, then since e is first evaluated when evaluating "e * 1", and "e", these side effects occur in the same order. the rest of the evaluations takes place as just described in c., which leads us to conclude that in this case, they are observationally equivalent.
- if evaluating e yields a value, performing no side effects in passing, then e is pure, which as just described in c., leads us to conclude that in this case, they are observationally equivalent. since e may be potentially impure,

*)

(* hence, it is valid to simplify e * 1 to v, if e is potentially impure *)


(* ********** *)

(* exercise 10 *)

(*
a. Are the two following expressions equivalent (i.e., does evaluating them carry out the same computation?):

let x1 = e1 and x2 = e2 in (x1, x2);;
let x2 = e2 and x1 = e1 in (x1, x2);;

*)

(* let x1 = (an_int 17) and x2 = (an_int 18) in (x1, x2);; *)
(*
processing 17...
processing 18...
- : int * int = (17, 18)
 *)

(* let x2 = (an_int 18) and x1 = (an_int 17) in (x1, x2);; *)
(*
processing 18...
processing 17...
- : int * int = (17, 18)
 *)

(* hence, the two expressions are not equivalent, as thereee exists an e1,
   namely "an_int 17", and and e2, namely "an_int 18", which lead to different
   computations, as we can see by the different outputs.

   this is because in the first expression, e1 is evaluated before e2.
   however, in the second expression, e2 is evaluated before e1.
   hence, their order of evaluation is different.

*)

(*

b. Are the two following expressions equivalent (i.e., does evaluating them carry out the same computation?):

let x1 = e1 in let x2 = e2 in (x1, x2);;
let x2 = e2 in let x1 = e1 in (x1, x2);;

*)

(* let x1 = (an_int 17) in let x2 = (an_int 18) in (x1, x2);; *)
(*
processing 17...
processing 18...
- : int * int = (17, 18)
 *)

(* let x2 = (an_int 18) in let x1 = (an_int 17) in (x1, x2);; *)
(*
processing 18...
processing 17...
- : int * int = (17, 18)
 *)

(* hence, the two expressions are not equivalent, as there exists an e1,
   namely "an_int 17", and and e2, namely "an_int 18", which lead to different
   computations, as we can see by the different outputs.

   this is because in the first expression, e1 is evaluated before e2.
   however, in the second expression, e2 is evaluated before e1.
   hence, their order of evaluation is different.

*)

(* ********** *)

(* exercise 11 *)

(*

a. Are the two following expressions equivalent (i.e., does evaluating them carry out the same computation?):

let x1 = v1 and x2 = v2 in (x1, x2);;
let x2 = v2 and x1 = v1 in (x1, x2);;

*)

(*

- first of all, we note that the expressions "v1" and "v2" are pure.
- to evaluate "let x1 = v1 and x2 = v2 in (x1, x2)", we must first evaluate v1 (from what we learnt in question 06)
- since v1 is a pure expression, evaluating v1, a variable of type t1, silently yields its value in the current environment, v_1 of type t1. then evaluating "let x1 = v1 and x2 = v2 in (x1, x2)" requires evaluating v2
- since v2 is a pure expression, evaluating v2, a variable of type t2, silently yields its value in the current environment, v_2 of type t2.
- then, evaluating "let x1 = v1 and x2 = v2 in (x1, x2)" reduces to evaluating (x1, x2) in an environment, G, where x1 denotes v_1 of type t1 and x2 denotes v_2 of type t2
  - to evaluate (x1, x2) in G, we first evaluate x2 (from what we learnt in question 03)
    - evaluating x2 silently yields its value in the current environment, v_2 of type t2, then we evaluate x1
    - evaluating x1 silently yields its value in the current environment, v_1 of type t1, then
    - evaluating (x1, x2) in G yields (v1, v2), which is the result of the entire evaluation

- first of all, we note that the expressions "v1" and "v2" are pure.
- to evaluate "let x2 = v2 and x1 = v1 in (x1, x2), we must first evaluate v2 (from what we learnt in question 06)
- since v2 is a pure expression, evaluating v2, a variable of type t2, silently yields its value in the current environment, v_2 of type t2. then evaluating "let x2 = v2 and x1 = v1 in (x1, x2) requires evaluating v1
- since v1 is a pure expression, evaluating v1, a variable of type t1, silently yields its value in the current environment, v_1 of type t1.
- then, evaluating "let x2 = v2 let x1 = v1 in (x1, x2)" reduces to evaluating (x1, x2) in an environment, G, where x1 denotes v_1 of type t1 and x2 denotes v_2 of type t2
  - to evaluate (x1, x2) in G, we first evaluate x2 (from what we learnt in question 03)
    - evaluating x2 silently yields its value in the current environment, v_2 of type t2, then we evaluate x1
    - evaluating x1 silently yields its value in the current environment, v_1 of type t1, then
    - evaluating (x1, x2) in G yields (v1, v2), which is the result of the entire evaluation

to sum up, in both cases:
- the evaluation silently yields (v1, v2)

*)

(* hence, these two expressions are equivalent *)

(*

b. Are the two following expressions equivalent (i.e., does evaluating them carry out the same computation?):

let x1 = v1 in let x2 = v2 in (x1, x2);;
let x2 = v2 in let x1 = v1 in (x1, x2);;

*)

(*

- first of all, we note that the expressions "v1" and "v2" are pure.
- to evaluate "let x1 = v1 in let x2 = v2 in (x1, x2)", we must first evaluate v1 (from what we learnt in question 06)
- since v1 is a pure expression, evaluating v1, a variable of type t1, silently yields its value in the current environment (the current environment being G0), v_1 of type t1. then evaluating "let x1 = v1 in let x2 = v2 in (x1, x2)" reduces to evaluating "let x2 = v2 in (x1, x2)" in an environment, G1, where x1 denotes v_1 of type t1
- to evaluate "let x2 = v2 in (x1, x2)", we must first evaluate v2
- since v2 is a pure expression, evaluating v2, a variable of type t2, silently yields its value in the current environment, v_2 of type t2.
- then, evaluating "let x2 = v2 in (x1, x2)" reduces to evaluating (x1, x2) in an environment, G2, where x2 denotes v_2 of type t2 and x1 denotes v_1 of type t1
  - to evaluate (x1, x2) in G2, we first evaluate x2 (from what we learnt in question 03)
    - evaluating x2 silently yields its value in the current environment, v_2 of type t2, then we evaluate x1
    - evaluating x1 silently yields its value in the current environment, v_1 of type t1, then
    - evaluating (x1, x2) in G2 yields (v1, v2), which is the result of the entire evaluation

- first of all, we note that the expressions "v1" and "v2" are pure.
- to evaluate "let x2 = v2 in let x1 = v1 in (x1, x2)", we must first evaluate v2 (from what we learnt in question 06)
- since v2 is a pure expression, evaluating v2, a variable of type t2, silently yields its value in the current environment (the current environment being G0), v_2 of type t1. then evaluating "let x2 = v2 in let x1 = v1 in (x1, x2)" reduces to evaluating "let x1 = v1 in (x1, x2)" in an environment, G1, where x2 denotes v_2 of type t2
- to evaluate "let x1 = v1 in (x1, x2)", we must first evaluate v1
- since v1 is a pure expression, evaluating v1, a variable of type t1, silently yields its value in the current environment, v_1 of type t1.
- then, evaluating "let x1 = v1 in (x1, x2)" reduces to evaluating (x1, x2) in an environment, G2, where x1 denotes v_1 of type t1 and x2 denotes v_2 of type t2
  - to evaluate (x1, x2) in G2, we first evaluate x2 (from what we learnt in question 03)
    - evaluating x2 silently yields its value in the current environment, v_2 of type t2, then we evaluate x1
    - evaluating x1 silently yields its value in the current environment, v_1 of type t1, then
    - evaluating (x1, x2) in G2 yields (v1, v2), which is the result of the entire evaluation

to sum up,

in the first case:
- v1 is evaluated in the environment G0
- v2 is evaluated in the environment G1

in the second case:
- v1 is evaluated in the environment G1
- v2 is evaluated in the environment G0

hence, we see how v1 and v2 may yield different values in the two expressions.
therefore, the two expressions are not observationally equivalent.

to wit:

let x1 = 5;;
(* # val x1 : int = 5 *)
let x2 = 10;;
(* # val x2 : int = 10 *)

let x1 = x1 + x2 in let x2 = x1 + x2 in (x1, x2);;
(* # - : int * int = (15, 25) *)
let x2 = x1 + x2 in let x1 = x1 + x2 in (x1, x2);;
(* # - : int * int = (20, 15) *)

however, if v1 and v2 do not contain either, x1 or x2, then they become observationally equivalent.
this is because G1 only adds either x1 or x2 to the current environment.
hence, if either of these variables that have been newly added to the environment are not used in v1 or v2, then evaluating them in G1 would be the same as evaluating them in G0.
therefore, if v1 and v2 do not contain either, x1 or x2, then they become observationally equivalent. (to clarify: they must not contain x1 and they must not contain x2)

*)

(* hence, these two expressions are equivalent *)

(* ********** *)

(* exercise 12 *)

let fun_non_strict _ = 42;; (* assigning to the function the meaning of life *)
(* fun_non_strict (Random.int 100);; (* applying the function to a *) *)
(* - : int = 42 *)
(* fun_non_strict ("any argument, no really, any!");; (* applying the function to a string *) *)
(* - : int = 42 *)
(* fun_non_strict ('a');; (* applying the function to a char *) *)
(* - : int = 42 *)
(* fun_non_strict (false);; (* applying the function to a bool *) *)
(* - : int = 42 *)
(* fun_non_strict ();; (* applying the function to the unit value *) *)
(* - : int = 42 *)
(* fun_non_strict (fun x -> x + 1);; (* applying the function to a function *) *)
(* - : int = 42 *)

(* I mean, since we're not really using the argument, is the function still only evaluated after the argument? *)

(* (a_function fun_non_strict) (an_int (Random.int 100));; *)
(*
processing 30...
processing a function...
- : int = 42
 *)

(* (a_function fun_non_strict) (a_string "any argument, no really, any!");; *)
(*
processing "any argument, no really, any!"...
processing a function...
- : int = 42
 *)

(* (a_function fun_non_strict) (a_char 'a');; *)
(*
processing 'a'...
processing a function...
- : int = 42
 *)

(* (a_function fun_non_strict) (a_bool false);; *)
(*
processing false...
processing a function...
- : int = 42
 *)

(* (a_function fun_non_strict) (a_unit ());; *)
(*
processing the unit value...
processing a function...
- : int = 42
 *)

(* yes! hence, what we observe is consistent with question 2! *)
(* we don't really know in advance whether the function we have to "apply" to our argument uses the argument or not! *)

(* hence, it is the case in OCaml as well that applying a non-strict function to any argument yields the same computation *)

(*

P.S. but not necessarily the same result, for example consider the function:
let fun_non_strict _ = Random.int 100;;
This function may yield different results every time it is applied to different arguments; however, it will carry out the same computation, i.e. yielding a random integer between 0 and 99 (inclusive)

reference:
https://delimited-continuation.github.io/YSC1212/2020-2021_Sem2/week-01_recap.html#a-plea-for-precision

*)

(* ********** *)

(*
        OCaml version 4.10.0

# #use "./midterm-project_underlying-determinism.ml";;
val show_bool : bool -> string = <fun>
val show_char : char -> string = <fun>
val show_string : string -> string = <fun>
val show_int : int -> string = <fun>
val show_unit : unit -> string = <fun>
val an_int : int -> int = <fun>
val an_int_v1 : int -> int = <fun>
val a_bool : bool -> bool = <fun>
val a_bool_v1 : bool -> bool = <fun>
val a_bool_v2 : bool -> bool = <fun>
val a_char : char -> char = <fun>
val a_string : string -> string = <fun>
val a_unit : unit -> unit = <fun>
val a_function : ('a -> 'b) -> 'a -> 'b = <fun>
val mul_3 : int -> int -> int -> int = <fun>
val fun_non_strict : 'a -> int = <fun>
val end_of_file : string = "midterm-project_underlying-determinism.ml"

 *)

(* end of midterm-project_underlying-determinism.ml *)
let end_of_file = "midterm-project_underlying-determinism.ml";;
