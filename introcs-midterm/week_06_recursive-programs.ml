(* week_06_recursive-programs.ml *)
(* Introduction to Computer Science (YSC1212), Sem2, 2020-2021 *)
(* Aditya Singhania <adityasinghania@yale-nus.edu.sg> *)
(* Version of Fri 12 Mar 2021, made improvements based on prof's comments *)
(* was: *)
(* Version of Mon 1 Mar 2021, declared end_of_file *)
(* Version of Sun 28 Feb 2021 *)

(* ********** *)

(* nat_fold_right *)
let nat_fold_right zero_case succ_case n_given =
 (* nat_fold_right : 'a -> ('a -> 'a) -> int -> 'a *)
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 0
    then zero_case
    else let n' = n - 1
         in let ih = visit n'
            in succ_case ih    (* <-- succ_case takes one argument *)
  in visit n_given;;

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

let show_bool b =
 (* show_bool : bool -> string *)
  if b
  then "true"
  else "false";;

let show_int n =
 (* show_int : int -> string *)
  if n < 0
  then "(" ^ string_of_int n ^ ")"
  else string_of_int n;;

(* positive random test *)
let positive_random_test_for_unary_function_taking_an_integer_once r name candidate witness show_result =
  let n = Random.int r
  in let actual_result = candidate n
     and expected_result = witness n
     in if actual_result = expected_result
        then true
        else let () = Printf.printf (* error message *)
                      "testing %s failed for %d failed with result %s instead of %s\n"
                      name n (show_result actual_result) (show_result expected_result)
             in false;;

(* function to repeat positive random test *)
let repeat n_given test candidate =
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 0
    then true
    else test candidate && visit (n - 1)
  in visit n_given;;

(* ********** *)

(* exercise 03 *)

(* a unit test for our future factorial function *)
let test_fac candidate =
      (* the base case: *)
  let b0 = (candidate 0 = 1)
      (* some intuitive cases: *)
  and b1 = (candidate 1 = 1)
  and b2 = (candidate 2 = 2)
  and b3 = (candidate 3 = 6)
  and b4 = (candidate 4 = 24)
  and b5 = (candidate 5 = 120)
      (* instance of the induction step: *)
  and b6 = (let n = Random.int 20
            in candidate (succ n) = (succ n) * candidate n)
  (* etc. *)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6;;

(* factorial function using nat_parafold_right *)
let fac_npfr n =
  let () = assert (n >= 0) in
  nat_parafold_right 1 (fun n' ih -> (succ n') * ih) n;;

(* testing function with unit test *)
let () = assert (test_fac fac_npfr);;

(* factorial function using nat_fold_right *)
let fac_nfr n =
  let () = assert (n >= 0) in
    let (n, result) =
      nat_fold_right (0, 1) (fun (n', ih) -> ((succ n'), (succ n') * ih)) n
    in result;;

(* testing function with unit test *)
let () = assert (test_fac fac_nfr);;

(* over here, (n, result) represent the given n and the result of our calculation.

   we begin with (0, 1) because:
    0: we must iterate from 0 until the given n
    1: the factorial of 0 (0!), the base case, is 1

  the input function simply iterates ahead through "(succ n')",
  and we use "(succ n') * ih" to calculate the actual factorial,
  (similar to how we did in our implementation with parafold)

  hence, by using "(succ n')" to iterate, we get access to n' as well,
  which we must use to calculate our factorial!

 *)

(* hmmm. a language whose functions only accept only one argument
   where have we heard of this before?
   is anyone's stomach rumbling? Indian today maybe? *)

(* a more detailed rationale of why this works has been provided in exercise 07 *)
(* no really, they're the same function (we've proved this earlier!) *)

(* ********** *)

(* exercise 04 *)

(* a unit test for our future sumtorial function *)
let test_sumtorial candidate =
      (* the base case: *)
  let b0 = (candidate 0 = 0)
      (* some intuitive cases: *)
  and b1 = (candidate 1 = 1)
  and b2 = (candidate 2 = 3)
  and b3 = (candidate 3 = 6)
  and b4 = (candidate 4 = 10)
  and b5 = (candidate 5 = 15)
      (* instance of the induction step: *)
  and b6 = (let n = Random.int 20
            in candidate (succ n) = (succ n) + candidate n)
  (* etc. *)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6;;

(* recurisve function for sumtorial *)
let sumtorial_rec n =
  let () = assert (n >= 0) in
  nat_parafold_right 0 (fun n' ih -> (succ n') + ih) n;;

(* testing function with unit test *)
let () = assert (test_sumtorial sumtorial_rec);;

(* similar to exercise 03, using nat_fold_right *)
let sumtorial_rec_nfr n =
  let () = assert (n >= 0) in
  let (n, result) =
    nat_fold_right (0, 0) (fun (n', ih) -> ((succ n'), (succ n') + ih)) n
  in result;;

(* testing function with unit test *)
let () = assert (test_sumtorial sumtorial_rec_nfr);;

(* non-recursive function for sumtorial *)
let sumtorial_non_rec n =
  let () = assert (n >= 0) in
  n * (n + 1) / 2;; (* using Gauss's formula to calculate sum of n natural numbers *)

(* testing function with unit test *)
let () = assert (test_sumtorial sumtorial_non_rec);;

(* ********** *)

(* exercise 05 *)

(* a. a unit test for our future sum function *)
let test_sum candidate f =
      (* base case *)
  let b0 = (candidate f 0 = f 0)
      (* some intuitive cases *)
  and b1 = (candidate f 1 = f 0 + f 1)
  and b2 = (candidate f 2 = f 0 + f 1 + f 2)
      (* some instances of successive steps *)
  and b3 = (candidate f 3 = candidate f 2 + f 3)
  and b4 = (candidate f 4 = candidate f 3 + f 4)
      (* instance of the induction step: *)
  and b5 = (let n = Random.int 20
            in candidate f n + f (succ n) = candidate f (succ n))
  in b0 && b1 && b2 && b3 && b4 && b5;;

(* b. recursive function for sum *)
let sum f n_given =
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 0
    then f 0
    else let n' = n - 1
         in let ih = visit n'
            in f (succ n') + ih
  in visit n_given;;

(* c. testing function with unit test *)
let () = assert (test_sum sum succ);;

(* d. recursive function for sum using nat_parafold_right *)
let sum_npfr f n =
  nat_parafold_right (f 0) (fun n' ih -> f (succ n') + ih) n;;

(* testing function with unit test *)
let () = assert (test_sum sum_npfr (fun x -> 2 * x));;

(* similar to exercise 03, using nat_fold_right *)
let sum_nfr f n =
  let (n, result) =
    nat_fold_right (0, (f 0)) (fun (n', ih) -> (succ n', (f (succ n') + ih))) n
  in result;;

(* testing function with unit test *)
let () = assert (test_sum sum_nfr (fun x -> 2 * x));;

(* e. representing sumtorial using sum *)
let sumtorial_sum n =
  sum_nfr (fun x -> x) n;;

(* testing function with unit test *)
let () = assert (test_sumtorial sumtorial_sum);;

(* e. function to calculate sum of first n + 1 odd numbers *)

(* a function to make a number odd *)
let make_odd_number n =
 let () = assert (n >= 0)
 in 2 * n + 1;;

(* a unit test for our future oddsum function *)
let test_oddsum candidate =
      (* the base case: *)
  let b0 = (candidate 0 = 1)
      (* some intuitive cases: *)
  and b1 = (candidate 1 = 4)
  and b2 = (candidate 2 = 9)
  and b3 = (candidate 3 = 16)
  and b4 = (candidate 4 = 25)
  and b5 = (candidate 5 = 36)
      (* instance of the induction step: *)
  and b6 = (let n = Random.int 20
            in candidate (succ n) = (make_odd_number (succ n)) + candidate (n))
  (* etc. *)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6;;

(* using sum *)
let oddsum_sum_v0 n =
  sum (fun i -> 2 * i + 1) n;;

(* testing function with unit test *)
let () = assert (test_oddsum oddsum_sum_v0);;

(* more succinctly *)
let oddsum_sum_v1 n =
  sum make_odd_number n;;

(* testing function with unit test *)
let () = assert (test_oddsum oddsum_sum_v1);;

(* structually recursive *)
let oddsum_rec n_given =
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 0
    then 1
    else let n' = n - 1
         in let ih = visit n'
            in ih + (make_odd_number n)
  in visit n_given;;

(* testing function with unit test *)
let () = assert (test_oddsum oddsum_rec);;

(* using nat_parafold_right *)
let oddsum_npfr n =
  nat_parafold_right 1 (fun n' ih -> ih + (make_odd_number (succ n'))) n;;

(* testing function with unit test *)
let () = assert (test_oddsum oddsum_npfr);;

(* similar to exercise 03, using nat_fold_right *)
let oddsum_nfr n =
  let (n, result) =
    nat_fold_right (0, 1) (fun (n', ih) -> (succ n', (ih + (make_odd_number (succ n'))))) n
  in result;;

(* testing function with unit test *)
let () = assert (test_oddsum oddsum_nfr);;

(* constant time *)
let oddsum_const_v0 n =
  let () = assert (n >= 0) in
  (succ n) * (succ n);;

(* testing function with unit test *)
let () = assert (test_oddsum oddsum_const_v0);;

(* lesser calculations, more space *)
let oddsum_const_v1 n =
  let () = assert (n >= 0) in
    let n' = succ n in
      n' * n';;

(* testing function with unit test *)
let () = assert (test_oddsum oddsum_const_v1);;

(* we can use the oddsum_const_v1 function as a witness function to test
   our other implementations against! *)

let positive_random_test_oddsum_once r name candidate =
  positive_random_test_for_unary_function_taking_an_integer_once
    r name candidate oddsum_const_v1 show_int;;

let () = assert (repeat 100 (positive_random_test_oddsum_once 100 "oddsum_npfr") oddsum_npfr);;
let () = assert (repeat 100 (positive_random_test_oddsum_once 100 "oddsum_nfr") oddsum_nfr);;
let () = assert (repeat 100 (positive_random_test_oddsum_once 100 "oddsum_sum_v1") oddsum_sum_v1);;
let () = assert (repeat 100 (positive_random_test_oddsum_once 100 "oddsum_rec") oddsum_rec);;

(* let () = assert (repeat 100 (positive_random_test_oddsum_once 100 "fake_function") (fun i -> i));; *)
(* fails test, with output:
  testing fake_function failed for 9 failed with result 9 instead of 100 *)

(* ********** *)

(* exercise 06 *)

let nat_fold_right_using_parafold zero_case succ_case n =
  nat_parafold_right zero_case (fun n' ih -> succ_case ih) n;;

(* a function to test our new nat_fold_right function *)
let test_nat_fold_right zero_case succ_case =
  let n = Random.int 20
  in nat_fold_right zero_case succ_case n = nat_fold_right_using_parafold zero_case succ_case n;;

(* testing function with testing function *)
let () = assert (test_nat_fold_right 0 (fun ih -> ih + 2));;

(* ********** *)

(* exercise 07 *)

let nat_parafold_right_using_fold zero_case succ_case n =
  let (n, result) =
    nat_fold_right (0, zero_case) (fun (n', ih) -> ((succ n'), succ_case n' ih)) n
  in result;;

(* the only difference between nat_fold_right and nat_parafold_right is that
   nat_parafold_right's function that calculates the successive case "succ_case"
   takes, ahem, "2" arguments whereas nat_fold_right's "succ_case" only accepts
   one argument.

   but wait a second [name redacted], doesn't any function in OCaml accept only one argument?
   YES! and there comes in the "difference" of the curried and the uncurried function.

   what a "function with multiple arguments" actually is, is simply a function
   that takes a function that takes a type, an integer in this case.

   and that is what we call the curried function!

   there is another way we can pass in "multiple arguments" without implementing
   a curried function: we pass tuples!

   tuples can contain multiple values that are of any type. and that is exactly
   what we have done above, when we pass a pair of (int * int), that is
   we define the succ_case function of nat_fold_right such that it accepts a pair.

   and this is what we call the uncurried function!

   and finallyy... as we saw in question 04 of the midterm project about the
   underlying determinism of OCaml, the curried function and the uncurried
   function are equivalent, that is: they give rise to the same evaluation/compuation!

   and since the only difference between nat_fold_right here and nat_parafold_right
   here is the use of an uncurried function and the use of a curried function,
   nat_fold_right and nat_parafold_right are equivalent!

   huzzah!

   a few final notes:
    - the pair (n, result) is named so because:
      - n: the final value of this will be n as we would have iterated from 0 up till n
      - result: the final value of this will be the result depending on the succ_case function

    - we pass in (0, zero_case) to nat_para_fold_right because:
      - 0: we begin with 0 and iterate up till n
      - zero_case: this is the base case of the what we aim to achieve.
                   this is what the succ_case function will start building upon!

*)

(* "hmmm. a language whose functions only accept only one argument
   where have we heard of this before?
   is anyone's stomach rumbling? Indian today maybe?" ~ [name redacted], 2021 *)

(* a function to test our new nat_parafold_right function *)
let test_nat_parafold_right zero_case succ_case =
  let n = Random.int 20
  in nat_parafold_right zero_case succ_case n = nat_parafold_right_using_fold zero_case succ_case n;;

(* testing function with testing function *)
let () = assert (test_nat_parafold_right 1 (fun n' ih -> (succ n') * ih));;

(* ********** *)

(* exercise 10 *)

(* a unit test for our future ternary predicates *)
let test_ternary candidate n =
  (* pass n as 0 for ternary, 1 for post-ternary, 2 for pre-ternary *)
      (* the base case: *)
  let b0 = (candidate 0 = ((0 mod 3) = n))
  and b1 = (candidate 1 = ((1 mod 3) = n))
  and b2 = (candidate 2 = ((2 mod 3) = n))
  and b3 = (candidate 3 = ((3 mod 3) = n))
  and b4 = (candidate 4 = ((4 mod 3) = n))
  and b5 = (candidate 5 = ((5 mod 3) = n))
      (* instance of a truth that should hold *)
  and b6 = (let i = Random.int 20
            in candidate i = candidate (i + 3))
  (* etc. *)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6;;

let ternary_v0 n_given =
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 0
    then true
    else if n < 3
    then false
    else let n' = n - 3
         in let ih = visit n'
            in ih
  in visit n_given;;

(* testing function with unit test *)
let () = assert (test_ternary ternary_v0 0);;

let pre_ternary_v0 n_given =
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 2
    then true
    else if n < 3
    then false
    else let n' = n - 3
         in let ih = visit n'
            in ih
  in visit n_given;;

(* testing function with unit test *)
let () = assert (test_ternary pre_ternary_v0 2);;

let post_ternary_v0 n_given =
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 1
    then true
    else if n < 3
    then false
    else let n' = n - 3
         in let ih = visit n'
            in ih
  in visit n_given;;

(* testing function with unit test *)
let () = assert (test_ternary post_ternary_v0 1);;

(* alternatively, to kill 3 birds with 1 stone *)
let ternary_predicates base_case n_given =
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = base_case
    then true
    else if n < 3
    then false
    else let n' = n - 3
         in let ih = visit n'
            in ih
  in visit n_given;;

let ternary_v1 n =
  ternary_predicates 0 n;;

(* testing function with unit test *)
let () = assert (test_ternary ternary_v1 0);;

let pre_ternary_v1 n =
  ternary_predicates 2 n;;

(* testing function with unit test *)
let () = assert (test_ternary pre_ternary_v1 2);;

let post_ternary_v1 n =
  ternary_predicates 1 n;;

(* testing function with unit test *)
let () = assert (test_ternary post_ternary_v1 1);;

(* however, the above implementation does not use the inductive sepcification of ternary!
therefore, the usage of "ih" seems to be a misnomer.

hence, below is a function that uses the inductive specification *)

let ternary_v2 n_given =
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 0
    then true
    else let n' = pred n
         in if n' = 0
            then false
            else let n'' = pred n'
                 in if n'' = 0
                    then false
                    else let n''' = pred n''
                         in visit n'''
  in visit n_given;;

(* testing function with unit test *)
let () = assert (test_ternary ternary_v2 0);;

let pre_ternary_v2 n_given =
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 0
    then false
    else let n' = pred n
         in if n' = 0
            then false
            else let n'' = pred n'
                 in if n'' = 0
                    then true
                    else let n''' = pred n''
                         in visit n'''
  in visit n_given;;

(* testing function with unit test *)
let () = assert (test_ternary pre_ternary_v2 2);;

let post_ternary_v2 n_given =
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 0
    then false
    else let n' = pred n
         in if n' = 0
            then true
            else let n'' = pred n'
                 in if n'' = 0
                    then false
                    else let n''' = pred n''
                         in visit n'''
  in visit n_given;;

(* testing function with unit test *)
let () = assert (test_ternary post_ternary_v2 1);;

(* a witness function for the ternary predicate *)
let ternary_witness n =
  n mod 3 = 0;;

(* we can now use this witness function to test
  our other implementations against! *)
let positive_random_test_ternary_once r name candidate =
  positive_random_test_for_unary_function_taking_an_integer_once
    r name candidate ternary_witness show_bool;;

let () = assert (repeat 100 (positive_random_test_ternary_once 100 "ternary_v2") ternary_v2);;

(* similarly, a witness function for pre-ternary and post-ternary *)
let pre_ternary_witness n =
  n mod 3 = 2;;

let post_ternary_witness n =
  n mod 3 = 1;;

(* we can now use these witness functions to test
  our other implementations! *)
let positive_random_test_preternary_once r name candidate =
  positive_random_test_for_unary_function_taking_an_integer_once
    r name candidate pre_ternary_witness show_bool;;

let positive_random_test_postternary_once r name candidate =
  positive_random_test_for_unary_function_taking_an_integer_once
    r name candidate post_ternary_witness show_bool;;

let () = assert (repeat 100 (positive_random_test_preternary_once 100 "pre_ternary_v2") pre_ternary_v2);;
let () = assert (repeat 100 (positive_random_test_postternary_once 100 "post_ternary_v2") post_ternary_v2);;

(* we can also recursively define quarternary *)
(* quarternary *)
let quarternary n_given =
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 0
    then true
    else let n' = pred n
         in if n' = 0
            then false
            else let n'' = pred n'
                 in if n'' = 0
                    then false
                    else let n''' = pred n''
                         in if n''' = 0
                            then false
                            else let n'''' = pred n'''
                                 in visit n''''
  in visit n_given;;

let mid_quarternary n_given =
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 0
    then false
    else let n' = pred n
         in if n' = 0
            then false
            else let n'' = pred n'
                 in if n'' = 0
                    then true
                    else let n''' = pred n''
                         in if n''' = 0
                            then false
                            else let n'''' = pred n'''
                                 in visit n''''
  in visit n_given;;

let pre_quarternary n_given =
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 0
    then false
    else let n' = pred n
         in if n' = 0
            then false
            else let n'' = pred n'
                 in if n'' = 0
                    then true
                    else let n''' = pred n''
                         in if n''' = 0
                            then true
                            else let n'''' = pred n'''
                                 in visit n''''
  in visit n_given;;

let post_quarternary n_given =
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 0
    then false
    else let n' = pred n
         in if n' = 0
            then true
            else let n'' = pred n'
                 in if n'' = 0
                    then true
                    else let n''' = pred n''
                         in if n''' = 0
                            then false
                            else let n'''' = pred n'''
                                 in visit n''''
  in visit n_given;;

(* we can also define ternary recursively using nat_fold_right *)
(* function to cycle triple *)
let tricycle (b1, b2, b3) =
  (b3, b1, b2);;

(* through nat_fold_right *)
let all_ternary_v3 n =
  nat_fold_right (true, false, false) tricycle n;;

let ternary_v3 n =
  let (b1, b2, b3) = all_ternary_v3 n
  in b1;;

let pre_ternary_v3 n =
  let (b1, b2, b3) = all_ternary_v3 n
  in b3;;

let post_ternary_v3 n =
  let (b1, b2, b3) = all_ternary_v3 n
  in b2;;

(* all our functions can now be be tested with a single unit test *)
let test_all_ternary candidate =
      (* the base case: *)
  let b0 = (candidate 0 = (true, false, false)) (* ternary *)
  and b1 = (candidate 1 = (false, true, false)) (* post-ternary *)
  and b2 = (candidate 2 = (false, false, true)) (* pre-ternary *)
  and b3 = (candidate 3 = (true, false, false)) (* ternary *)
  and b4 = (candidate 4 = (false, true, false)) (* post-ternary *)
  and b5 = (candidate 5 = (false, false, true)) (* pre-ternary *)
      (* instance of the induction step: *)
  and b6 = (let i = Random.int 20
            in candidate i = candidate (i + 3))
  (* etc. *)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6;;

(* testing function(s) with unit test *)
let () = assert (test_all_ternary all_ternary_v3);;

(* we can also run our positive random test on these functions *)
let () = assert (repeat 100 (positive_random_test_ternary_once 100 "ternary_v3") ternary_v3);;
let () = assert (repeat 100 (positive_random_test_preternary_once 100 "pre_ternary_v3") pre_ternary_v3);;
let () = assert (repeat 100 (positive_random_test_postternary_once 100 "post_ternary_v3") post_ternary_v3);;

(* we can also implement a similar function for quarternary numbers *)

(* function to cycle quadruple *)
let quadcycle (b1, b2, b3, b4) =
  (b4, b1, b2, b3);;

let all_quarternary_v1 n =
  nat_fold_right (true, false, false, false) quadcycle n;;

let quarternary_v1 n =
  let (b1, b2, b3, b4) = all_quarternary_v1 n
  in b1;;

let mid_quarternary_v1 n =
  let (b1, b2, b3, b4) = all_quarternary_v1 n
  in b3;;

let pre_quarternary_v1 n =
  let (b1, b2, b3, b4) = all_quarternary_v1 n
  in b4;;

let post_quarternary_v1 n =
  let (b1, b2, b3, b4) = all_quarternary_v1 n
  in b2;;

(* similarly, quinternary *)

(* function to cycle quintuple *)
let quintcycle (b1, b2, b3, b4, b5) =
  (b5, b1, b2, b3, b4);;

let all_quinternary n =
  nat_fold_right (true, false, false, false, false) quintcycle n;;

(* ternary predicate mutually recursively *)
let rec ternary_v4 n =
  if n = 0
  then true
  else let n' = n - 1
       in pre_ternary_v4 n'
and pre_ternary_v4 n =
  if n = 0
  then false
  else let n' = n - 1
       in post_ternary_v4 n'
and post_ternary_v4 n =
  if n = 0
  then false
  else let n' = n - 1
       in ternary_v4 n';;

(* testing function with unit test *)
let () = assert (test_ternary ternary_v4 0);;

(* testing function with unit test *)
let () = assert (test_ternary pre_ternary_v4 2);;

(* testing function with unit test *)
let () = assert (test_ternary post_ternary_v4 1);;

(* we can also run our positive random test on these functions *)
let () = assert (repeat 100 (positive_random_test_ternary_once 100 "ternary_v4") ternary_v4);;
let () = assert (repeat 100 (positive_random_test_preternary_once 100 "pre_ternary_v4") pre_ternary_v4);;
let () = assert (repeat 100 (positive_random_test_postternary_once 100 "post_ternary_v4") post_ternary_v4);;

(* and, of course, the qurternary predicate as well, mutually recursively *)
let rec quarternary_v2 n =
  if n = 0
  then true
  else let n' = n - 1
       in pre_quarternary_v2 n'
and pre_quarternary_v2 n =
  if n = 0
  then false
  else let n' = n - 1
       in post_quarternary_v2 n'
and mid_quaternary_v2 n =
  if n = 0
  then false
  else let n' = n - 1
       in post_quarternary_v2 n'
and post_quarternary_v2 n =
  if n = 0
  then false
  else let n' = n - 1
       in quarternary_v2 n';;

(* ********** *)

(*
        OCaml version 4.10.0

# #use "./week_06_recursive-programs.ml";;
val nat_fold_right : 'a -> ('a -> 'a) -> int -> 'a = <fun>
val nat_parafold_right : 'a -> (int -> 'a -> 'a) -> int -> 'a = <fun>
val show_bool : bool -> string = <fun>
val show_int : int -> string = <fun>
val positive_random_test_for_unary_function_taking_an_integer_once :
  int -> string -> (int -> 'a) -> (int -> 'a) -> ('a -> string) -> bool =
  <fun>
val repeat : int -> ('a -> bool) -> 'a -> bool = <fun>
val test_fac : (int -> int) -> bool = <fun>
val fac_npfr : int -> int = <fun>
val fac_nfr : int -> int = <fun>
val test_sumtorial : (int -> int) -> bool = <fun>
val sumtorial_rec : int -> int = <fun>
val sumtorial_rec_nfr : int -> int = <fun>
val sumtorial_non_rec : int -> int = <fun>
val test_sum : ((int -> int) -> int -> int) -> (int -> int) -> bool = <fun>
val sum : (int -> int) -> int -> int = <fun>
val sum_npfr : (int -> int) -> int -> int = <fun>
val sum_nfr : (int -> int) -> int -> int = <fun>
val sumtorial_sum : int -> int = <fun>
val make_odd_number : int -> int = <fun>
val test_oddsum : (int -> int) -> bool = <fun>
val oddsum_sum_v0 : int -> int = <fun>
val oddsum_sum_v1 : int -> int = <fun>
val oddsum_rec : int -> int = <fun>
val oddsum_npfr : int -> int = <fun>
val oddsum_nfr : int -> int = <fun>
val oddsum_const_v0 : int -> int = <fun>
val oddsum_const_v1 : int -> int = <fun>
val positive_random_test_oddsum_once : int -> string -> (int -> int) -> bool =
  <fun>
val nat_fold_right_using_parafold : 'a -> ('a -> 'a) -> int -> 'a = <fun>
val test_nat_fold_right : 'a -> ('a -> 'a) -> bool = <fun>
val nat_parafold_right_using_fold : 'a -> (int -> 'a -> 'a) -> int -> 'a =
  <fun>
val test_nat_parafold_right : 'a -> (int -> 'a -> 'a) -> bool = <fun>
val test_ternary : (int -> bool) -> int -> bool = <fun>
val ternary_v0 : int -> bool = <fun>
val pre_ternary_v0 : int -> bool = <fun>
val post_ternary_v0 : int -> bool = <fun>
val ternary_predicates : int -> int -> bool = <fun>
val ternary_v1 : int -> bool = <fun>
val pre_ternary_v1 : int -> bool = <fun>
val post_ternary_v1 : int -> bool = <fun>
val ternary_v2 : int -> bool = <fun>
val pre_ternary_v2 : int -> bool = <fun>
val post_ternary_v2 : int -> bool = <fun>
val ternary_witness : int -> bool = <fun>
val positive_random_test_ternary_once :
  int -> string -> (int -> bool) -> bool = <fun>
val pre_ternary_witness : int -> bool = <fun>
val post_ternary_witness : int -> bool = <fun>
val positive_random_test_preternary_once :
  int -> string -> (int -> bool) -> bool = <fun>
val positive_random_test_postternary_once :
  int -> string -> (int -> bool) -> bool = <fun>
val quarternary : int -> bool = <fun>
val mid_quarternary : int -> bool = <fun>
val pre_quarternary : int -> bool = <fun>
val post_quarternary : int -> bool = <fun>
val tricycle : 'a * 'b * 'c -> 'c * 'a * 'b = <fun>
val all_ternary_v3 : int -> bool * bool * bool = <fun>
val ternary_v3 : int -> bool = <fun>
val pre_ternary_v3 : int -> bool = <fun>
val post_ternary_v3 : int -> bool = <fun>
val test_all_ternary : (int -> bool * bool * bool) -> bool = <fun>
val quadcycle : 'a * 'b * 'c * 'd -> 'd * 'a * 'b * 'c = <fun>
val all_quarternary_v1 : int -> bool * bool * bool * bool = <fun>
val quarternary_v1 : int -> bool = <fun>
val mid_quarternary_v1 : int -> bool = <fun>
val pre_quarternary_v1 : int -> bool = <fun>
val post_quarternary_v1 : int -> bool = <fun>
val quintcycle : 'a * 'b * 'c * 'd * 'e -> 'e * 'a * 'b * 'c * 'd = <fun>
val all_quinternary : int -> bool * bool * bool * bool * bool = <fun>
val ternary_v4 : int -> bool = <fun>
val pre_ternary_v4 : int -> bool = <fun>
val post_ternary_v4 : int -> bool = <fun>
val quarternary_v2 : int -> bool = <fun>
val pre_quarternary_v2 : int -> bool = <fun>
val mid_quaternary_v2 : int -> bool = <fun>
val post_quarternary_v2 : int -> bool = <fun>
val end_of_file : string = "week_06_recursive-programs.ml"

 *)

(* end of week_06_recursive-programs.ml *)
let end_of_file = "week_06_recursive-programs.ml";;
