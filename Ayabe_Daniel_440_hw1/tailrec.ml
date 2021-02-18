(*** IIT CS 440, Spring 2021 ***)
(*** Homework 1 ***)

exception ImplementMe
;;

(** Tail Recursion **)

(*>* Problem 1.1 *>*)
let rec fact (n: int) : int =
  if n == 0 then
    1
  else n * fact(n - 1)
;;

(**Problem 1.1 Test Case**)
let _ = Printf.printf "Factorial of 6 is: %d\n" (fact 6);;


(*>* Problem 1.2 *>*)
let rec split (l: 'a list) : 'a list * 'a list =
  match l with
  | x::y::tail ->
      let a, b = split tail in
      x::a, y::b
  | x::[] -> [x], []
  | [] -> [], []
;;

(**Problem 1.2 Test Case**)
let m, n = split [1;2;3;4;5];;
open Printf
let p1 = Printf.printf("First List: ")
let () = List.iter (printf "%d,") m
let p2 = Printf.printf("\nSecond List: ")
let () = List.iter (printf "%d,") n

(*>* Problem 1.3 *>*)
let foldr (f: 'a -> 'b -> 'b) (l: 'a list) (u: 'b)  =
  raise ImplementMe
;;
