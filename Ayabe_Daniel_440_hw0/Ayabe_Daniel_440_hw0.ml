(*** IIT CS 440, Spring 2021 ***)
(*** Homework 0 ***)

exception ImplementMe

(** Lists *)

(* IMPORTANT: Do not remove any lines beginning *>*, like the following one.
 * These are used by our autograder and need to be there for you to get points. 
 *)
(*>* Task 4.1 *>*)

let rec stutter (n: int) (x: 'a) : 'a list =
  (* Remove the line "raise ImplementMe" and write your code for Task 4.1
   * here. Do not change the signature above. *)
  if (n <= 0) then [] else (x::(stutter (n - 1) x));;

(* A couple example unit tests. Write these for each function you implement.
 * Uncomment them once you've implemented stutter. *)

assert (stutter 0 5 = []);;
assert (stutter 2 5 = [5; 5]);;


(*>* Task 4.2 *>*)

let rec filter (f: 'a -> bool) (l: 'a list) : 'a list =
  match l with
  | [] -> []
  | a::x -> if f a then a::filter f x else filter f x
;;

assert(filter (fun x -> x > 2) [5; 3; 1; 2; 4] = [5; 3; 4]);;
assert(filter (fun x -> x > 5) [5; 3; 1; 2; 4] = []);;

(*>* Task 4.3 *>*)

let rec find (f: 'a -> bool) (l: 'a list) : 'a option =
  match l with
  | [] -> None
  | a::x -> if f a then Some a else find f x
;;

assert(find (fun x -> x > 2) [5; 3; 1; 2; 4] = Some 5);;
assert(find (fun x -> x < 3) [5; 3; 1; 2; 4] = Some 1);;
assert(find (fun x -> x > 5) [5; 3; 1; 2; 4] = None);;

(*>* Task 4.4 *>*)

let parens (l: char list) : bool =
  let rec parens1 (l1: char list)(l2: char list): bool =
      match l1 with
      |[] -> (match l2 with
      |[] -> true
      |_ -> false)  
      |a::x -> if a == '(' then parens1 x (a::l2)
        else if a == ')' then
        (match l2 with
        |[] -> false
        |h::t -> parens1 x t)
      else parens1 x l2
      in parens1 l []
;;
assert (parens [')'] = false);;
assert (parens ['('] = false);;

(*>* Task 4.5 *>*)

let rec column (x: 'a -> bool) (l: 'a list)(r: int) (c: int) : (int*int) option =
  match l with
  |[] -> None
  |h::t -> if x h then Some (r,c)
  else column x t r (c + 1);;

let rec row (x: 'a -> bool) (lines: 'a list list)(r: int) : (int*int) option =
  match lines with
  |[] -> None
  |h::t -> if column x h r 0 == None then row x t (r + 1) 
  else column x h r 0;;


let find2D (f: 'a -> bool) (lines: 'a list list) : (int * int) option =
  row f lines 0
;;


assert (find2D (fun _ -> true) [] = None);;
assert (find2D (fun _ -> true) [[]; []] = None);;
let file =
  [[1;2;3;4;5];
   [5;4;3;2;1];
   [10;9;8;7;6];
   [2;4;6;8;10;12]]
;;
assert (find2D ((=) 5) file = Some (0, 4));;
assert (find2D ((<) 5) file = Some (2, 0));;
assert (find2D ((<) 10) file = Some (3, 5));;
 
