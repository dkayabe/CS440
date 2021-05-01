open Tac
open Env
open Util

(* Apply an optimization "opt" repeatedly until it stops making the code
 * shorter. You can use this in your optimization if you want. *)
let rec fix (opt: code -> code) (c: code) =
  let c' = opt c in
  if List.length c' = List.length c then
    c'
  else
    fix opt c'

(*>* Problem 2.3 *>*)
      
(* Optimize code. You can define other helper functions if you want, just put
them above this function. *)
let rec optimize (c: code) = c
