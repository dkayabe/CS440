(*** IIT CS 440, Spring 2021 ***)
(*** Homework 2 ***)

(** NFA Simulator *)

(* Same deal here with ignoring some of these lines. *)
module N : ParseNFA.NFA_Types =
  struct
    
    type symbol = char
    type state = int
    (* Type is the same as before except now a transition has a
     * symbol option:
     * (m, Some s, n) is a transition from m to n on seeing symbol s
     * (m, None, n) is an epsilon-transition from m to n
     *)
    type nfa = { states : int;
                 accept : state list;
                 alpha  : symbol list;
                 trans  : (state * symbol option * state) list }

  end

open N
module Parser = ParseNFA.Parser(N)
  
exception IllformedInput of string
exception ImplementMe

(** Some useful helper functions **)

(* Sorts a list and removed duplicates; after you call norm on a list,
 * you can treat it like a set, that is, if (norm l1) = (norm l2), then l1
 * and l2 are equal as sets (have the same elements, regardless of order and
 * multiples)
 *)
let norm l =
  let rec dedup l =
    match l with
    | [] -> []
    | x::t -> x::(dedup (List.filter (fun x' -> x' <> x) t))
  in
  List.sort (fun a b -> a - b) (dedup l)
;;

(* Turns a list of states into a human-readable string. Useful for debugging *)
let string_of_states states =
  Printf.sprintf "{%s}"
    (String.concat ", " (List.map string_of_int states))
;;

(** Your code starts here **)

(*>* Problem 3.1 *>*)

(* Returns a list of states you can be in on seeing symbol symb (which is
 * either Some s for a symbol s, or None for epsilon) in state
 * "state". This is a list and not just a single state because this is
 * an NFA. Note that if symb is None, this should just be the states
 * reachable with one epsilon-transition. *)
let transitions (trans_list : (state * symbol option * state) list)
      (symb: symbol option) (state: state) : state list =
  let rec transitions' l symb st statel =
    match l with
    | (st1, cur_symb, st2)::t when (cur_symb = symb) && (st1 = st) -> transitions' t symb st (st2::statel)
    | _::t -> transitions' t symb st statel
    | [] -> statel in
  transitions' trans_list symb state [];;

(*>* Problem 3.2 *>*)

(* Returns the list of states accessible by (possibly multiple) epsilon
 * transitions from "states" *)
let rec eps_clos (nfa: nfa) (states: state list) : state list =
  let rec dfs (states: state list) (visited:state list) = match states with
    | current::x when ((norm (current::visited)) = visited) -> dfs x visited
    | current::x -> let routes = transitions nfa.trans None current in
        let routes1 = dfs (norm routes) (current::visited) in
          dfs x (norm routes1)
    | [] -> visited in
  norm (dfs states []);;

(*>* Problem 3.3 *>*)

(* Returns true if nfa accepts input: "states" is the list of states we
 * might be in currently *)
let rec nfa_sim (nfa: nfa) (states: state list) (input: symbol list) : bool =
  let rec list_contains l elem = match l with
    | x::y when (x = elem) -> true
    | _::y -> list_contains y elem
    | [] -> false
  in let rec states' = eps_clos nfa states
  in let rec check_states states accepting_states = match states with
    | st::y when (list_contains accepting_states st) -> true
    | st::y -> check_states y accepting_states
    | [] -> false
  in let rec all_transitions symb states l = match states with
    | st1::t_ -> let routes = transitions nfa.trans (Some symb) st1 in
            all_transitions symb t_ (routes@l)
    | [] -> l
  in
    match input with
    | symb::y -> let new_states = all_transitions symb states' [] in
              nfa_sim nfa new_states y
    | [] -> (check_states states' nfa.accept)
;;
