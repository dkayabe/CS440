(*** IIT CS 440, Spring 2021 ***)
(*** Homework 2 ***)

(** DFA Simulator *)

(* Ignore these next two lines... *)
module D : ParseDFA.DFA_Types =
  struct

    (* These are the type definitions for DFAs *)
    type symbol = char
    type state = int
    type dfa = { states : int; (* Number of states *)
                 (* States are numbered 0 through states
                  * State 0 is the start state *)
                 accept : state list; (* List of accept states *)
                 alpha  : symbol list; (* Alphabet of the DFA *)
                 trans  : (state * symbol * state) list
                   (* List of transitions in the form
                    * (from state, transition symbol, to state) *)
               }

                 (* Ignore the next few lines too. *)
  end

open D
module Parser = ParseDFA.Parser(D)

(* OK, stop ignoring. *)

exception ImplementMe
exception IllformedInput of string

(*>* Problem 2.1 *>*)

(* Returns the new state after seeing symbol symb in state "state".
 * trans_list is the list of transitions for the DFA 
 * Raises IllformedInput with an informative message if the state and
 * symbol aren't found in the transition list *)
let rec transition (trans_list : (state * symbol * state) list)
          (symb : symbol) (state: state) : state =
  match trans_list with 
  | (s1, symb1, s2)::x when (s1 = state) && (symb = symb1) -> s2  
  | _::x -> transition x symb state 
  | [] -> raise (IllformedInput
    (Printf.sprintf
    "State, symbol pair (%d, %c) not in transitions"
    state symb));;

(*>* Problem 2.2 *>*)

(* Return true if dfa accepts input starting from state, false otherwise
 * Raises IllformedInput with an informative message if a symbol in the
 * input isn't in the alphabet. *)
let rec dfa_sim (dfa: dfa) (state: state) (input: symbol list) : bool =
  let rec list_contains l elem = match l with 
    | x::y when (x = elem) -> true
    | _::y -> list_contains y elem
    | [] -> false in
      match input with 
      | sym::y -> let new_state = transition dfa.trans sym state in
          dfa_sim dfa new_state y
      | [] when (list_contains dfa.accept state) -> true
      | [] -> false;;


