open Tac
exception ImplementMe

(*>* Problem 1.1 *>*)
let rec compile_intexpr (e: intexpr) : code * addr =
  raise ImplementMe

(*>* Problem 1.2 *>*)
let rec compile_boolexpr (e: boolexpr) (t: label) (f: label) : code =
  match e with
  | Relop (Lt, e1, e2) -> raise ImplementMe
  | Relop (Gt, e1, e2) -> raise ImplementMe
  | Relop (Eq, e1, e2) -> raise ImplementMe
  | Relop (Le, e1, e2) -> compile_boolexpr (Relop (Gt, e1, e2)) f t
  | Relop (Ge, e1, e2) -> compile_boolexpr (Relop (Lt, e1, e2)) f t
  | Relop (Ne, e1, e2) -> compile_boolexpr (Relop (Eq, e1, e2)) f t
  | And (e1, e2) -> raise ImplementMe
  | Or (e1, e2) -> raise ImplementMe
  | Not e1 -> raise ImplementMe

(*>* Problem 1.3 *>*)
let rec compile_stmt (s: stmt) : code =
  match s with
  | Assign (x, e) -> raise ImplementMe
  | If (e, ifb, None) -> raise ImplementMe
  | If (e, ifb, Some elseb) -> raise ImplementMe
  | While (e, s) -> raise ImplementMe
  | Return e -> raise ImplementMe
  | Block b -> List.concat (List.map compile_stmt b)
