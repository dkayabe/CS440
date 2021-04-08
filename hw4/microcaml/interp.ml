open Types

exception TypeError of string
exception UnboundVariable of string
exception ImplementMe

(*>* Problem 3.3 *>*)
(* [v/x]v' *)
let rec sub_val (v: value) (x: string) (v': value) =
  match v' with
  | Const (const) -> v'
  | fun (var, exp) -> if (var = x) then fun (var, exp) else fun (var, (sub_exp v x exp))
  | Op (op) -> v'
  | Absfun (value) -> v'
  | _ -> v'
(* [v/x]e *)
and sub_exp (v: value) (x: string) (e: exp)  =
match e with 
| Var (var)             -> if (var = x) then Value (v) else Var (var)
| Value (value)         -> Value (sub_val v x value)
| Let (var, exp1, exp2) -> if (var = x) then Let (var, (sub_exp v x exp1), exp2) else Let (var, (sub_exp v x exp1), (sub_exp v x exp2))
| If1 (exp1, exp2, exp3) -> If1 ((sub_exp v x exp1), (sub_exp v x exp2), (sub_exp v x exp3))
| App (exp1, exp2)      -> App ((sub_exp v x exp1), (sub_exp v x exp2))

let bool_of b = if b then True else False

let denote_op (o: op) : value -> value -> value =
  match o with
  | Plus | Minus | Times | Div ->
     (fun x y ->
       match (o, x, y) with
       | (Plus, Const (Int n1), Const (Int n2)) -> Const (Int (n1 + n2))
       | (Minus, Const (Int n1), Const (Int n2)) -> Const (Int (n1 - n2))
       | (Times, Const (Int n1), Const (Int n2)) -> Const (Int (n1 * n2))
       | (Div, Const (Int n1), Const (Int n2)) -> Const (Int (n1 / n2))
       | _ -> raise (TypeError "type mismatch"))
  | Lt | Gt | Le | Ge | Eq ->
     (fun x y ->
       match (o, x, y) with
       | (Lt, Const (Int n1), Const (Int n2)) -> Const (bool_of (n1 < n2))
       | (Gt, Const (Int n1), Const (Int n2)) -> Const (bool_of (n1 > n2))
       | (Le, Const (Int n1), Const (Int n2)) -> Const (bool_of (n1 <= n2))
       | (Ge, Const (Int n1), Const (Int n2)) -> Const (bool_of (n1 >= n2))
       | (Eq, Const (Int n1), Const (Int n2)) -> Const (bool_of (n1 = n2))
       | _ -> raise (TypeError "type mismatch"))
  | And | Or ->
     (fun x y ->
       match (o, x, y) with
       | (And, Const True, Const True) -> Const True
       | (And, _, _) -> Const False
       | (Or, Const False, Const False) -> Const False
       | (Or, _, _) -> Const True
       | _ -> raise (TypeError "type mismatch"))

(*>* Problem 3.4 *>*)
let rec eval_exp (e: exp) : value =
  match e with
  | Var x -> raise (UnboundVariable "unbound variable")
  | Value v -> v
  | If (e1, e2, e3) -> if (eval_exp e1 = Const True) then (eval_exp e2) else (eval_exp e3)
  | Let (x, e1, e2) -> eval_exp (sub_exp (eval_exp e1) x e2)
  | App (e1, e2) ->
     let v1 = eval_exp e2 in
     (match eval_exp e1 with
      | Fun (x, e) -> eval_exp (sub_exp v2 x e)
      | Op o -> Absfun ((denote_op o) v1)
      | Absfun f -> f v1
      | _ -> raise (TypeError "expected function"))
                
