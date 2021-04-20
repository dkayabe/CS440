open Types
open Env

exception TypeError
exception UnboundVariable of string
   
let eval_op (o: infixop) (v1: value) (v2: value) : value =
  VConst (
      match (o, v1, v2) with
      | (Plus, VConst (Num n1), VConst (Num n2)) -> Num (n1 + n2)
      | (Minus, VConst (Num n1), VConst (Num n2)) -> Num (n1 - n2)
      | (Times, VConst (Num n1), VConst (Num n2)) -> Num (n1 * n2)
      | (Div, VConst (Num n1), VConst (Num n2)) -> Num (n1 / n2)
      | (Lt, VConst (Num n1), VConst (Num n2)) -> Bool (n1 < n2)
      | (Le, VConst (Num n1), VConst (Num n2)) -> Bool (n1 <= n2)
      | (Gt, VConst (Num n1), VConst (Num n2)) -> Bool (n1 > n2)
      | (Ge, VConst (Num n1), VConst (Num n2)) -> Bool (n1 >= n2)
      | (Eq, VConst (Num n1), VConst (Num n2)) -> Bool (n1 = n2)
      | (Ne, VConst (Num n1), VConst (Num n2)) -> Bool (n1 <> n2)
      | (And, VConst (Bool b1), VConst (Bool b2)) -> Bool (b1 && b2)
      | (Or, VConst (Bool b1), VConst (Bool b2)) -> Bool (b1 || b2)
      | (Concat, VConst (String s1), VConst (String s2)) -> String (s1 ^ s2)
      | _ -> raise TypeError)

let rec unannot_expr (e: expr) =
  match e with
  | EVar _ -> e
  | EConst v -> e
  | EInfixop (o, e1, e2) -> EInfixop (o, unannot_expr e1, unannot_expr e2)
  | EFun (x, e) -> EFun (x, unannot_expr e)
  | EIf (e1, e2, e3) -> EIf (unannot_expr e1, unannot_expr e2, unannot_expr e3)
  | ELet (x, _, e1, e2) ->
     ELet (x, None, unannot_expr e1, unannot_expr e2)
  | ELetFun (is_rec, f, x, _, _, e1, e2) ->
     ELetFun (is_rec, f, x, None, None, unannot_expr e1, unannot_expr e2)
  | ELetPair (x, y, e1, e2) ->
     ELetPair (x, y, unannot_expr e1, unannot_expr e2)
  | EApp (e1, e2) -> EApp (unannot_expr e1, unannot_expr e2)
  | EMatchList (e1, e2, h, t, e3) ->
     EMatchList (unannot_expr e1, unannot_expr e2, h, t, unannot_expr e3)
  | EPair (e1, e2) -> EPair (unannot_expr e1, unannot_expr e2)
  | ECons (e1, e2) -> ECons (unannot_expr e1, unannot_expr e2)
  | EAnnot (e, _) -> unannot_expr e
let rec unannot_value (v: value) =
  match v with
  | VConst _ -> v
  | VPair (v1, v2) -> VPair (unannot_value v1, unannot_value v2)
  | VCons (v1, v2) -> VCons (unannot_value v1, unannot_value v2)
  | VClos (x, e, env) -> VClos (x, unannot_expr e, env)
  | VAnnot (v, _) -> unannot_value v

let rec interp_expr (e: expr) (env: env) : value =
  match e with
  | EVar x ->
     (match env_find env x with
      | Some v -> v
      | None -> raise (UnboundVariable x))
  | EConst c -> VConst c
  | EInfixop (o, e1, e2) ->
     let v1 = interp_expr e1 env in
     let v2 = interp_expr e2 env in
     eval_op o v1 v2
  | EFun (x, e) -> VClos (x, e, env)
  | EIf (e1, e2, e3) ->
     let b = interp_expr e1 env in
     (match b with
      | VConst (Bool true) -> interp_expr e2 env
      | VConst (Bool false) -> interp_expr e3 env
      | _ -> raise TypeError
     )
  | ELet (x, _, e1, e2) ->
     let v = interp_expr e1 env in
     let env' = env_add env x v in
     interp_expr e2 env'
  | ELetFun (is_rec, f, x, _, _, e1, e2) ->
     let cl =
       if is_rec then env_with_me env f (fun env -> VClos (x, e1, env))
       else VClos (x, e1, env)
     in
     let env' = env_add env f cl in
     interp_expr e2 env'
  | ELetPair (x, y, e1, e2) ->
     (match interp_expr e1 env with
      | VPair (v1, v2) ->
         let env' = env_add (env_add env x v1) y v2 in
         interp_expr e2 env'
      | _ -> raise TypeError)
  | EApp (e1, e2) ->
     let v2 = interp_expr e2 env in
     (match interp_expr e1 env with
      | VClos (x, e, env') -> interp_expr e (env_add (env_union env env') x v2)
      | _ -> raise TypeError)
  | EMatchList (e1, e2, h, t, e3) ->
     (match interp_expr e1 env with
      | VConst Nil -> interp_expr e2 env
      | VCons (v1, v2) ->
         let env' = env_add (env_add env h v1) t v2 in
         interp_expr e3 env'
      | _ -> raise TypeError)
  | EPair (e1, e2) ->
     VPair (interp_expr e1 env, interp_expr e2 env)
  | ECons (e1, e2) ->
     VCons (interp_expr e1 env, interp_expr e2 env)
  | EAnnot (e, t) ->
     VAnnot (interp_expr e env, t)

let rec interp_prog (ds: prog) (env: env) : env =
  let interp_decl (d: decl) (env: env) : string * value =
    match d with
    | DVal (x, _, e) -> (x, interp_expr (unannot_expr e) env)
    | DFun (is_rec, f, x, _, _, e) ->
       (f,
        if is_rec then
          env_with_me env f (fun env -> VClos (x, unannot_expr e, env))
        else
          VClos (x, unannot_expr e, env))
    | DExp e -> ("-", interp_expr (unannot_expr e) env)
  in
  match ds with
  | [] -> env
  | d::ds ->
     let (x, v) = interp_decl d env in
     let env' = env_add env x v in
     let _ = Format.fprintf Format.std_formatter
               "%s = %a"
               x
               Print.pprint_value v
     in
     let _ = Format.print_newline () in
     interp_prog ds env'
