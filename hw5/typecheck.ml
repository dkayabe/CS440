open Types
open Unify
open Context

(*<* Problem 2.4 *<*)
let type_of_const (c: const) : typ =
  raise Unify.ImplementMe

let type_of_op (o: infixop) : typ * typ * typ =
  match o with
  | Plus | Minus | Times | Div -> (TInt, TInt, TInt)
  | Lt | Le | Gt | Ge | Eq | Ne -> (TInt, TInt, TBool)
  | And | Or -> (TBool, TBool, TBool)
  | Concat -> (TString, TString, TString)

exception TypeError of string

let rec typecheck_exp (c: ctx) (e: expr) : typ * substitution  =
  match e with
  | EConst c -> (type_of_const c, [])
  | EVar x ->
     (match ctx_find c x with
      | None -> raise (TypeError (Printf.sprintf "unbound identifier %s"
                                    x))
      | Some t -> (inst t, []))
  | EInfixop (o, e1, e2) ->
     let (t1, t2, t3) = type_of_op o in
     let (t1', s) = typecheck_exp c e1 in
     let (t2', s1) = typecheck_exp (sub_ctx s c) e2 in
     let s2 = unify (sub_all s1 t1') t1 in
     let s3 = unify (sub_all s2 t2') t2 in
     (t3, s @ s1 @ s2 @ s3)
  | EFun (x, e) ->
     let xt = new_type () in
     let c' = ctx_add c x (SMono xt) in
     let (t', s) = typecheck_exp c' e in
     (TArrow (sub_all s xt, t'), s)
  | EIf (e1, e2, e3) ->
     let (t1, s) = typecheck_exp c e1 in
     let s1 = unify t1 TBool in
     let (t2, s2) = typecheck_exp (sub_ctx s1 c) e2 in
     let (t3, s3) = typecheck_exp (sub_ctx s2 c) e3 in
     let s4 = unify (sub_all s3 t2) t3 in
     (t2, s @ s1 @ s2 @ s3 @ s4)
  | ELet (x, t, e1, e2) ->
     let (t1, s) = typecheck_exp c e1 in
     let s = (match t with
                None -> s
              | Some t' -> s @ (unify t1 t'))
     in
     let c' = ctx_add c x (gen (sub_all s t1)) in
     let (t2, s1) = typecheck_exp c' e2 in
     (t2, s @ s1)
  | ELetFun (is_rec, f, x, t1, t2, e1, e2) ->
     let tx = new_type () in
     let s =
       (match t1 with
        | Some t1' -> unify tx t1'
        | None -> [])
     in
     let tr = new_type () in
     let s' =
       (match t2 with
        | Some t2' -> unify tr t2'
        | None -> [])
     in
     let c' = ctx_add c x (SMono tx) in
     let tf = TArrow (tx, tr) in
     let c' = if is_rec then ctx_add c' f (SMono tf) else c' in
     let (te, s1) = typecheck_exp (sub_ctx (s @ s') c') e1 in
     let s2 = unify (sub_all s1 tr) te in
     let c'' = ctx_add c f (gen (sub_all (s @ s' @ s1 @ s2) tf)) in
     let (t, s3) = typecheck_exp (sub_ctx (s @ s' @ s1 @ s2) c'') e2 in
     (sub_all (s @ s' @ s1 @ s2 @ s3) t, s @ s' @ s1 @ s2 @ s3)
  | ELetPair (x, y, e1, e2) ->
     let (t1, s) = typecheck_exp c e1 in
     let tx = new_type () in
     let ty = new_type () in
     let s1 = unify t1 (sub_all s (TProd (tx, ty))) in
     let c' = ctx_add (ctx_add c x (SMono tx)) y (SMono ty) in
     let (t2, s2) = typecheck_exp (sub_ctx (s @ s1) c') e2 in
     (t2, s @ s1 @ s2)
  | EApp (e1, e2) ->
     let (t1, s1) = typecheck_exp c e1 in
     let (t2, s2) = typecheck_exp (sub_ctx s1 c) e2 in
     let tr = new_type () in
     let s3 = unify (sub_all s2 t1) (TArrow (t2, tr)) in
     (sub_all (s1 @ s2 @ s3) tr, s1 @ s2 @ s3)
  | EMatchList (e1, e2, h, t, e3) ->
     let (t1, s) = typecheck_exp c e1 in
     let ta = new_type () in
     let s1 = unify t1 (TList ta) in
     let (t2, s2) = typecheck_exp (sub_ctx (s @ s1) c) e2 in
     let c' = ctx_add (ctx_add c h (SMono ta)) t (SMono t1) in
     let (t3, s3) = typecheck_exp (sub_ctx (s @ s1 @ s2) c') e3 in
     let s4 = unify (sub_all s3 t2) t3 in
     (t3, s @ s1 @ s2 @ s3 @ s4)
  | EPair (e1, e2) ->
     let (t1, s1) = typecheck_exp c e1 in
     let (t2, s2) = typecheck_exp (sub_ctx s1 c) e2 in
     (TProd (sub_all s2 t1, t2), s1 @ s2)
  | ECons (e1, e2) ->
     let (t1, s1) = typecheck_exp c e1 in
     let (t2, s2) = typecheck_exp c e2 in
     let tl = TList (sub_all s2 t1) in
     let s3 = unify t2 tl in
     (tl, s1 @ s2 @ s3)
  | EAnnot (e, t) ->
     let (t', s) = typecheck_exp c e in
     let s' = unify t t' in
     (t', s @ s')

let typecheck_decl (c: ctx) (d: decl) : typ * substitution =
  match d with
  | DVal (x, topt, e) ->
     typecheck_exp c (ELet (x, topt, e, EVar x))
  | DFun (is_rec, f, x, t1, t2, e) ->
     typecheck_exp c (ELetFun (is_rec, f, x, t1, t2, e, EVar f))
  | DExp e ->
     typecheck_exp c e
     
let var_of_decl d =
  match d with
  | DVal (x, _, _) -> x
  | DFun (_, f, _, _, _, _) -> f
  | DExp _ -> "-"
     
let rec typecheck_prog (c: ctx) (ds : prog) ds' : (string * typ) list =
  match ds with
  | [] -> ds'
  | d::ds ->
     let (td, s) = typecheck_decl c d in
     let x = var_of_decl d in
     let c' = ctx_add c x (gen td) in
     typecheck_prog (sub_ctx s c') ds ((x, td)::ds')
     
let tc_prog (ds: prog) : unit =
  let ds' = typecheck_prog Context.empty_ctx ds [] in
  List.iter (fun (x, t) -> Printf.printf "%s: %s\n"
                             x
                             (string_of_sch (gen t)))
    (List.rev ds')
       
(*
let typecheck_decl (c: ctx) (d: decl) : tvar =
  match d with
  | DVal (_, topt, e) ->
     let t = typecheck_exp c e in
     let t' =
       (match topt with
        | Some t' ->
           let t' = varify t' in
           unify t t';
           t'
        | None -> t)
     in
     t'
  | DFun (is_rec, f, x, t1, t2, e) ->
     let tx = new_type () in
     let _ =
       (match t1 with
        | Some t1' -> unify tx (varify t1')
        | None -> ())
     in
     let tr = new_type () in
     let _ =
       (match t2 with
        | Some t2' -> unify tr (varify t2')
        | None -> ())
     in
     let c' = ctx_add c x (SMono tx) in
     let tf = ref (TTArrow (tx, tr)) in
     let c' = if is_rec then ctx_add c' f (SMono tf) else c' in
     let te = typecheck_exp c' e in
     let _ = unify tr te in
     tf

let var_of_decl d =
  match d with
  | DVal (x, _, _) -> x
  | DFun (_, f, _, _, _, _) -> f
     
let typecheck_prog ((ds, e) : prog) : tvar =
  let rec typecheck_decls (c: ctx) (ds: decl list) =
    match ds with
    | [] -> c
    | d::ds ->
       let td = typecheck_decl c d in
       let gtd = gen td in
       let tdm = inst gtd in
       let x = var_of_decl d in
       let _ = Printf.printf "%s: %s\n"
                 x
                 (string_of_ttyp tdm)
       in
       let c' = ctx_add c x gtd in
       typecheck_decls c' ds
  in
  let c = typecheck_decls empty_ctx ds in
  typecheck_exp c e
  *)
