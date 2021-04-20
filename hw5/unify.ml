open Types
open Context
open Print

exception ImplementMe
   
exception UnificationFailure of string
;;

(* Returns true iff unification variable x occurs in t *)
let rec occurs (x: int) (t: typ) =
    match t with
    | TUVar x' -> x = x'
    | TList t -> occurs x t
    | TArrow (t1, t2) -> occurs x t1 || occurs x t2
    | TProd (t1, t2) -> occurs x t1 || occurs x t2
    | _ -> false

(* A substitution is a list of pairs (0, t).
 * [(0, t0); (1, t1)] means "substitute t0 for ?0, t1 for ?1" *)
type substitution = (int * typ) list

(* Substitute t' for unification var x in t *)
let rec substitute (t: typ) (x: int) (t': typ) =
  match t with
  | TInt -> TInt
  | TString -> TString
  | TBool -> TBool
  | TUnit -> TUnit
  | TList t0 -> TList (substitute t0 x t')
  | TArrow (t1, t2) -> TArrow (substitute t1 x t', substitute t2 x t')
  | TProd (t1, t2) -> TProd (substitute t1 x t', substitute t2 x t')
  | TUVar x' ->
     if x = x' then t'
     else t
  | TVar s -> t

(* Apply a substitution to a type *)
let sub_all (sub: substitution) (t: typ) =
  List.fold_left (fun t (x, t') -> substitute t x t')
    (List.fold_left (fun t (x, t') -> substitute t x t') t sub)
    sub

(*<* Problem 2.3 *<*)
(* Unify t1 and t2, returning a substitution that makes the two equal.
 * If they're already equal, return [].
 * If they can't be unified, raise a UnificationFailure. *)
let rec unify (t1: typ) (t2: typ) : substitution =
  raise ImplementMe

(* Generate a new unification variable *)
let uvar_ctr = ref (-1)
let new_uvar () =
  let _ = uvar_ctr := (!uvar_ctr) + 1 in
  !uvar_ctr

(* Generate a new "guess" type (it's just a unification variable). *)
let new_type () = TUVar (new_uvar ())

(* You can ignore all the code below this. *)

(* Returns true iff type variable a occurs free in t *)
let rec free_in (a: string) (t: typ) =
    match t with
    | TVar a' -> a = a'
    | TList t -> free_in a t
    | TArrow (t1, t2) -> free_in a t1 || free_in a t2
    | TProd (t1, t2) -> free_in a t1 || free_in a t2
    | _ -> false

let rec free_in_s (a: string) (s: schema) =
  match s with
  | SMono t -> free_in a t
  | SForall (a', s) -> if a = a' then false else free_in_s a s
        

(* Substitute t' for type var x in t *)
let rec substitute_tv (t: typ) (x: string) (t': typ) =
  match t with
  | TInt -> TInt
  | TString -> TString
  | TBool -> TBool
  | TUnit -> TUnit
  | TList t0 -> TList (substitute_tv t0 x t')
  | TArrow (t1, t2) -> TArrow (substitute_tv t1 x t', substitute_tv t2 x t')
  | TProd (t1, t2) -> TProd (substitute_tv t1 x t', substitute_tv t2 x t')
  | TUVar x' -> t
  | TVar x' ->
     if x = x' then t'
     else t

let tvar_ctr = ref (Char.code 'a')
let new_tvar () =
  let n = !tvar_ctr in
  let _ = if n > Char.code 'z' then
            raise (UnificationFailure
                     "OK, not actually a unification failure, but having more than 26 type variables is just unreasonable and not supported.")
  in
  let v = "'" ^ String.make 1 (Char.chr n) in
  let _ = tvar_ctr := (!tvar_ctr) + 1 in
  v

let rec sub_schema_tv (s: schema) (x: string) (t': typ) =
  match s with
  | SMono t -> SMono (substitute_tv t x t')
  | SForall (a, s) ->
     if free_in a t' then
       let a' = new_tvar () in
       SForall (a', sub_schema_tv (sub_schema_tv s a (TVar a')) x t')
     else
       SForall (a, sub_schema_tv s x t')
  
let rec sub_schema (s: schema) (x: int) (t': typ) =
  match s with
  | SMono t -> SMono (substitute t x t')
  | SForall (a, s) ->
     if free_in a t' then
       let a' = new_tvar () in
       SForall (a', sub_schema (sub_schema_tv s a (TVar a')) x t')
     else
       SForall (a, sub_schema s x t')
    
let substitute_all_s (sub: substitution) (s: schema) =
  List.fold_right (fun (x, t') t -> sub_schema t x t') sub s

let sub_ctx (s: substitution) (c: ctx) : ctx =
  VMap.map (substitute_all_s s) c

let rec inst s =
  match s with
  | SMono t -> t
  | SForall (a, s) ->
     let n = new_uvar () in
     inst (sub_schema_tv s a (TUVar n))

let rec gen t =
  let rec free_uvars t =
    match t with
    | TList t -> free_uvars t
    | TArrow (t1, t2)
      | TProd (t1, t2) -> (free_uvars t1) @ (free_uvars t2)
    | TUVar n -> [n]
    | _ -> []
  in
  let free = free_uvars t in
  List.fold_left
    (fun s n ->
      let a = new_tvar () in
      SForall (a, sub_schema s n (TVar a)))
    (SMono t)
    free

    
let string_of_sch s =
  let tvar_ctr = ref (Char.code 'a') in
  let new_tvar () =
    let n = !tvar_ctr in
    let _ = if n > Char.code 'z' then
              raise (UnificationFailure
                       "OK, not actually a unification failure, but having more than 26 type variables is just unreasonable and not supported.")
    in
    let v = "'" ^ String.make 1 (Char.chr n) in
    let _ = tvar_ctr := (!tvar_ctr) + 1
    in
    v
  in
  let rec sos_rec s =
    match s with
    | SMono t -> string_of_typ t
    | SForall (a, s) ->
       let a' = new_tvar () in
       sos_rec (sub_schema_tv s a (TVar a'))
  in
  sos_rec s
