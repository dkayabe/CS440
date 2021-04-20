type const = Num of int
           | String of string
           | Bool of bool
           | Unit
           | Nil

type typ = TInt
         | TString
         | TBool
         | TUnit
         | TList of typ
         | TArrow of typ * typ
         | TProd of typ * typ
         | TVar of string (* Type variables like 'a *)
         | TUVar of int (* Unification variables like ?0 *)

(* Don't worry about this *)
type schema = SMono of typ
            | SForall of string * schema

(* Infix operators -- we have them now! *)
type infixop = Plus | Minus | Times | Div
               | Lt | Le | Gt | Ge | Eq | Ne
               | And | Or
               | Concat

(* Don't worry about this either, it'sused for building contexts *)
module S =
  struct
    type t = string
    let compare = String.compare
  end

module VMap = Map.Make(S)

(* Expressions *)
type expr =
  | EVar of string
  | EConst of const
  | EInfixop of infixop * expr * expr
  | EFun of string * expr
  | EIf of expr * expr * expr (* if e1 then e2 else e3 *)
  | ELet of string * typ option * expr * expr (* let x (: t) = e1 in e2 *)
  (* let (rec?) f (x (:t1)) (:t2) = e1 in e2 *)
  | ELetFun of bool * string * string * typ option * typ option * expr * expr
  | ELetPair of string * string * expr * expr (* let (x, y) = e1 in e2 *)
  | EApp of expr * expr
  (* match e1 with | [] -> e2 | h::t -> e3 *)
  | EMatchList of expr * expr * string * string * expr
  | EPair of expr * expr
  | ECons of expr * expr
  | EAnnot of expr * typ (* Annotations (e : t) *)
            
(* Values are now fairly separate from expressions.
 * They're only used as the result of evaluation and you shouldn't need to
 * worry about them. *)
type value = VConst of const
           | VPair of value * value
           | VCons of value * value
           | VClos of string * expr * value ref VMap.t
           | VAnnot of value * typ

type decl =
  | DVal of string * typ option * expr (* let x (: t) = e;; *)
  (* let (rec?) f (x (:t1)) (:t2) = e1;; *)
  | DFun of bool * string * string * typ option * typ option * expr
  (* e;; *)
  | DExp of expr

(* A program is a list of declarations *)
type prog = decl list
