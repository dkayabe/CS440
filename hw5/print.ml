open Types

let rec pprint_value f v =
  match v with
  | VConst (Num n) -> Format.fprintf f "%d" n
  | VConst (String s) -> Format.fprintf f "\"%s\"" s
  | VConst (Bool true) -> Format.fprintf f "true"
  | VConst (Bool false) -> Format.fprintf f "false"
  | VConst Unit -> Format.fprintf f "()"
  | VConst Nil -> Format.fprintf f "[]"
  | VPair (v1, v2) -> Format.fprintf f "@[(@[%a@], @[%a@])@]"
                        pprint_value v1
                        pprint_value v2
  | VCons (v1, v2) -> Format.fprintf f "@[(@[%a@])::(@[%a@])@]"
                        pprint_value v1
                        pprint_value v2
  | VClos _ -> Format.fprintf f "<fun>"
  | VAnnot (v, _) -> pprint_value f v

let print_value v =
  (pprint_value Format.std_formatter v;
   Format.pp_print_newline Format.std_formatter ())

let string_of_typ t =
  let rec stl t l =
    let parenat l' s =
      if l >= l' then "(" ^ s ^ ")" else s
    in
    match t with
    | TInt -> "int"
    | TString -> "string"
    | TBool -> "bool"
    | TUnit -> "unit"
    | TList t ->
       parenat 3 ((stl t 2) ^ " list")
    | TArrow (t1, t2) ->
       parenat 2 ((stl t1 2) ^ " -> " ^ (stl t2 1))
    | TProd (t1, t2) ->
       parenat 1 ((stl t1 0) ^ " * " ^ (stl t2 1))
    | TVar s -> s
    | TUVar n -> "?" ^ (string_of_int n)
  in
  stl t 0
