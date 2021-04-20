open Types

type env = value ref VMap.t

let empty_env = VMap.empty

let env_add (e: env) (x: string) (v: value) = VMap.add x (ref v) e
let env_union (e1: env) (e2: env) =
  VMap.merge (fun _ a b ->
      match (a, b) with
      | (a, None) -> a
      | _ -> b)
    e1
    e2
let env_find (e: env) (x: string) =
  match VMap.find_opt x e with
  | Some r -> Some (!r)
  | None -> None

let env_with_me (e: env) (x: string) (f: env -> value) : value =
  let r = ref (VConst Unit) in
  let env' = VMap.add x r e in
  let v = f env' in
  r := v;
  v
  
  
