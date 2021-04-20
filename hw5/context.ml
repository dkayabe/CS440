open Types
   
module S =
  struct
    type t = string
    let compare = String.compare
  end

module VMap = Map.Make(S)

type ctx = schema VMap.t

let empty_ctx = VMap.empty

let ctx_add (c: ctx) (x: string) (t: schema) = VMap.add x t c
let ctx_find (c: ctx) (x: string) = VMap.find_opt x c
