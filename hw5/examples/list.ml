let emp : int list = []
;;

let one = 1::[]
;;

let t =
  match emp with
  | [] -> true
  | h::t -> false
;;

let f =
  match one with
  | [] -> (true : bool)
  | h::t -> false
;;
