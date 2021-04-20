let rec sum l =
  match l with
  | [] -> 0
  | h::t -> h + (sum t)
;;

let rec length l =
  match l with
  | [] -> 0
  | h::t -> 1 + (length t)
;;

let rec map fl = (* (fl: ((int -> string) * (int list))) =  *)
  let (f, l) = fl in
  match l with
  | [] -> []
  | h::t -> (f h)::(map (f, t))
;;

let int_to_string n =
  if n = 1 then "one"
  else if n = 2 then "two"
  else if n = 3 then "three"
  else "big"
;;

let onetwothree = map (int_to_string, 1::2::3::[])
;;
