let f x = x + 1
;;

let three = f 2
;;

let g (x: string) = "Hi, " ^ x ^ "!"
;;

let greet = g "Name"
;;

let h x = fun y -> x + y
;;

let hof f = (f ()) + 1
;;
