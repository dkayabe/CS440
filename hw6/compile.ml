open Tac
exception ImplementMe

(*>* Problem 1.1 *>*)
let rec compile_intexpr (e: intexpr) : code * addr =
  match e with
  |Num n -> ([], ANum n)
  |Var v -> ([], AVar v)
  |NBinop (n1, e1, e2) -> let (c1, a1) = compile_intexpr e1 in
                         let (c2, a2) = compile_intexpr e2 in
                         let v1 = new_var() in
                         (List.concat [c1; c2; [Binop(v1, n1, a1, a2)]], AVar v1)

(*>* Problem 1.2 *>*)
let rec compile_boolexpr (e: boolexpr) (t: label) (f: label) : code =
  match e with
  | Relop (Lt, e1, e2) -> let (c1, a1) = compile_intexpr e1 in
                          let (c2, a2) = compile_intexpr e2 in
                          let a3 = new_var() in
                          List.concat [c1; c2; [Binop (a3, Minus, a1, a2); JumpIfNeg (t, AVar a3); Jump f]]
  | Relop (Gt, e1, e2) -> let (c1, a1) = compile_intexpr e1 in
                          let (c2, a2) = compile_intexpr e2 in
                          let a3 = new_var() in
                          List.concat [c1; c2; [Binop (a3, Minus, a2, a1); JumpIfNeg (t, AVar a3); Jump f]]
  | Relop (Eq, e1, e2) -> let (c1, a1) = compile_intexpr e1 in
                          let (c2, a2) = compile_intexpr e2 in
                          let a3 = new_var() in
                          List.concat [c1; c2; [Binop (a3, Minus, a1, a2); JumpIfZero (t, AVar a3); Jump f]]
  | Relop (Le, e1, e2) -> compile_boolexpr (Relop (Gt, e1, e2)) f t
  | Relop (Ge, e1, e2) -> compile_boolexpr (Relop (Lt, e1, e2)) f t
  | Relop (Ne, e1, e2) -> compile_boolexpr (Relop (Eq, e1, e2)) f t
  | And (e1, e2) -> let l = new_label() in
                    let c1 = (compile_boolexpr e1 l f) in
                    let c2 = (compile_boolexpr e2 t f) in
                    List.concat [c1; [Label l]; c2]
  | Or (e1, e2) -> let l = new_label() in
                   let c1 = (compile_boolexpr e1 t l) in
                   let c2 = (compile_boolexpr e2 t f) in
                   List.concat [c1; [Label l]; c2]
  | Not e1 -> (compile_boolexpr e1 f t)

(*>* Problem 1.3 *>*)
let rec compile_stmt (s: stmt) : code =
  match s with
  | Assign (x, e) -> let (c1, a1) = compile_intexpr e in
                     List.concat [c1; [Copy (x, a1)]]
  | If (e, ifb, None) -> let l = new_label()) in
                         let l2 = new_label()) in
                         let cb = (compile_boolexpr e l l2) in
                         List.concat [cb; [Label l]; (compile_stmt ifb); [Label l2]]
  | If (e, ifb, Some elseb) -> let l1 = (new_label()) in
                               let l2 = (new_label()) in
                               let l3 = (new_label()) in
                               let cb = (compile_boolexpr e l1 l2) in
                               List.concat [cb; [Label l1]; compile_stmt ifb; [Jump l3; Label l2]; compile_stmt else b; [Label l3]] | While (e, s) -> let l = (new_label()) in
                            let l1 = (new_label()) in
                            let l2 = (new_label()) in
                            let l3 = (new_label()) in
                            let cb = (compile_boolexpr e l1 l2)
  | While (e, s) -> raise ImplementMe
  | Return e -> raise ImplementMe
  | Block b -> List.concat (List.map compile_stmt b)
