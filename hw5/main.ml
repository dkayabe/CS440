open Unify

let rec repl ch ctx env =
  let _ = Printf.printf "# %!" in
  let lexbuf = Lexing.from_channel ch in
  try
    let d = Parser.tdecl Lexer.token lexbuf in
    let (ty, s) = Typecheck.typecheck_decl ctx d in
    let ctx' =
      sub_ctx s (Context.ctx_add ctx (Typecheck.var_of_decl d) (gen ty))
    in
    
    let _ = Printf.printf "%s: %s\n"
              (Typecheck.var_of_decl d)
              (string_of_sch (gen ty))
    in
    let env' = Interp.interp_prog [d] env in
    repl ch ctx' env'
  with Parsing.Parse_error ->
        (Printf.printf "Parse error\n%!";
         repl ch ctx env)
     | Typecheck.TypeError s ->
        (Printf.printf "Type error: %s\n%!" s;
         repl ch ctx env)
     | Unify.UnificationFailure s ->
        (Printf.printf "Type error: %s\n%!" s;
         repl ch ctx env)
     | Lexer.Quit -> exit 0
  
;;

if Array.length Sys.argv > 2 then
  (Printf.printf "Usage: ./miniml [filename]\n";
   exit 1)
else if Array.length Sys.argv = 1 then
  (Printf.printf "Welcome to MiniCaml. Type #quit;; to exit.\n%!";
   repl stdin Context.empty_ctx Env.empty_env)
else
  let fname = Array.get Sys.argv 1 in

  let chan = open_in fname in
  let lexbuf = Lexing.from_channel chan
  in
  let p = Parser.prog Lexer.token lexbuf in
  let _ = Typecheck.tc_prog p in
  let _ = Printf.printf "\n" in
  ignore (Interp.interp_prog p (Env.empty_env))
