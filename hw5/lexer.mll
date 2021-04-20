{

  open Parser
  exception Quit

}

let digit = ['0'-'9']
let identchar = ['a'-'z' 'A'-'Z' '\'' '_' '0'-'9']
let ident = ['a'-'z'] identchar*
let opencomment = "(*"
let closecomment = "*)"
let ws = [' ' '\t' '\n']

rule comment = parse
       | closecomment { token lexbuf }
       | _ { comment lexbuf}
and token = parse
       | ws { token lexbuf }
       | opencomment { comment lexbuf }
       | digit+ as n { NUM (int_of_string n) }
       | '\"' ((_ # '\"')* as s) '\"' { STRING s }
       | "true" { TRUE }
       | "false" { FALSE }
       | "()" { UNIT }
       | "[]" { NIL }

       | "int" { TINT }
       | "string" { TSTRING }
       | "bool" { TBOOL }
       | "unit" { TUNIT }
       | "list" { TLIST }
       | ('\'' ident) as s { TIDENT s }

       | "," { COMMA }
       | "+" { PLUS }
       | "-" { MINUS }
       | "*" { TIMES }
       | "/" { DIV }
       | "^" { CARAT }
       | "::" { CONS }
       | ":" { COLON }
       | "&&" { AND }
       | "||" { OR }
       | "<=" { LE }
       | "<" { LT }
       | ">=" { GE }
       | ">" { GT }
       | "<>" { NE }
             
       | "=" { EQUAL }

       | "fun" { FUN }
       | "->" { ARROW }

       | "if" { IF }
       | "then" { THEN }
       | "else" { ELSE }

       | "match" { MATCH }
       | "with" { WITH }
       | "|" { PIPE }

       | "let" { LET }
       | "rec" { REC }
       | "in" { IN }

       | "(" { LPAREN }
       | ")" { RPAREN }
               
       | ident as s { IDENT s }

       | eof { EOF }
       | ";;" { DOUBLESEMI }
       | "#quit;;" { QUIT }