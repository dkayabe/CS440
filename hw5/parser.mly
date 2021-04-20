%{
    open Types
%}

%token <int> NUM
%token <string> STRING
%token <string> IDENT
%token <string> TIDENT
%token TRUE FALSE UNIT NIL
%token TINT TSTRING TBOOL TUNIT TLIST
%token PLUS MINUS TIMES DIV
%token CARAT
%token AND OR
%token COLON COMMA CONS
%token LT LE GT GE NE
%token EQUAL
%token IF THEN ELSE
%token MATCH WITH PIPE
%token LET REC IN
%token FUN ARROW
%token LPAREN RPAREN
%token DOUBLESEMI
%token EOF
%token APP

%token QUIT

%right ARROW
%nonassoc COLON
%left COMMA
%left LT LE GT GE NE EQUAL
%left PLUS MINUS
%left TIMES DIV
%left AND OR
%left APP


%start prog
%start tdecl
%type <Types.prog> prog
%type <Types.decl> tdecl

%%
expr:
  const                                    { EConst $1 }
| IDENT                                    { EVar $1 }
| infixop                                  { $1 }
| FUN IDENT ARROW expr                     { EFun ($2, $4) }
| IF expr THEN expr ELSE expr              { EIf ($2, $4, $6) }
| LET IDENT optannot EQUAL expr IN expr    { ELet ($2, $3, $5, $7) }
| LET REC IDENT ppat optannot EQUAL expr IN expr
                           { ELetFun (true, $3, fst $4, snd $4, $5, $7, $9) }
| LET IDENT ppat optannot EQUAL expr IN expr
                           { ELetFun (false, $2, fst $3, snd $3, $4, $6, $8) }
| LET LPAREN IDENT COMMA IDENT RPAREN EQUAL expr IN expr
                                           { ELetPair ($3, $5, $8, $10) }
| MATCH expr WITH NIL ARROW expr PIPE IDENT CONS IDENT ARROW expr
                                           { EMatchList ($2, $6, $8, $10, $12) }
| MATCH expr WITH PIPE NIL ARROW expr PIPE IDENT CONS IDENT ARROW expr
                                           { EMatchList ($2, $7, $9, $11, $13) }
| expr CONS expr                           { ECons ($1, $3) }
| LPAREN expr RPAREN                       { $2 }
| expr expr %prec APP                      { EApp ($1, $2) }
| expr COLON typ                           { EAnnot ($1, $3) }
| expr COMMA expr %prec COMMA              { EPair ($1, $3) }
;

ppat:
  IDENT                                    { ($1, None) }
| LPAREN IDENT COLON typ RPAREN            { ($2, Some $4) }

optannot:
  COLON typ                                { Some $2 }
|                                          { None }

const:
  NUM                                      { Num $1 }
| STRING                                   { String $1 }
| TRUE                                     { Bool true }
| FALSE                                    { Bool false }
| UNIT                                     { Unit }
| NIL                                      { Nil }
;

atyp:
  TINT                                     { TInt }
| TSTRING                                  { TString }
| TBOOL                                    { TBool }
| TUNIT                                    { TUnit }
// | TIDENT                                   { TVar $1 }
| LPAREN typ RPAREN                        { $2 }

typ:
  atyp                                     { $1 }
| typ TLIST                                { TList $1 }
| atyp ARROW typ                           { TArrow ($1, $3) }
| atyp TIMES typ                           { TProd ($1, $3) }

infixop:
  expr PLUS expr                           { EInfixop (Plus, $1, $3) }
| expr MINUS expr                          { EInfixop (Minus, $1, $3) }
| expr TIMES expr                          { EInfixop (Times, $1, $3) }
| expr DIV expr                            { EInfixop (Div, $1, $3) }
| expr CARAT expr                          { EInfixop (Concat, $1, $3) }
| expr AND expr                            { EInfixop (And, $1, $3) }
| expr OR expr                             { EInfixop (Or, $1, $3) }
| expr EQUAL expr                          { EInfixop (Eq, $1, $3) }
| expr LT expr                             { EInfixop (Lt, $1, $3) }
| expr LE expr                             { EInfixop (Le, $1, $3) }
| expr GT expr                             { EInfixop (Gt, $1, $3) }
| expr GE expr                             { EInfixop (Ge, $1, $3) }
| expr NE expr                             { EInfixop (Ne, $1, $3) }
;

decl:
| LET IDENT optannot EQUAL expr            { DVal ($2, $3, $5) }
| LET REC IDENT ppat optannot EQUAL expr
                           { DFun (true, $3, fst $4, snd $4, $5, $7) }
| LET IDENT ppat optannot EQUAL expr
                           { DFun (false, $2, fst $3, snd $3, $4, $6) }
| expr                                     { DExp $1 }
;

tdecl:
| decl DOUBLESEMI                          { $1 }
| QUIT                                     { raise Lexer.Quit }
;

prog:
| tdecl EOF                                { [$1] }
| tdecl prog                               { $1::$2 }
;
