%{
    open Figures
    exception ImplementMe
%}

%token <int> NUM
%token <string> STRING
%token <string> IDENT
%token CAPTION
%token ABOVE LEFT RIGHT BELOW OVER
%token LBRACE RBRACE
%token LPAREN RPAREN
%token HORIZ VERT
%token BORDER
%token EOF

%start figure
%type <Figures.content> figure
%type <Figures.cell list> panels
%type <Figures.content> panel
%type <Figures.content> caption

%%
figure:
  HORIZ optcap LBRACE panels RBRACE   { let t = table_one_row $4 in
					match $2 with
					| Some div ->
					   table_one_col
					     [tabcell None t; tabcell None div]
					| None -> t
				      }
| VERT optcap LBRACE panels RBRACE   { let t = table_one_col $4 in
					match $2 with
					| Some div ->
					   table_one_row
					     [tabcell None t; tabcell None div]
					| None -> t
				      }
| panel                               { $1 } 
;

optcap:
  caption                                  { Some $1 }
|                                          { None }

/* Please do not change the commments for autograder */
/*<* Problem 1.2a *<*/ 
/* Grammar rules for "panels" goes here */
panels:
	panel									{ [ (tabcell None $1) ] }
|	panel panels							{ (tabcell None $1):: $2 }
/*>* Problem 1.2a *>*/ 

/*<* Problem 1.2b *<*/ 
/* Grammar rules for "panel" goes here */
panel:
	LPAREN IDENT CAPTION ABOVE caption RPAREN			{ table_one_col [tabcell None $5; tabcell (Some $2) nocontent] }
|	LPAREN IDENT CAPTION BELOW caption RPAREN			{ table_one_col [tabcell (Some $2) nocontent ; tabcell None $5] }
|	LPAREN IDENT CAPTION RIGHT caption RPAREN			{ table_one_row [tabcell (Some $2) nocontent ; tabcell None $5] }
|	LPAREN IDENT CAPTION LEFT caption RPAREN			{ table_one_row [tabcell None $5; tabcell (Some $2) nocontent]}
|	LPAREN IDENT CAPTION OVER caption RPAREN			{ table_one_row [tabcell (Some $2) $5]}
/*>* Problem 1.2b *>*/ 

optborder:
                                           { false }
| BORDER                                   { true }
;

optfont: 
                                           { None }
| IDENT                                    { Some $1 }
;

optsize: 
                                           { None }
| NUM                                      { Some $1 }
;

optcolor: 
                                           { None }
| IDENT                                    { Some $1 }
;

/*<* Problem 1.1 *<*/ 
caption:
  optsize optborder STRING           { div Center None $1 None $2 $3}
| optsize IDENT optborder STRING     { div Center (Some $2) $1 None $3 $4}
| optsize IDENT IDENT optborder STRING
                                     { div Center (Some $2) $1 (Some $3) $4 $5 }
;
/*>* Problem 1.1 *>*/ 
