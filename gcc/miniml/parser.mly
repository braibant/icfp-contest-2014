%{
  open Syntax
%}

%token TINT
%token TBOOL
%token TARROW
%token <Syntax.name> VAR
%token <int> INT
%token TRUE FALSE
%token PLUS
%token MINUS
%token TIMES
%token EQUAL LESS
%token IF THEN ELSE
%token FUN REC IS
%token COLON
%token LPAREN RPAREN
%token LET IN
%token SEMICOLON2
%token EOF

%start toplevel
%type <Syntax.toplevel_cmd list> toplevel

%nonassoc IN
%nonassoc LET
%nonassoc FUN REC IS
%nonassoc IF THEN ELSE
%nonassoc EQUAL LESS
%left PLUS MINUS
%left TIMES
%left COLON
%right TARROW

%%

toplevel:
    EOF                      { [] }
  | def EOF                  { [$1] }
  | def SEMICOLON2 EOF       { [$1] }
  | expr EOF                 { [Expr $1] }
  | expr SEMICOLON2 EOF      { [Expr $1] }
  | def SEMICOLON2 toplevel  { $1 :: $3 }
  | expr SEMICOLON2 toplevel { (Expr $1) :: $3 }

def:
    LET VAR EQUAL expr { Def ($2, $4) }
  | LET REC VAR LPAREN VAR COLON ty RPAREN COLON ty EQUAL expr
      { Def($3, FunRec ($3, $5, $7, $10, $12)) }
  | LET VAR LPAREN VAR COLON ty RPAREN EQUAL expr
      { Def($2, Fun ($4, $6, $9)) }

expr:
    non_app             { $1 }
  | app                 { $1 }
  | arith               { $1 }
  | boolean             { $1 }
  | IF expr THEN expr ELSE expr	{ If ($2, $4, $6) }
  | FUN REC VAR LPAREN VAR COLON ty RPAREN COLON ty IS expr
      { FunRec ($3, $5, $7, $10, $12) }
  | LET VAR COLON ty EQUAL expr IN expr
      { Apply(Fun ($2, $4, $8),$6) }

app:
    app non_app         { Apply ($1, $2) }
  | non_app non_app     { Apply ($1, $2) }

non_app:
    VAR		        	  { Var $1 }
  | TRUE                	  { Bool true }
  | FALSE               	  { Bool false }
  | INT		                  { Int $1 }
  | LPAREN expr RPAREN		  { $2 }

arith:
  | MINUS INT           { Int (-$2) }
  | expr PLUS expr	{ Plus ($1, $3) }
  | expr MINUS expr	{ Minus ($1, $3) }
  | expr TIMES expr	{ Times ($1, $3) }

boolean:
  | expr EQUAL expr { Equal ($1, $3) }
  | expr LESS expr  { Less ($1, $3) }

ty:
    TBOOL	 { TBool }
  | TINT         { TInt }
  | ty TARROW ty { TArrow ($1, $3) }
  | LPAREN ty RPAREN { $2 }

%%

