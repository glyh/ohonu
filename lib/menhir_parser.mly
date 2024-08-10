%{
  [@@@coverage exclude_file]
  open Ast

%}

%token EOF
%token LPAREN
%token RPAREN
%token TRUE
%token FALSE
%token <string> SYMBOL
%token <string> STR
%token <int> INT

%start <value> program_eof

%%

program_eof:
  | f=form EOF { f }

value: 
  | s=SYMBOL { Sym(s) }
  | s=STR { String(s) }
  | TRUE { Bool(true) }
  | FALSE { Bool(false) }
  | i=INT { Int(i) }
form:
  | v=value { v }
  | LPAREN lst=list(form) RPAREN {
    List(lst)
  }
