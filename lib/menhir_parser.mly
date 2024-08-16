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

%start <cst> program_eof

%%

program_eof:
  | f=form EOF { f }

value: 
  | s=SYMBOL { CSym(s) }
  | s=STR { CString(s) }
  | TRUE { CBool(true) }
  | FALSE { CBool(false) }
  | i=INT { CInt(i) }
form:
  | v=value { v }
  | LPAREN lst=list(form) RPAREN {
    CList(lst)
  }
