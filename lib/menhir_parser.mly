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

%start <form> program_eof

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
  | v=value { Val v }
  | LPAREN hd=form rest=list(form) RPAREN {
    match hd :: rest with
    | [Val (Sym "let"); Val (Sym id); rhs; body] -> Let(id, rhs, body)
    | [Val (Sym "let-syntax"); Val (Sym id); rhs; body] -> LetSyntax(id, rhs, body)
    | _ -> List(hd, rest) 
  }
