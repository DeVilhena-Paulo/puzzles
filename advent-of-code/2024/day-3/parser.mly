
/* Syntactic analyser */

%{
  open Ast
%}

/* Tokens. */
%token <int * int> MUL
%token DO
%token DONT
%token EOF

/* Starting point of the grammar. */
%start commands

/* Type of values produced by the syntactic analyser. */
%type <Ast.commands> commands

%%

/* Grammar rules. */

commands:
| args_list = list(command) EOF
    { args_list }
;

command:
| args = MUL
    { let a, b = args in Mul (a, b) }
| DO
    { Do }
| DONT
    { Dont }
;
