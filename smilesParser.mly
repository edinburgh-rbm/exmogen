%{
  open Reactions
%}

%token ARROW REVARROW
%token SINGLE DOUBLE TRIPLE
%token DOT EOF
%token PLUS
%token OPENP CLOSEP OPENBRACKET CLOSEBRACKET OPENBRACE CLOSEBRACE SEMICOLON
%token <string> FLOAT INTEGER IDENT VAR

%start reactions
%type <Reactions.reactions> reactions

%%

atom:
 | IDENT { Atom($1) }
 | VAR   { Var($1)  }
;

link:
 | SINGLE { Simple }
 | DOUBLE { Double }
 | TRIPLE { Triple }

molecule:
 | atom list(protected_molecule) { Node($1, $2) }
;

protected_molecule:
 | link atom                  { ($1, Node($2, [])) }
 | link OPENP molecule CLOSEP { ($1, $3) }
;

molecules:
 | l = separated_list(PLUS, molecule)  { l }
;

reversible_reaction:
 | molecules REVARROW molecules { { input = $1; output = $3} }
;

reactions:
 | l = separated_list(SEMICOLON, reversible_reaction) EOF { l }
;     

%%
