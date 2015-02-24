%{
  open Smiles
%}

%token ARROW REVARROW
%token SINGLE DOUBLE TRIPLE
%token DOT EOF
%token PLUS
%token OPENP CLOSEP OPENBRACKET CLOSEBRACKET OPENBRACE CLOSEBRACE
%token <string> FLOAT INTEGER IDENT VAR

%start molecule
%type <Smiles.smiles_ast> molecule

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
 | atom list(protected_molecule) EOF { Node($1, $2) }
;

protected_molecule:
 | link atom                  { ($1, Node($2, [])) }
 | link OPENP molecule CLOSEP { ($1, $3) }
;

%%
