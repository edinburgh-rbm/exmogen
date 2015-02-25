type link = Simple | Double | Triple
type atom = Atom of string | Var of string
type smiles_ast = Node of atom * (link * smiles_ast) list
type reaction = { input : smiles_ast list; output : smiles_ast list; }
type reactions = reaction list
val extract_variables : string list -> smiles_ast -> string list
val print_link : link -> string
val print_atom : atom -> string
val print : smiles_ast -> string
val print_list : (link * smiles_ast) list -> string
val print_molecules : smiles_ast list -> string
val print_reaction : reaction -> string
val print_reactions : reaction list -> string
