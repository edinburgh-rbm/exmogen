type link = Simple | Double | Triple
type atom = Atom of string | Var of string
type smiles_ast = Node of atom * (link * smiles_ast) list
val print_link : link -> string
val print_atom : atom -> string
val print : smiles_ast -> string
val print_list : (link * smiles_ast) list -> string
