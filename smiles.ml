(* Tree-like smiles parsing. *)
type link =
| Simple
| Double
| Triple

type atom =
| Atom of string
| Var  of string
    
type smiles_ast =
| Node of atom * (link * smiles_ast) list
    


let print_link = function
  | Simple -> "-"
  | Double -> "="
  | Triple -> "#"

let print_atom = function
  | Atom s -> s
  | Var  s -> Printf.sprintf "{%s}" s

let rec print = function
  | Node(atom, sub) ->
    Printf.sprintf "%s%s" (print_atom atom) (print_list sub)

and print_list = function
  | [] -> ""
  | (link, mol) :: tl ->
    (match mol with
    | Node(_, []) ->
      Printf.sprintf "%s%s%s" (print_link link) (print mol) (print_list tl)
    | _ ->
      Printf.sprintf "(%s%s)%s" (print_link link) (print mol) (print_list tl)
    )
