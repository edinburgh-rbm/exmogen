(* ---------------------------
  Testing grafting of graphs *)

let c = Atom.({ atom = C; arity = 4 })

let h = Atom.({ atom = H; arity = 1 })

let zero  = Graph.v_of_int 0
let one   = Graph.v_of_int 1
let two   = Graph.v_of_int 2
let three = Graph.v_of_int 3

let mol1 = 
  let g = Graph.empty in
  let g = Graph.add_node_with_colour g c in
  let g = Graph.add_node_with_colour g c in
  let g = Graph.add_node_with_colour g h in
  let g = Graph.add_edge g zero Link.Triple one in
  let g = Graph.add_edge g one Link.Simple two in
  g

let mol2 = 
  let g = Graph.empty in
  let g = Graph.add_node_with_colour g c in
  let g = Graph.add_node_with_colour g h in
  let g = Graph.add_node_with_colour g h in
  let g = Graph.add_node_with_colour g h in
  let g = Graph.add_edge g zero Link.Simple one in
  let g = Graph.add_edge g zero Link.Simple two in
  let g = Graph.add_edge g zero Link.Simple three in
  g

let mol3 =
  Graph.graft mol1 mol2 zero zero

let _ =
  Graph.to_dot "mol1" "mol1" mol1 (fun nlab id -> Atom.print nlab) Link.print

let _ =
  Graph.to_dot "mol2" "mol2" mol2 (fun nlab id -> Atom.print nlab) Link.print

let _ =
  Graph.to_dot "mol3" "mol3" mol3 (fun nlab id -> Atom.print nlab) Link.print


(*-----------------------------
  Testing group theoretic stuff*)

open Group

type gen = R | F

let r = Elt R
let f = Elt F

let print = function
  | R -> "r"
  | F -> "f"

let print_elt f = function
  | Elt x -> f x
  | Inv x -> (f x)^"^-"


open Prelude

let dihedral = {
  Group.generators = [| R; F |];
  Group.relators   = [| Array.make 8 r;
                        Array.make 2 f;
                        [| r; f; r; f |] |];
  Group.subgroup   = [||]
}  

let tables, state = ToddCoxeter.todd_coxeter dihedral

let words = ToddCoxeter.abstract_representation tables

let _ = 
  List.iter (fun word ->
    List.iter ((print_elt print) ++ print_string) word;
    print_newline ()
  ) words


type gen2 = A | B

let print = function A -> "a" | B -> "b"

let a = Elt A
let b = Elt B
let ai = Inv A
let bi = Inv B

let quaternion = {
  Group.generators = [| A; B |];
  Group.relators   = [| Array.make 4 a;
                        Array.make 4 b;
                        [| a; a; bi; bi |];
                        [| bi; a; b; a |] |];
  Group.subgroup   = [||]
}  

let tables, state = ToddCoxeter.todd_coxeter quaternion

let words = ToddCoxeter.abstract_representation tables

let _ = 
  List.iter (fun word ->
    List.iter ((print_elt print) ++ print_string) word;
    print_newline ()
  ) words




open Group

module Dihedral =
  struct
    
    type generator = R | F

    type elt = generator and_inverses

    let generators = [| R; F |]

    let r = Elt R
    let f = Elt F

    let relators = [| Array.make 8 r;
                      Array.make 2 f;
                      [| r; f; r; f |] |]

    let subgroup = [| |]

    let print = function
      | R -> "R"
      | F -> "F"

  end

module Cyclic =
  struct
    
    type generator = G

    type elt = generator and_inverses

    let generators = [| G |]

    let g = Elt G

    let relators = [| Array.make 5 g |]

    let subgroup = [| |]

    let print = function
      | G -> "G"

  end


module Quaternion =
  struct
    
    type generator = A | B

    type elt = generator and_inverses

    let generators = [| A; B |]

    let a = Elt A
    let b = Elt B

    let ia = Inv A
    let ib = Inv B


    let relators = [| Array.make 4 a;
                      Array.make 4 b;
                      [| a; a; ib; ib |];
                      [| ib; a; b; a |]
                   |]

    let subgroup = [| |]

    let print = function
      | A -> "A"
      | B -> "B"

  end

module ToddTest = ToddCoxeter(Quaternion)


(* -----------------------------
   Test automorphism detection *)


let graph = 
  let g = Graph.empty in
  let g = Graph.add_node_with_colour g 1 in
  let g = Graph.add_node_with_colour g 0 in
  let g = Graph.add_node_with_colour g 0 in
  let g = Graph.add_node_with_colour g 0 in
  let g = Graph.add_edge g 0 "" 1 in
  let g = Graph.add_edge g 1 "" 2 in
  let g = Graph.add_edge g 2 "" 3 in
  Graph.add_edge g 3 "" 0

let automorphisms = Auto.compute_automorphisms graph

let _ = List.iter Auto.print (Prelude.filter_duplicates automorphisms)
  
"
