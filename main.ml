
module Link =
  struct

    type t = Simple | Double | Triple

    let compare (x : t) (y : t) = compare x y

    let inhabited = Simple

    let print = function
      | Simple -> "simple"
      | Double -> "double"
      | Triple -> "triple"

  end

module Atom =
  struct

    type t = C | H | O | P

    let compare (x : t) (y : t) = compare x y

    let inhabited = C

    let print = function
      | C -> "C"
      | H -> "H"
      | O -> "O"
      | P -> "P"

  end

module Gram =
  struct
    
    type nc = Atom.t

    type lc = Link.t

    open Link
    open Atom

    let rec electrons acc = function
      | [] -> acc
      | a :: tl ->
        match a with
        | Simple -> electrons (1 + acc) tl
        | Double -> electrons (2 + acc) tl
        | Triple -> electrons (3 + acc) tl

    let growth_policy nc llc =
      let e = electrons 0 llc in
      let c = match nc with
        | C -> 4 - e
        | H -> 1 - e
        | O -> 2 - e
        | P -> 1 - e
      in match c with
      | 0 -> []
      | 1 -> [Simple]
      | 2 -> [Simple; Double]
      | 3 -> [Simple; Double; Triple]
      | 4 -> [Simple; Double; Triple]
      | _ -> failwith "inconsistent stuff happening"

    let compatibility _ _ _ = true

  end


module Unrooted = UnrootedTree.Make(Atom)(Link)(Gram)

module Canon =
  struct

    type t = Unrooted.t

    type canonical = Unrooted.R.Encoded.t

    let canonical t =
      snd (Unrooted.canonical_root t)

    let compare x y = Unrooted.R.Encoded.tree_compare x y

  end

module G = Growable.Enumerate(Unrooted)(Canon)

(* start from a single carbon *)
let seed =
  let g = Unrooted.empty in
  Unrooted.add_node_with_colour g Atom.C


(* carbon pattern *)
let carbon =
  let g = Unrooted.empty in
  Unrooted.add_node_with_colour g Atom.C

(* hydrogen pattern *)
let hydrogen =
  let g = Unrooted.empty in
  Unrooted.add_node_with_colour g Atom.H

(* oxygen pattern *)
let oxygen =
  let g = Unrooted.empty in
  Unrooted.add_node_with_colour g Atom.O

(* phosphate pattern *)
let oxygen =
  let g = Unrooted.empty in
  Unrooted.add_node_with_colour g Atom.P

(* Allow 4 hydrogens to bind *)
(*
let mset =
  [ (hydrogen, 4) ]
*)

(* Allow 1 hydrogen to bind  *)
(*
let mset =
  [ (hydrogen, 1) ]
*)

(* Allow 3 carbons and 8 hydrogens to bind  *)
let mset =
  [ (carbon, 4); (hydrogen, 8) ]

(*

let result = G.enumerate seed mset G.Canonical.empty

let result = G.Canonical.elements result

let s = List.map (fun (res, _) ->  Unrooted.R.print (Unrooted.root res 0)) result

let _ = List.iter (Printf.printf "%s\n" ) s

*)


(* Testing group theoretic stuff *)

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


(*

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
*)
