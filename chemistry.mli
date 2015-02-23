
module Link :
  sig
    type t = Simple | Double | Triple
    val compare : t -> t -> int
    val inhabited : t
    val print : t -> string
  end

module Atom :
  sig
    type atom = C | H | O | P
    type t = { atom : atom; arity : int; }
    val compare : t -> t -> int
    val inhabited : t
    val print : t -> string
  end

module Molecule :
  sig
    type t = (Atom.t, Link.t) Graph.t

    val empty                : t
    val add_node_with_colour : t -> Atom.t -> t * Graph.vertex
    val add_edge             : t -> Graph.vertex -> Link.t -> Graph.vertex -> t
    val to_dot               : string -> string -> t -> (Atom.t -> Graph.vertex -> string) -> unit

    val disjoint_union       : t -> t -> t
    val print                : t -> unit
  end

type reaction = {
  input   : Molecule.t;
  output  : Molecule.t;
  mapping : (Graph.vertex * Graph.vertex) list;
}

val to_smiles : Molecule.t -> string option

val typeof : (Atom.t, Link.t) Graph.info -> Atom.t

val enumerate : Molecule.t -> Molecule.t Growable.mset -> (Molecule.t -> unit) -> unit
