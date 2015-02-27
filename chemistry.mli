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
    type t  = (Atom.t, Link.t) Graph.t
    type t' = t

    val empty                : t
    val add_node_with_colour : t -> Atom.t -> t * Graph.vertex
    val add_edge             : t -> Graph.vertex -> Link.t -> Graph.vertex -> t
    val to_dot               : string -> string -> t -> (Atom.t -> Graph.vertex -> string) -> unit

    val disjoint_union       : t -> t -> t
    val print                : t -> unit

    module Canonical : 
      (CanonicalSet.CanonicalizableType
       with type t = t')

    module Growable :
      (Growable.GrowableType
       with type t = t')

  end

module CanonicalSet : 
  (CanonicalSet.CanonicalSetType
   with type elt       = Molecule.t
   and type  canonical = Molecule.Canonical.canonical)


val carbon    : Molecule.t
val hydrogen  : Molecule.t
val oxygen    : Molecule.t
val phosphate : Molecule.t

(* A reaction scheme is given by a reaction input, a reaction output and
   an (injective) mapping from input stubs to output stubs. If everything
   is to make sense, the mapping should in fact be bijective. *)
type nontrivial_reactants =
  { input   : Molecule.t;
    output  : Molecule.t;
    mapping : (Graph.vertex * Graph.vertex) list
  }

type reaction = 
  { nontrivial_reactants : nontrivial_reactants option;
    trivial_inputs       : string list;
    trivial_outputs      : string list
  }

val to_smiles : Molecule.t -> string option

val molecule_to_graphs : Reactions.smiles_ast -> (Molecule.t * Graph.vertex * ((string * Graph.vertex) list)) list

val check_arity : Molecule.t -> bool

val typeof : (Atom.t, Link.t) Graph.info -> Atom.t

val enumerate : Molecule.t -> Molecule.t Prelude.mset -> (Molecule.t -> unit) -> unit
  
(* val instantiate_schemes : *)
(*   reaction list -> *)
(*   Molecule.Growable.t Prelude.mset -> (reaction -> unit) -> unit *)

 val instantiate_reactions :
  Reactions.reactions ->
  Molecule.Growable.t Prelude.mset -> (string -> unit) -> unit
  
