(* Growable structures. This abstracts aways the details of how the structures are encoded,
   and deals with enumeration of graph-like structures in a nice high-level way. 

   We assume that the growable structure handles automorhism detection and everything 
   nicely and silently. TODO - that's not really the place for that. *)

module type GrowableType =
  sig
    type t
    type plug
    type saturation_outcome =
        Alternatives of plug list list
      | Rejected
      | Finished
    val extend : t -> plug list
    val saturate : t -> saturation_outcome
    val merge : t -> plug -> plug -> t -> t
    val compatible : plug -> plug -> bool
    val print : t -> unit
    val print_plug : plug -> string
  end

module Enumerate :
  functor
    (G : GrowableType) 
    (C : CanonicalSet.CanonicalizableType with type t = G.t) ->
    sig

      (* Set of G.t up to equivalence. *)
      module CanonicalSet : 
        (CanonicalSet.CanonicalSetType
         with type elt       = C.t
         and  type canonical = C.canonical)

      val enumerate :
        G.t -> G.t Prelude.mset -> CanonicalSet.t * int -> CanonicalSet.t * int

    end
      
