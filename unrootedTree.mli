module type GrammarType =
  sig
    type nc
    type lc
    val extension_policy : nc -> lc list -> lc list
    val saturation_policy : nc -> lc list -> lc list list
    val compatibility : nc -> lc -> nc -> bool
  end

module Make :
  functor
    (NLab : Prelude.Ordered) 
    (LLab : Prelude.Ordered) 
    (Gram : GrammarType
     with type nc = NLab.t
     and  type lc = LLab.t) ->

sig

  type t          = (NLab.t, LLab.t) Graph.t
  type t'         = t

  val empty : t
  val add_node_with_colour : t -> NLab.t -> t * Graph.vertex
  val add_edge             : t -> Graph.vertex -> LLab.t -> Graph.vertex -> t
  val fold                 : (Graph.vertex -> NLab.t -> (LLab.t * Graph.vertex) list -> 'c -> 'c) -> t -> 'c -> 'c

  val get_colour           : t -> Graph.vertex -> NLab.t

  val to_dot               : string -> string -> t -> (NLab.t -> Graph.vertex -> string) -> unit
  val disjoint_union       : t -> t -> t
  val print                : t -> unit

  module Canonical : 
    (CanonicalSet.CanonicalizableType
     with type t = t')

  module Growable :
    (Growable.GrowableType
     with type t = t')

end

