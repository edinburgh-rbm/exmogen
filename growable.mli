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
type 'a mset = ('a * int) list
module Enumerate :
  functor
    (G : GrowableType) (C : sig
                              type t = G.t
                              type canonical
                              val canonical : t -> canonical
                              val compare : canonical -> canonical -> int
                              val print : t -> string
                            end) ->
    sig
      module Canonical :
        sig
          type t = CanonicalSet.Make(C).t
          val empty : t
          val add : C.t -> t -> t
          val elements : t -> C.t list
          val mem : C.t -> t -> bool
          val test_and_set : C.t -> t -> t option
          val fold : (C.t * C.canonical -> 'a -> 'a) -> t -> 'a -> 'a
          val card : t -> int
          val iter : (C.t * C.canonical -> unit) -> t -> unit
        end
      val pullback_elt :
        G.plug list ->
        G.plug -> (G.plug * G.plug) list -> (G.plug * G.plug) list
      val pullback_aux :
        G.plug list ->
        G.plug list -> (G.plug * G.plug) list -> (G.plug * G.plug) list
      val pullback : G.plug list -> G.plug list -> (G.plug * G.plug) list
      val pick_one_of_each_class :
        ('a * int) list ->
        ('a * int) list -> ('b -> 'a -> ('a * int) list -> 'b) -> 'b -> 'b
      val print_section_dbg : G.plug list -> string
      val canonicalize :
        (G.t * C.canonical * G.t mset) list ->
        (G.t * C.canonical * G.t mset) list
      val enumerate_augmentations :
        G.t ->
        G.plug list ->
        G.t mset ->
        (G.t * C.canonical * G.t mset) list ->
        (G.t * C.canonical * G.t mset) list
      val count : int ref
      val log : 'a -> unit
      val enumerate :
        G.t -> G.t mset -> Canonical.t * int -> Canonical.t * int
    end
