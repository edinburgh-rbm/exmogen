module type CanonicalizableType =
  sig
    type t
    type canonical
    val canonical : t -> canonical
    val compare : canonical -> canonical -> int
    val print : t -> string
  end

module Make :
  functor (C : CanonicalizableType) ->
    sig
      type t
      val empty : t
      val add : C.t -> t -> t
      val elements : t -> C.t list
      val mem : C.t -> t -> bool
      val test_and_set : C.t -> t -> t option
      val fold : (C.t * C.canonical -> 'a -> 'a) -> t -> 'a -> 'a
      val card : t -> int
      val iter : (C.t * C.canonical -> unit) -> t -> unit
    end
