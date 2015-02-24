module type CanonicalizableType =
sig
  type t
  type canonical
  val canonical : t -> canonical
  val compare : canonical -> canonical -> int
  val print : t -> string
end

module type CanonicalSetType =
sig
  type t
  type elt
  type canonical

  val empty : t
  val add : elt -> t -> t
  val elements : t -> elt list
  val mem : elt -> t -> bool
  val fold : (elt * canonical -> 'a -> 'a) -> t -> 'a -> 'a
  val card : t -> int
  val iter : (elt * canonical -> unit) -> t -> unit
  val choose : t -> elt
end

module Make : 
  functor 
    (C : CanonicalizableType) -> 
      (CanonicalSetType 
       with type elt = C.t
       and type canonical = C.canonical)



(* module Make : *)
(*   functor (C : CanonicalizableType) -> SetType *)
(*     sig *)
(*       type t *)
(*       val empty : t *)
(*       val add : C.t -> t -> t *)
(*       val elements : t -> C.t list *)
(*       val mem : C.t -> t -> bool *)
(*       val fold : (C.t * C.canonical -> 'a -> 'a) -> t -> 'a -> 'a *)
(*       val card : t -> int *)
(*       val iter : (C.t * C.canonical -> unit) -> t -> unit *)
(*     end *)
