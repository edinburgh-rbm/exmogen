(* From an arbitrary canonicalizable data structure,
   we produce a data structure storing equivalence classes
   of elememts.

*)

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


module Make (C : CanonicalizableType) =
  struct

    (* Set of C.t up to canonical equivalence *)
    module S = Set.Make(
      struct
        type t = (C.t * C.canonical)
          
        let compare (_, x) (_, y) = C.compare x y
      end)

    type t         = S.t
    type elt       = C.t
    type canonical = C.canonical

    let empty = S.empty

    let add elt set =
      S.add (elt, C.canonical elt) set

    let elements set = List.rev_map fst (S.elements set)
      
    let mem elt set =
      S.mem (elt, C.canonical elt) set

    let fold = S.fold

    let card = S.cardinal

    let iter = S.iter

    let choose (x : t) = fst (S.choose x)

  end
