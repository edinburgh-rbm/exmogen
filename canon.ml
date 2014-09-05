module type Canonicalizable =
  sig

    type t

    type canonical

    val canonical : t -> canonical

    val compare : canonical -> canonical -> int

  end

module Make (C : Canonicalizable) =
  struct

    (* Set of canonical solutions *)
    module Canonical = Set.Make(
      struct
        type t = (C.t * C.canonical)
          
        let compare (_, x) (_, y) = C.compare x y
      end)

    type t = Canonical.t

    let empty = Canonical.empty

    let add elt set =
      Canonical.add (elt, C.canonical elt) set

    let elements set = List.rev_map fst (Canonical.elements set)

    let mem elt set =
      Canonical.mem (elt, C.canonical elt) set

    let test_and_set elt set =
      let x = (elt, C.canonical elt) in
      if Canonical.mem x set then
        None
      else
        Some (Canonical.add x set)

    let fold = Canonical.fold

    let card = Canonical.cardinal

  end
