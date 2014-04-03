(* Growable structures. This abstracts aways the details of how the structures are encoded,
   and deals with enumeration of graph-like structures in a nice high-level way. 

   We assume that the growable structure handles automorhism detection and everything 
   nicely and silently. *)

module type GrowableType =
  sig

    type t
    
    type plug

    type canonical

    val extend  : t -> plug list

    val saturate : t -> plug list list

    val merge : t -> plug -> plug -> t -> t
      
    val compatible : plug -> plug -> bool

    val print : t -> unit

    val print_plug : plug -> string

  end

module type Canonicalizable =
  sig

    type t

    type canonical


    val canonical : t -> canonical

    val compare : canonical -> canonical -> int

  end

module Enumerate
  (G : GrowableType) 
  (C : Canonicalizable with type t = G.t)
  =
  struct

    (* Set of canonical solutions *)
    module Canonical = Set.Make(
      struct
        type t = (G.t * C.canonical)
          
        let compare (_, x) (_, y) = C.compare x y
      end)

    (* we want to compute the set pullback of lists for the co-span defined by G.compatible *)
    let rec pullback_elt l1 patt2 acc =
      match l1 with
      | []      -> acc
      | x :: tl ->
        if G.compatible x patt2 then
          pullback_elt tl patt2 ((x, patt2) :: acc)
        else
          pullback_elt tl patt2 acc

    let rec pullback_aux l1 l2 acc =
      match l2 with
      | [] -> acc
      | x :: tl ->
        pullback_aux l1 tl (pullback_elt l1 x acc)

    let pullback l1 l2 = pullback_aux l1 l2 []

    (* This only enumerate trees. *)
    (* let rec enumerate (seed : G.t) (patterns : G.t list) (acc : G.t list) = *)
    (*   let plugs = G.propose seed in *)
    (*   match plugs with *)
    (*   | [] -> *)
    (*     (\* No more growing opportunities: the tree is complete. *\) *)
    (*     seed :: acc *)
    (*   | _ -> *)
    (*     List.fold_left (fun acc patt -> *)
    (*       let pplugs = G.propose patt in *)
    (*       let branches = pullback plugs pplugs in *)
    (*       List.fold_left (fun acc (p, pp) -> *)
    (*         enumerate (G.merge seed p pp patt) patterns acc *)
    (*       ) acc branches *)
    (*     ) acc patterns *)

    (* multisets -- we use a list to increase sharing *)
    type 'a mset = ('a * int) list

    let rec pick_one_of_each_class mset mset_acc f acc =
      match mset with
      | [] -> acc
      | ((elt, c) as x) :: tail ->
        if c = 0 then
          pick_one_of_each_class tail mset_acc f acc
        else if c = 1 then
          pick_one_of_each_class tail (x :: mset_acc) f (f acc elt (mset_acc @ tail))
        else
          let acc' = f acc elt ((elt, c - 1) :: mset_acc @ tail) in
          pick_one_of_each_class tail (x :: mset_acc) f acc'

    (* enumerate and consume elements in pattern *)
(*
    let rec enumerate_old (seed : G.t) (patterns : G.t mset) (acc : Canonical.t) =
      let plugs = G.propose seed in
      match plugs with
      | [] ->
        (* No more growing opportunities: the tree is complete. *)
        Canonical.add (seed, C.canonical seed) acc
      | _ ->
        (match patterns with
        | [] ->
          (*let _ = Printf.fprintf stderr "warning: empty multiset of patterns but molecule still open\n%!" in*)
          (* seed :: acc *)
          acc
        | _ ->
          pick_one_of_each_class patterns [] (fun acc patt patterns' ->
            let pplugs = G.propose patt in
            let branches = pullback plugs pplugs in
            List.fold_left (fun acc (p, pp) ->
              enumerate_old (G.merge seed p pp patt) patterns' acc
            ) acc branches
          ) acc
        ) *)
  (* List.fold_left (fun acc patt -> *)
  (*   let pplugs = G.propose patt in *)
  (*   let branches = pullback plugs pplugs in *)
  (*   List.fold_left (fun acc (p, pp) -> *)
  (*     enumerate (G.merge seed p pp patt) patterns acc *)
  (*   ) acc branches *)
  (* ) acc patterns *)

    let print_section_dbg s =
      Prelude.to_sseq G.print_plug ", " s
            
    let rec enumerate_augmentations (seed : G.t) (word : G.plug list) (patterns : G.t mset) (acc : Canonical.t) =
      match word with
      | [] ->
        enumerate seed patterns acc
      | wplug :: wtail ->
        pick_one_of_each_class patterns [] (fun acc patt patterns' ->
          let pplugs   = G.extend patt in
          let branches = pullback [wplug] pplugs in (* There should be pretty much only one branch *)
          match branches with
          | [] ->
            acc
          | _ ->
            List.fold_left (fun acc (wp, pp) ->
              enumerate_augmentations (G.merge seed wp pp patt) wtail patterns' acc
            ) acc branches
        ) acc

    and enumerate (seed : G.t) (patterns : G.t mset) (acc : Canonical.t) =
      let plugs = G.saturate seed in
      match plugs with
      | [] -> failwith "Growable.enumerate: impossible saturation"
      | [[]] ->
        (* No more growing opportunities: the tree is complete. *)
        Canonical.add (seed, C.canonical seed) acc
      | _ ->        
        (match patterns with
        | [] ->
          (* let _ = Printf.fprintf stderr "warning: empty multiset of patterns but molecule still open\n%!" in *)
          (* seed :: acc *)
          acc
        | _ ->
          (* TODO check size of patterns vs. |x| where x \in plugs *)
          List.fold_left (fun acc word ->
            enumerate_augmentations seed word patterns acc
          ) acc plugs
        )
  

  end
