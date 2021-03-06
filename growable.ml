open Prelude

(* Growable structures. This abstracts aways the details of how the structures are encoded,
   and deals with enumeration of graph-like structures in a nice high-level way. 

   We assume that the growable structure handles automorhism detection and everything 
   nicely and silently. TODO - that's not really the place for that. *)

module type GrowableType =
  sig

    type t
    
    type plug

    type saturation_outcome =
    | Alternatives of plug list list
    | Rejected
    | Finished

    val extend  : t -> plug list

    val saturate : t -> saturation_outcome

    val merge : t -> plug -> plug -> t -> t
      
    val compatible : plug -> plug -> bool

    val print : t -> unit
      
    val print_plug : plug -> string

  end

module Enumerate
  (G : GrowableType) 
  (C : CanonicalSet.CanonicalizableType with type t = G.t)
  =
  struct

    module CanonicalSet = CanonicalSet.Make(C)
      
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

    let print_section_dbg s =
      to_sseq G.print_plug ", " s

    let canonicalize : (G.t * C.canonical * G.t mset) list -> (G.t * C.canonical * G.t mset) list =
      fun metacc ->
        sort_uniq (fun (_, g1, _) (_, g2, _) ->
          C.compare g1 g2
        ) metacc
        

    let rec enumerate_augmentations
        (seed : G.t)
        (thread : G.plug list)
        (patterns : G.t mset)
        (metacc : (G.t * C.canonical * G.t mset) list)
        =
      match thread with
      | [] ->
        (seed, C.canonical seed, patterns) :: metacc
      | wplug :: wtail ->
        pick_one_of_each_class patterns [] (fun metacc patt patterns' ->
          let pplugs   = G.extend patt in
          let branches = pullback [wplug] pplugs in (* There should be pretty much only one branch *)
          match branches with
          | [] ->
            metacc
          | _ ->
            List.fold_left (fun metacc (wp, pp) ->
              let extended_graph = G.merge seed wp pp patt in
              enumerate_augmentations extended_graph wtail patterns' metacc
            ) metacc branches
        ) metacc

    let rec enumerate 
        (seed : G.t) 
        (patterns : G.t mset) 
        (((canon, card) as acc) : CanonicalSet.t * int)
        =
      let saturation = G.saturate seed in
      match saturation with
      | G.Rejected -> acc
      | G.Finished ->
        (* No more growing opportunities: the tree is complete. *)
        if CanonicalSet.mem seed canon then
          acc
        else
          (CanonicalSet.add seed canon, card+1)
      | G.Alternatives plugs ->
        (match patterns with
        | [] ->
          (* let _ = Printf.fprintf stderr "warning: empty multiset of patterns but molecule still open\n%!" in *)
          (* seed :: acc *)
          acc
        | _ ->
          (* TODO check size of patterns vs. |x| where x \in plugs *)
          List.fold_left (fun acc thread ->
            let alternatives = canonicalize (enumerate_augmentations seed thread patterns []) in
            List.fold_left (fun acc (elt, _, mset) ->
              enumerate elt mset acc
            ) acc alternatives
          ) acc plugs
        )
  
  

  end

