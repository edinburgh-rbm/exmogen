(* Growable structures. This abstracts aways the details of how the structures are encoded,
   and deals with enumeration of graph-like structures in a nice high-level way. 

   We assume that the growable structure handles automorhism detection and everything 
   nicely and silently. TODO - that's not really the place for that. *)

module type GrowableType =
  sig

    type t
    
    type plug

    val extend  : t -> plug list

    val saturate : t -> plug list list

    val merge : t -> plug -> plug -> t -> t
      
    val compatible : plug -> plug -> bool

    val print : t -> unit

    val print_plug : plug -> string

  end

module Enumerate
  (G : GrowableType) 
  (C : Canon.Canonicalizable with type t = G.t)
  =
  struct

    module Canonical = Canon.Make(C)
      
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
      Prelude.to_sseq G.print_plug ", " s

    (* (\* Implementation of semi-imperative multisets. *\) *)

    type 'a imset = {
      keys  : 'a  array;
      count : int array
    }

    let mset_copy { keys; count } =
      { keys; count = Array.copy count }

    let mset_eq { count = count1 } { count = count2 } =
      count1 = count2

    let mset_decr i mset =
      let m = mset_copy mset in
      m.count.(i) <- m.count.(i) - 1;
      m

    (* multisets -- we use a list to increase sharing *)
    type 'a mset = ('a * int) list


    (* let mset_max { keys; count } m2 = *)
    (*   { m2 with *)
    (*    count = Array.init (Array.length count) (fun i -> max count.(i)  m2.count.(i)) *)
    (*   } *)

    let canonicalize : (G.t * C.canonical * G.t mset) list -> (G.t * C.canonical * G.t mset) list =
      fun metacc ->
        Prelude.sort_uniq (fun (_, g1, _) (_, g2, _) ->
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
        (acc : Canonical.t) 
        =
      let plugs = G.saturate seed in
      match plugs with
      | [] -> failwith "Growable.enumerate: impossible saturation"
      | [[]] ->
        (* No more growing opportunities: the tree is complete. *)
        Canonical.add seed acc
      | _ ->
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
  

    (* let rec enumerate_augmentations *)
    (*     (seed : G.t) *)
    (*     (thread : G.plug list) *)
    (*     (patterns : G.t mset) *)
    (*     (acc : Canonical.t) *)
    (*     = *)
    (*   match thread with *)
    (*   | [] -> *)
    (*     enumerate seed patterns acc *)
    (*   | wplug :: wtail -> *)
    (*     pick_one_of_each_class patterns [] (fun acc patt patterns' -> *)
    (*       let pplugs   = G.extend patt in *)
    (*       let branches = pullback [wplug] pplugs in (\* There should be pretty much only one branch *\) *)
    (*       match branches with *)
    (*       | [] -> *)
    (*         acc *)
    (*       | _ -> *)
    (*         List.fold_left (fun acc (wp, pp) -> *)
    (*           let extended_graph = G.merge seed wp pp patt in *)
    (*           enumerate_augmentations extended_graph wtail patterns' acc *)
    (*         ) acc branches *)
    (*     ) acc *)

    (* and enumerate  *)
    (*     (seed : G.t)  *)
    (*     (patterns : G.t mset)  *)
    (*     (acc : Canonical.t)  *)
    (*     = *)
    (*   let plugs = G.saturate seed in *)
    (*   match plugs with *)
    (*   | [] -> failwith "Growable.enumerate: impossible saturation" *)
    (*   | [[]] -> *)
    (*     (\* No more growing opportunities: the tree is complete. *\) *)
    (*     Canonical.add seed acc *)
    (*   | _ -> *)
    (*     (match patterns with *)
    (*     | [] -> *)
    (*       (\* let _ = Printf.fprintf stderr "warning: empty multiset of patterns but molecule still open\n%!" in *\) *)
    (*       (\* seed :: acc *\) *)
    (*       acc *)
    (*     | _ -> *)
    (*       (\* TODO check size of patterns vs. |x| where x \in plugs *\) *)
    (*       List.fold_left (fun acc thread -> *)
    (*         enumerate_augmentations seed thread patterns acc *)
    (*       ) acc plugs *)
    (*     ) *)
  

  end


(*
    let rec enumerate_augmentations 
        (seed : G.t) 
        (thread : G.plug list) 
        (patterns : G.t mset) 
        (acc : Canonical.t)
        (augs : Canonical.t)
        =
      match thread with
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
              let extended_graph = G.merge seed wp pp patt in
              match Canonical.test_and_set extended_graph augs with
              | None ->
                (* The proposed augmentation is isomorphic to another one at the same level. *)
                acc                
              | Some augs' ->
                enumerate_augmentations extended_graph wtail patterns' acc augs'
            ) acc branches
        ) acc

    and enumerate 
        (seed : G.t) 
        (patterns : G.t mset) 
        (acc : Canonical.t) 
        =
      let plugs = G.saturate seed in
      match plugs with
      | [] -> failwith "Growable.enumerate: impossible saturation"
      | [[]] ->
        (* No more growing opportunities: the tree is complete. *)
        Canonical.add seed acc
      | _ ->
        (match patterns with
        | [] ->
          (* let _ = Printf.fprintf stderr "warning: empty multiset of patterns but molecule still open\n%!" in *)
          (* seed :: acc *)
          acc
        | _ ->
          (* TODO check size of patterns vs. |x| where x \in plugs *)
          List.fold_left (fun acc thread ->
            enumerate_augmentations seed thread patterns acc
          ) acc plugs
        )
*)    
