open Prelude

module Make(Concrete : Perm.PermSig) =
struct

  (* perm words *)
  module Perm = Perm.Make(Perm.CycleBased)

  (* ----------------------- *)
  (* Schreier-sims algorithm *)

  (* Record for the stabilizer subgroup of [elt] *)
  type subgroup = {
    elt         : int;            (* elt is stabilised by the subgroup *)
    generators  : Perm.t list;    (* Generators of G^[i] *)
    transversal : Perm.t IntMap.t (* the orbit is the domain of the map, the codomain are the coset repr. *)
  }

  type group = {
    base      : int list;
    chain     : subgroup list
  }
    
  let rec strip subgroups perm =
    match subgroups with
    | [] -> true
    | { elt; generators; transversal } :: subgroups ->
      let im = Perm.action perm elt in
      (IntMap.mem im transversal) && (strip subgroups perm)

  (* A partial base and strong generating set (bsgs) is
     a set of generators containing the original set of
     generators of the group and s.t. no point of the
     base is fixed. So given a base and a preliminary
     set of elements (pre_bsgs), we want to extend it to a
     partial bsgs by adding points to the base, and possibly
     by closing [pre_bsgs].

     Note: in the following function, we assume that [pre_bsgs]
     already contains the generators for the group.
  *)

  let partial_bsgs
      (pre_bsgs : Concrete.t list)
      (base : int list) =
    List.fold_left (fun (base, pre_bsgs) perm  ->
      let pre_bsgs =
        if Concrete.prod perm perm <> Concrete.identity then
          (Concrete.inv perm) :: pre_bsgs
        else pre_bsgs
      in
      let base' = List.map (Concrete.action perm) base in
      if base = base' then
        match Concrete.pick_from_support perm with
        | None -> failwith "Bsgs.partial_bsgs: pre_bsgs contains the identity perm."
        | Some pt ->
          (pt :: base, pre_bsgs)
      else
        (base, pre_bsgs)
    ) (base, pre_bsgs) pre_bsgs
  
  (* Sifting, i.e. testing an element for inclusion in a group with
     strong generators *)

  let rec sift_aux base_im chain =
    match base_im, chain with
    | [], [] -> true
    | [], _
    | _, [] -> failwith "inconsistent chain"
    | x :: base', subgroup :: chain' ->
      if IntMap.mem x subgroup.transversal then
        sift_aux base' chain'
      else
        false

  let sift group perm =
    let base' = List.map (Perm.action perm) group.base in
    sift_aux base' group.chain

  (* Schreier-Sims algorithm (from scratch) *)

  exception EarlyExit of int

  (* A nice invariant of our repesentation of permutations is that identity
     mappings are not stored. Hence any binding will do. *)
  let rec find_point (generators : Perm.t list) =
    match generators with
    | [] -> None
    | (Perm.Perm g) :: gens ->    
      if IntMap.is_empty g.Perm.p then
        find_point gens
      else
        Some (fst (IntMap.choose g.Perm.p))
    | _ -> failwith "Group.find_point: generators not in normal form"

  open Perm.Operators

  (* It is assumed that the generators are normalised. *)
  (* let rec schreier_sims_aux (generators : Perm.t list) acc = *)
  (*   (\* Find a point that is not fixed by a generator *\) *)
  (*   match find_point generators with *)
  (*   | None -> (\* trivial group - end recursion *\) *)
  (*     let points, subgroups = List.split acc in *)
  (*     { base  = List.rev points; *)
  (*       chain = List.rev subgroups } *)
  (*   | Some point -> *)
  (*     let orb  = Perm.orbit generators [point] in *)
  (*     let gens = (\* Schreier generators for isotropy subgroup *\) *)
  (*       IntMap.fold (fun point u acc -> *)
  (*         List.fold_left (fun acc g -> *)
  (*           let ug    = u *** g in *)
  (*           let ug_tr = *)
  (*             let img = Perm.action ug point in *)
  (*             try IntMap.find img orb with *)
  (*             | Not_found -> failwith "Group.schreier_sims: bug found, could not find transversal" *)
  (*           in *)
  (*           (\* TODO: normalise these ... OR normalise as needed in [find_point] *\) *)
  (*           (ug *** (Perm.Inv ug_tr)) :: acc *)
  (*         ) acc generators *)
  (*       ) orb [] in *)
  (*     let subgroup = { generators = gens;  transversal = orb } in *)
  (*     schreier_sims_aux gens ((point, subgroup) :: acc) *)

  (* let schreier_sims generators = schreier_sims_aux generators [] *)

end



module Make2(Concrete : Perm.PermSig) =
struct

  (* perm words *)
  module Perm = Perm.Make(Perm.CycleBased)

  (* ----------------------- *)
  (* Schreier-sims algorithm *)

  (* Record for the stabilizer subgroup of [elt] *)
  type subgroup = {
    elt         : int;            (* elt is stabilised by the subgroup *)
    generators  : Perm.t list;    (* Generators of G^[i] *)
    transversal : Perm.t IntMap.t (* the orbit is the domain of the map, the codomain are the coset repr. *)
  }

  type group = {
    base      : int array;
    chain     : subgroup array
  }

  type sift_outcome =
  | Ok
  | DropOut of int * Perm.t

  open Perm.Operators
    
  let rec strip_aux group perm i =
    if i = Array.length group.chain then
      Ok
    else
      let { elt; transversal } = group.chain.(i) in
      let im = Perm.action perm elt in
      match IntMap.find_opt im transversal with
      | None ->
        DropOut(i, perm)
      | Some repr ->
        strip_aux group ((Perm.invert repr) *** perm) (i+1)

  let strip group perm = strip_aux group perm 0

  (* A partial base and strong generating set (bsgs) is
     a set of generators containing the original set of
     generators of the group and s.t. no point of the
     base is fixed. So given a base and a preliminary
     set of elements (pre_bsgs), we want to extend it to a
     partial bsgs by adding points to the base, and
     by closing [pre_bsgs] with inverses.

     Note: in the following function, we assume that [pre_bsgs]
     already contains the generators for the group as well as
     their inverses, but /not/ the identity. *)

  let rec check_stability_of_base perm base =
    match base with
    | [] -> false
    | b :: base' ->
      (Concrete.action perm b <> b)
      || (check_stability_of_base perm base')

  let partial_bsgs
      (pre_bsgs : Concrete.t list)
      (base : int list)  (* We assume that [base] is given in reverse order *)
      = 
    List.fold_right (fun perm (base, pre_bsgs) ->
      let pre_bsgs =
        if Concrete.prod perm perm <> Concrete.identity then
          (Concrete.inv perm) :: pre_bsgs
        else pre_bsgs
      in
      if check_stability_of_base perm base then
        match Concrete.pick_from_support perm with
        | None -> failwith "Bsgs.partial_bsgs: pre_bsgs contains the identity perm."
        | Some pt ->
          (pt :: base, pre_bsgs)
      else
        (base, pre_bsgs)
    ) pre_bsgs (base, pre_bsgs)

  (* Standard Schreier-Sims. *)
  (* let rec schreier_sims_iteration *)


end
