open Prelude


module Perm = Perm.Make(Perm.CycleBased)


(* ----------------------- *)
(* Schreier-sims algorithm *)

type subgroup = {
  generators  : Perm.t list;    (* Generators of G^[i] *)
  transversal : Perm.t IntMap.t (* the orbit is the domain of the map *)
}

type group = {
  base      : int list;
  chain     : subgroup list
}

(* Sifting, i.e. testing an element for inclusion in a group with
   strong generators *)

let rec sift_aux base_im chain =
  match base_im, chain with
  | [], [] -> true
  | [], _
  | _, [] -> failwith "inconsistent chain"
  | x :: base', subgroup :: chain' ->
    (IntMap.mem x subgroup.transversal) && sift_aux base' chain'

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
let rec schreier_sims_aux (generators : Perm.t list) acc =
  (* Find a point that is not fixed by a generator *)
  match find_point generators with
  | None -> (* trivial group - end recursion *)
    let points, subgroups = List.split acc in
    { base  = List.rev points;
      chain = List.rev subgroups }
  | Some point ->
    let orb  = Perm.orbit generators [point] in
    let gens = (* Schreier generators for isotropy subgroup *)
      IntMap.fold (fun point u acc ->
        List.fold_left (fun acc g ->
          let ug    = u *** g in
          let ug_tr =
            let img = Perm.action ug point in
            try IntMap.find img orb with
            | Not_found -> failwith "Group.schreier_sims: bug found, could not find transversal"
          in
          (* TODO: normalise these ... OR normalise as needed in [find_point] *)
          (ug *** (Perm.Inv ug_tr)) :: acc
        ) acc generators
      ) orb [] in
    let subgroup = { generators = gens;  transversal = orb } in
    schreier_sims_aux gens ((point, subgroup) :: acc)

let schreier_sims generators = schreier_sims_aux generators []
