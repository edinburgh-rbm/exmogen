open Prelude

(* Todd-Coxeter algorithm *)

module type FinitelyPresentedGroup =
sig
  (* An abstract type of generators for the group. *)
  type generator

  (* A set of generators. They /must/ be distinct. Formal inverses /must/ be present. *)
  val  generators : generator array

  (* Give the inverse of a generator. Of course, this should be involutive. *)
  val inverse : generator -> generator

  (* A set of relators. They *should* be distinct,
   * but the algorithm will of course work with syntactically equal relators. *)
  val  relators   : (generator list) array
end

module ToddCoxeter(G : FinitelyPresentedGroup) =
struct
  
  (* We use ints to represent group elements. Easier to handle. So we have a first phase of
   * translation from the abstract representation to integers. *)
  type elt = int (* concrete group element *)
  type gen = int (* concrete generator *)

  (* Tables are implemented by hashtables. *)
  type table       = ((elt * gen), elt) Hashtbl.t
  type group_table = ((elt * elt), elt) Hashtbl.t

  (* It's hard to estimate the order of a group from its presentation. We go with an
   * initial guess that matches the target applications ... *)
  let order_guess = 1000

  (* Some standard wrappers on top of Hashtbl *)
  let table_create : unit -> table = 
    fun () -> Hashtbl.create order_guess

  let table_get table e1 e2 =
    try Some (Hashtbl.find table (e1, e2)) with
    | Not_found -> None

  let table_mem table e1 e2 =
    Hashtbl.mem table (e1, e2)

  let table_remove table e1 e2 =
    Hashtbl.remove table (e1, e2)

  (* /!\ No mem-check is performed before overwriting! *)
  let table_set table e1 e2 x =
    Hashtbl.add table (e1, e2) x

  (* -------------------------------------------------------------- *)
  (* Initialisation *)
  (* -------------------------------------------------------------- *)

  (* Produce tables translating between concrete and abstract elements,
   * map the inverse relation on generators to concrete elements. *)
  let forward, backward, inverse =
    let forward  = Hashtbl.create (Array.length G.generators * 2) in
    let backward = Hashtbl.create (Array.length G.generators * 2) in
    let inverse  = Array.create (Array.length G.generators) 0 in
    let gen =
      let counter = ref 0 in
      fun () ->
        let v = !counter in
        incr counter;
        v
    in
    for i = 0 to Array.length G.generators - 1 do
      Hashtbl.add forward i G.generators.(i);
      Hashtbl.add backward G.generators.(i) i
    done;
    for i = 0 to Array.length G.generators - 1 do
      inverse.(i) <- Hashtbl.find backward (G.inverse G.generators.(i))
    done;
    (forward, backward, inverse)

  (* Convert relators to the conrete repr. *)
  let relators : gen array = Array.map (Dllist.map (Hashtbl.find backward)) G.relators

  (* Create the tables *)
  let link_table    = (table_create ()  : table)
  let chain_table   = (ref []           : (list (elt, gen list,e elt)) ref)
  let group_table   = (table_create ()  : group_table)

  (* Populate chains with elementary relations for group element [g] *)
  let fill_chains (g : elt) =
    for i = 0 to Array.length relators - 1 do
      chain_table := (g, relators.(i), g) :: !chain_table
    done

  let propagate () =
    let new_chains, new_links =
      let eltr   = ref 0 in
      let chainr = ref [] in
      let imager = ref 0 in
      List.fold_left (fun (chains, links) (elt, chain, image) ->
        eltr   := elt;
        chainr := chain;
        imager := image;
        cont   := true;
        while !cont do
          let first = !chainr.data in
          let last  = !chainr.prev.data in
          cont := false;
          begin match table_find link_table !eltr first with
          | None -> ()
          | Some elt' ->
            (eltr := elt';
             chainr := !chainr.next;
             Dllist.remove !chainr;
             cont := true)
          end;
          begin match table_find link_table !imager invert.(last) with
          | None -> ()
          | Some image' ->
            (imager := elt'; 
             Dllist.remove !chainr.prev;
             cont := true)
          end;
          cont := (!chainr.next <> !chainr.prev) && !cont
        done;
        if !chainr.next == !chainr.prev then
          (chains,  (!eltr, !chainer.data, !imager) :: links)
        else
          (!chainr :: chains, links)
      ) ([], []) !chain_table
    in
    chain_table := new_chains;
    new_links

  let add_links (elt, gen, image) =
    


  let todd_coxeter =
    let level = ref 0 in
    fill_links !level;
    contraction ();
    
    
    
    

end


(* Persistent perms implemented by cycles *)
module type PermSig =
sig
  val size : int
end


module Perm(P : PermSig) =
struct

  let size = P.size

  (* ------------------------------- *)
  (* Operations on real permutations *)

  module Concrete =
  struct

    (* Invariants:
       1 cycles are nonempty lists
       2 1-element cycles are omitted
    *)
    type cycle = int array

    (* A perm is a list of /disjoint/ cycles -- i.e. a set of cycles,
     * implemented as a map from elements to pairs of 
     * (cycles containing those elements, image through the permutation).
     * Sharing allows to not waste too much memory, and we compute the
     * orbit of an element in O(log(n) (the access time))
     *)
    type t = (cycle * int) IntMap.t

    (* Identity permutation *)
    let identity = IntMap.empty

    let rec image_cycle_aux first point = function
      | [] -> failwith "Perm.image_cycle_aux: bug found"
      | [x] ->
        if x = point then first
        else failwith "Perm.image_cycle_aux: bug found"
      | x :: ((y :: tl) as ytl) ->
        if x = point then
          y
        else
          (image_cycle_aux first point ytl)

    let image_cycle point cyc =
      let res = ref 0 in
      let len = Array.length cyc in
      for i = 0 to len - 1 do
        if cyc.(i) = point then
          res := cyc.((i+1) mod len)
      done;
      !res

    (* Image of an element through a perm *)
    (* O(log(n)) *)
    let image x perm =
      try 
        snd (IntMap.find x perm)
      with
        Not_found -> x

    (* Orbit of an element through a perm, i.e. cycle of the elt *)
    let orbit_perm x perm =
      try 
        fst (IntMap.find x perm)
      with
        Not_found -> [x]

    (* Product of two permutations and related functions *)
    let rec compute_cycle_aux p1 p2 first i acc =
      let i' = image i p1 in
      let j  = image i' p2 in
      if j = first then (List.rev acc)
      else
        compute_cycle_aux p1 p2 first j (j :: acc)

    let compute_cycle p1 p2 i =
      Array.of_list (compute_cycle_aux p1 p2 i i [i])

    (* Product of two permutations. This is relatively costly. *)
    let prod (p1 : t) (p2 : t) =
      let result = ref IntMap.empty in
      let bv  = Bitv.create size false in
      for i = 0 to size - 1 do
        if not (Bitv.get bv i) then
          begin
            let cyc = compute_cycle p1 p2 i in
            let len = Array.length cyc in
            if len = 0 then
              failwith "Perm.prod: empty cycle"
            else if len > 1 then
              let map = Array.fold_left (fun map x ->
                Bitv.set bv x true;
                IntMap.add x (cyc, image_cycle x cyc) map
              ) !result cyc in
              result := map
            else ()
          end
        else
          ()
      done;
      !result
        
    (* Inverse of a cycle. *)
    let inv_cycle cyc =
      let arr = Array.copy cyc in
      let len = Array.length arr in
      for i = 0 to len / 2 do
        let tmp = arr.(i) in
        arr.(i) <- arr.(len - i - 1);
        arr.(len - i - 1) <- tmp
      done;
      arr

    (* A wrapper to ensure no cycle is duplicated. Reduces memory waste. 
     * A hashtable is used to intercept duplicates. The size of the hashtable is
     * set to [size]: in average, a permutation has log(size) different cycles so
     * collisions should be pretty rare (for big enough size). 
     * This is set with the assumption that size won't be enormous either. *)
    let inv_cycle_persistent () =
      let table = Hashtbl.create size in
      fun cyc ->
        try Hashtbl.find table cyc with
        | Not_found ->
          begin
            let icyc = inv_cycle cyc in
            Hashtbl.add table cyc icyc;
            icyc
          end

    (* Inverse of a permutation. *)
    let inv (p : t) =
      let invert_cycle = inv_cycle_persistent () in
      IntMap.fold (fun key (cycle, point) acc ->
        IntMap.add point (invert_cycle cycle, key) acc
      ) p IntMap.empty

    let action perm point = image point perm

    let add_mapping perm point image cycle =
      IntMap.add point (cycle, image) perm

    let push_cyc perm cyc =
      let len = Array.length cyc in
      let acc = ref perm in
      for i = 0 to len - 1 do
        acc := add_mapping !acc cyc.(i) cyc.((i+1) mod len) cyc
      done;
      !acc

    (* Create a perm from a list of /disjoint/ cycles. Notice we don't check for consistency. *)
    let rec of_cycles cycles =
      List.fold_left push_cyc identity cycles

    let print perm =
      let dom    = Prelude.mk_ints 0 (size - 1) in
      let codom  = List.map (fun i -> image i perm) dom in
      let doms   = Prelude.to_sseq string_of_int " " dom in
      let codoms = Prelude.to_sseq string_of_int " " codom in
      Printf.sprintf "%s\n%s\n" doms codoms

  end
    
  (* ------------------------------------------------ *)
  (* Lifting concrete operations to permutation words *)

  type permrec = 
    { p    : Concrete.t;
      invp : Concrete.t  }
      
  (* We want to avoid computing products and inverses unless we really need to.
   * Products of perms are simply lists of perms, and we compute the product only
   * when explicitly required. *)
  type t = 
  | Perm of permrec
  | Prod of t * t
  | Inv  of t

  (* Normalize a perm *)
  let rec normalize_aux x =
    match x with
    | Perm p -> p
    | Prod(l, r) ->
      let nl = normalize_aux l in
      let nr = normalize_aux r in
      { p    = Concrete.prod nl.p nr.p;
        invp = Concrete.prod nr.invp nl.invp }
    | Inv p ->
      let np = normalize_aux p in
      { p = np.invp; invp = np.p }

  let normalize x = Perm (normalize_aux x)

  let identity = Perm { p = Concrete.identity; invp = Concrete.identity }

  (* Moderately smart constructor (still O(1)) *)
  let invert p =
    match p with
    | Inv p' -> p'
    | _ -> Inv p

  let rec power x n =
    if n = 0 then identity
    else if n = 1 then x
    else if n mod 2 = 0 then
      let px = power x (n/2) in
      Prod(px, px)
    else
      let px = power x (n/2) in
      Prod(Prod(px, px),px)

  let rec action perm point =
    match perm with
    | Perm p -> Concrete.action p.p point
    | Prod(l, r) ->
      action r (action l point)
    | Inv p ->
      invert_action p point

  and invert_action perm point =
    match perm with
    | Perm p -> Concrete.action p.invp point
    | Prod(l, r) ->
      invert_action l (invert_action r point)
    | Inv p ->
      action p point

  (* Compute the orbit of a set of elements, and for each point in the
     orbit, a transversal. Another slower but more compact method would be to
     use a Schreier tree (i.e. a prefix tree with paths labelled by permutation 
     words). Storing the full transversal allows for direct access to
     its elements. 

     TODO: possibly more efficient algo, taking advantage of the cycles stored
     in the perm. Each elt of the cycle corresponds to a particular power of
     the perm acting on the considered point. This gives for each point and each
     group element the complete set of all possible transitions to other points.
     Orbit is then the connected component of a point. 

     NOTE: in the following algo, we forget the orignating point and return
     only for each point in the orbit the corresponding transversal.
  *)    
  let orbit group points =
    let queue        = Queue.create () in
    let transversals = ref IntMap.empty in
    let visited      = Bitv.create size false in
    List.iter (fun point -> Queue.add (point, identity) queue) points;
    while not (Queue.is_empty queue) do
      let (point, u) = Queue.take queue in
      if Bitv.get visited point then ()
      else begin
        Bitv.set visited point true;
        transversals := IntMap.add point u !transversals;
        List.iter (fun g ->
          Queue.add (action g point, Prod(u, g)) queue
        ) group
      end
    done;
    !transversals

  let of_cycles cycles =
    let cp = Concrete.of_cycles cycles in
    Perm { p = cp; invp = Concrete.inv cp }

  let print p =
    let perm = normalize_aux p in
    Concrete.print perm.p

  let print_orbit orb =
    let points = IntMap.bindings orb in
    Prelude.to_sseq (fun (point, transversal) ->
      Printf.sprintf "%d with transversal:\n%s\n" point (print transversal)
    ) "\n" points


  module Operators =
    struct
  
      let ( *** ) a b = Prod(a, b)
       

      let (^^) point perm = action perm point

    end

end

module P = Perm(struct let size = 5 end)

open P.Operators

let alpha = P.of_cycles [ [| 0; 4; 1; 2 |] ]

let beta  = P.of_cycles [ [| 0; 4; 3 |]; [| 1; 2 |] ]

let ab = alpha *** beta

let _ = Printf.printf "%s\n" (P.print alpha)

let _ = Printf.printf "%s\n" (P.print beta)

let _ = Printf.printf "%s\n" (P.print ab)

(* Try some orbit computation *)

let orb = P.orbit [ab] [ 0 ]

let _ = print_string (P.print_orbit orb)

(* Seems to work. *)

(* ----------------------- *)
(* Schreier-sims algorithm *)

type subgroup = {
  generators  : P.t list;    (* Generators of G^[i] *)
  transversal : P.t IntMap.t (* the orbit is the domain of the map *)
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
  let base'  = List.map (P.action perm) group.base in
  sift_aux base' group.chain

(* Schreier-Sims algorithm (from scratch) *)

exception EarlyExit of int

(* A nice invariant of our repesentation of permutations is that identity
   mappings are not stored. Hence any binding will do. *)
let rec find_point (generators : P.t list) =
  match generators with
  | [] -> None
  | (P.Perm g) :: gens ->    
    if IntMap.is_empty g.P.p then
      find_point gens
    else
      Some (fst (IntMap.choose g.P.p))
  | _ -> failwith "Group.find_point: generators not in normal form"

(* It is assumed that the generators are normalized. *)
let rec schreier_sims_aux (generators : P.t list) acc =
  (* Find a point that is not fixed by a generator *)
  match find_point generators with
  | None -> (* trivial group - end recursion *)
    let points, subgroups = List.split acc in
    { base  = List.rev points;
      chain = List.rev subgroups }
  | Some point ->
    let orb  = P.orbit generators [point] in
    let gens = (* Schreier generators for isotropy subgroup *)
      IntMap.fold (fun point u acc ->
        List.fold_left (fun acc g ->
          let ug    = u *** g in
          let ug_tr =
            let img = P.action ug point in
            try IntMap.find img orb with
            | Not_found -> failwith "Group.schreier_sims: bug found, could not find transversal"
          in
          (* TODO: normalize these ... OR normalize as needed in [find_point] *)
          (ug *** (P.Inv ug_tr)) :: acc
        ) acc generators
      ) orb [] in
    let subgroup = { generators = gens;  transversal = orb } in
    schreier_sims_aux gens ((point, subgroup) :: acc)

let schreier_sims generators = schreier_sims_aux generators []
