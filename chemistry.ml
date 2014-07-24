(* This modules implement automatic and exhaustive generation of chemical 
   reactions. We handle tree-like CHOP molecules. *)



(* -------------------------------------------------------------------------- *)
(* Specification of the CHOP chemistry *)
(* -------------------------------------------------------------------------- *)



(* -------------------------------------------------------------------------- *)
(* specify what kind of bindings can exist between atoms or radicals  *)
module Link =
  struct

    type t = Simple | Double | Triple

    let compare (x : t) (y : t) = compare x y

    let inhabited = Simple

    let print = function
      | Simple -> "simple"
      | Double -> "double"
      | Triple -> "triple"

  end

(* -------------------------------------------------------------------------- *)
(*  Nodes in the graphs are coloured by C,H,O or P *)
module Atom =
  struct
    
    type atom = C | H | O | P

    type t = { atom  : atom;
               arity : int  }

    let compare (x : t) (y : t) = compare x y

    let inhabited = { atom = C; arity = 4 }

    let print { atom } =
      match atom with
      | C -> "C"
      | H -> "H"
      | O -> "O"
      | P -> "P"

  end

(* -------------------------------------------------------------------------- *)
(* Grammar: specify what can link to what *)
module Gram =
  struct
    
    type nc = Atom.t

    type lc = Link.t

    open Link
    open Atom

    let rec electrons acc = function
      | [] -> acc
      | a :: tl ->
        match a with
        | Simple -> electrons (1 + acc) tl
        | Double -> electrons (2 + acc) tl
        | Triple -> electrons (3 + acc) tl

    let electrons x = electrons 0 x

    (* Specify how many free electrons for each type *)
    let free_electrons { arity } = arity

    (* Specify how much electrons are eaten by each
       type of bond *)
    let bound_electrons = function
      | Simple -> 1
      | Double -> 2
      | Triple -> 3

    (* Given the current state of a node, this function
       returns all the admissible bindings *)
    let extension_policy nc llc =
      let c = (free_electrons nc) - (electrons llc) in
      match c with
      | 0 -> []
      | 1 -> [Simple]
      | 2 -> [Simple; Double]
      | 3 -> [Simple; Double; Triple]
      | 4 -> [Simple; Double; Triple]
      | _ -> failwith "Main.extension_policy: inconsistent number of electrons"

    (* Given the current state of a node, this function
       returns all ways to saturate it. This function could
       in theory be automatically generated from
       extension_policy and a maximum number of electrons. *)
    let saturation_policy nc llc =
      let c = (free_electrons nc) - (electrons llc) in
      match c with
      | 0 -> [ [] ]
      | 1 -> [ [ Simple ] ]
      | 2 -> [ [ Simple; Simple ]; [ Double ] ]
      | 3 -> 
            [ [ Simple; Simple; Simple ];
              [ Simple; Double ];
              [ Triple ];
            ]
      | 4 ->
          [ [ Simple; Simple; Simple; Simple ];
            [ Simple; Simple; Double ];
            [ Simple; Triple ];
            [ Double; Double ]
          ]
      | _ -> failwith "Main.saturation_policy: inconsistent number of electrons"


    (* This function is where one could forbid e.g.
       oxygen-oxygen bindings *)
    let compatibility { atom = nc } lc { atom = nc' } =
      match nc, lc, nc' with
      | O, _,      O
      | O, _,      P
      (* TODO test *)
      | P, _,      O
      | C, Triple, C   -> false
      | _              -> true

  end


(* -------------------------------------------------------------------------- *)
(* Instantiate the exhaustive generators with the particular modules
   given above *)

module Molecule = UnrootedTree.Make(Atom)(Link)(Gram)

module Canon =
  struct

    type t = Molecule.t

    type canonical = Molecule.R.Encoded.t

    let canonical t =
      snd (Molecule.canonical_root t)

    let compare x y = Molecule.R.Encoded.tree_compare x y

  end

module Generator = Growable.Enumerate(Molecule)(Canon)

(* -------------------------------------------------------------------------- *)
(* SMILES ad-hoc output -- TODO make this generic *)

(* module NodeIdSet = *)
(*   Set.Make *)
(*     (struct *)
(*       type t      = int *)
(*       let compare (x : int) (y : int) =  *)
(*         if x < y then -1 *)
(*         else if x > y then 1 *)
(*         else 0 *)
(*      end) *)

(* (\* assuming acyclicity & connectedness *\) *)
(* let rec to_smiles g already_explored current_node = *)
(*   if NodeIdSet.mem current_node already_explored then *)
(*     None *)
(*   else *)
(*     let set = NodeIdSet.add current_node already_explored in *)
(*     let { Graph.clr; adj; deg } = Graph.get_info g current_node in *)
(*     match clr with *)
(*     | Atom.H -> None *)
(*     | Atom.P -> *)
(*       Some "O(P(=O)(O)(O))" *)
(*     | _ -> *)
(*       let str =  *)
(*         List.fold_left (fun acc (bond, target) -> *)
(*           match to_smiles g set target with *)
(*           | None -> acc *)
(*           | Some str -> *)
(*             match bond with *)
(*             | Link.Simple -> *)
(*               Printf.sprintf "%s(%s)" acc str *)
(*             | Link.Double -> *)
(*               Printf.sprintf "%s(=%s)" acc str *)
(*             | Link.Triple -> *)
(*               Printf.sprintf "%s(#%s)" acc str *)
(*         ) (Atom.print clr) adj *)
(*       in *)
(*       Some str *)

let to_smiles =
  let open Molecule.R in
  let rec aux tree =
    match tree with
    | ECNode(x, cs) ->
      match x.Atom.atom with
      | Atom.H -> None
      | Atom.P -> Some "P"
      | _ ->
        let str = 
          List.fold_left (fun acc (bond, target) ->
            match aux target with
            | None -> acc
            | Some str ->
              match bond with
              | Link.Simple ->
                Printf.sprintf "%s(%s)" acc str
              | Link.Double ->
                Printf.sprintf "%s(=%s)" acc str
              | Link.Triple ->
                Printf.sprintf "%s(#%s)" acc str
          ) (Atom.print x) cs
        in
        Some str
  in
  aux

(* -------------------------------------------------------------------------- *)
(* Reaction instantiation *)

(* A reaction scheme is given by a reaction input, a reaction output and
   an (injective) mapping from input stubs to output stubs. If everything
   is to make sense, the mapping should in fact be bijective. *)
type scheme =
  { input   : Molecule.t;
    output  : Molecule.t;
    mapping : (int * int) list
  }

(* Given a node, returns its "signature", which uniquely identifies
   the set of its possible extensions. *)
let node_extension_signature (info : (Atom.t, Link.t) Graph.info) =
  let nc   = info.Graph.clr in
  let llc  = List.map fst info.Graph.adj in
  let free = (Gram.free_electrons nc) - (Gram.electrons llc) in
  { nc with Atom.arity = free}
 

(* Extract all "grafting points" a.k.a. "seeds" from list of reaction schemes.
   We only need to generate possible instantiations once for each "seed". *)
let extract_seeds schemes =
  let rec loop schemes acc =
    match schemes with
    | [] -> acc
    | { input; mapping } :: tail ->
      List.fold_left (fun acc (seed, _) ->
        let info = Graph.get_info input seed in
        (node_extension_signature info) :: acc
      ) acc mapping
  in
  let seeds = loop schemes [] in
  Prelude.filter_duplicates seeds
  

(* The process of instantiation takes as inputs a reaction scheme and
   a multiset of components with which to extend the scheme.input into
   a complete input. Complete inputs are generated in two stages: first
   we generate once and for all all possible completions of all "seeds",
   i.e. all completions for each kind of grafting point. This avoids
   regenerating completions each time. Then we enumerate all combinations
   of inputs by (roughly) computing a cartesian product of completions.
   We have to check for isomorphism again while doing that.

   For each complete extension of the scheme.input, a matching scheme.output
   is automatically computed. The reaction is marshalled into an efficient
   representation (effectively mapping molecules to unique integers) and
   the whole list is written in a file. *)

let instantiate_schemes schemes multiset =
  let seeds = extract_seeds schemes in
  let seed_table = List.map (fun atom ->
    let m = Molecule.empty in
    let n = Molecule.add_node_with_colour m atom in
    (atom, Generator.enumerate n multiset Generator.Canonical.empty)
  ) seeds in
  seed_table