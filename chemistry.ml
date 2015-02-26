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
      | O, _,      P   (* TODO test *)
      | P, _,      O
      | C, Triple, C   -> false
      | _              -> true

  end


(* -------------------------------------------------------------------------- *)
(* Instantiate the exhaustive generators with the particular modules
   given above *)

module Molecule  = UnrootedTree.Make(Atom)(Link)(Gram)

module Generator = Growable.Enumerate(Molecule.Growable)(Molecule.Canonical)

module CanonicalSet = Generator.CanonicalSet

(* -------------------------------------------------------------------------- *)
(* SMILES ad-hoc output -- TODO make this generic *)

(* assuming acyclicity & connectedness *)
let rec to_smiles_aux g already_explored current_node =
  if List.mem current_node already_explored then
    None
  else
    let set = current_node :: already_explored in
    let ({ Graph.clr; adj; deg } as info) = Graph.get_info g current_node in
    match clr.Atom.atom with
    | Atom.H -> None
    | Atom.P -> Some "p"
      (* Some "O(P(=O)(O)(O))" *)      
    | _ ->
      let str =
        List.fold_left (fun acc (bond, target) ->
          match to_smiles_aux g set target with
          | None -> acc
          | Some str ->
            match bond with
            | Link.Simple ->
              Printf.sprintf "%s(%s)" acc str
            | Link.Double ->
              Printf.sprintf "%s(=%s)" acc str
            | Link.Triple ->
              Printf.sprintf "%s(#%s)" acc str
        ) (Atom.print clr) adj
      in
      Some str

let to_smiles graph =
  to_smiles_aux graph [] (Graph.v_of_int 0)

(* let to_smiles = *)
(*   let open Molecule.R in *)
(*   let rec aux tree = *)
(*     match tree with *)
(*     | ECNode(x, cs) -> *)
(*       match x.Atom.atom with *)
(*       | Atom.H -> None *)
(*       | Atom.P -> Some "p" *)
(*       | _ -> *)
(*         let str =  *)
(*           List.fold_left (fun acc (bond, target) -> *)
(*             match aux target with *)
(*             | None -> acc *)
(*             | Some str -> *)
(*               match bond with *)
(*               | Link.Simple -> *)
(*                 Printf.sprintf "%s(%s)" acc str *)
(*               | Link.Double -> *)
(*                 Printf.sprintf "%s(=%s)" acc str *)
(*               | Link.Triple -> *)
(*                 Printf.sprintf "%s(#%s)" acc str *)
(*           ) (Atom.print x) cs *)
(*         in *)
(*         Some str *)
(*   in *)
(*   fun graph -> *)
(*     let tree = Molecule.root graph (Graph.v_of_int 0) in *)
(*     aux tree *)

(* -------------------------------------------------------------------------- *)
(* Reaction instantiation *)

(* A reaction scheme is given by a reaction input, a reaction output and
   an (injective) mapping from input stubs to output stubs. If everything
   is to make sense, the mapping should in fact be bijective. *)
type reaction =
  { input   : Molecule.t;
    output  : Molecule.t;
    mapping : (Graph.vertex * Graph.vertex) list
  }

(* (\* Checks the validity of a Reactions.smiles_ast molecule *\) *)
(* let rec check_chop_validity m = *)
(*   match m with *)
(*   | Reactions.Node(atom, submols) -> *)
(*     match atom with *)
(*     | Reaction.Atom s -> *)
(*       match s with *)
(*       | "C" -> *)
(*       | "H" *)
(*       | "O" *)
(*       | "P" *)
(*     | Reaction.Var s *)


let c = Atom.({ atom = C; arity = 4 })
let h = Atom.({ atom = H; arity = 1 })
let o = Atom.({ atom = O; arity = 2 })
let p = Atom.({ atom = P; arity = 1 })

let atoms_enum = [ c; h; o; p ]

let link_of_smiles = function
  | Reactions.Simple -> Link.Simple
  | Reactions.Double -> Link.Double
  | Reactions.Triple -> Link.Triple

(* Maps Reactions.smart_ast to actual graphs, instantiating all
   variables for concrete atoms. 

           /!\ Does not check for arity /!\  

   Produces a map variable -> node along the way.
*)
let rec molecule_to_graphs m graph map =
  match m with
  | Reactions.Node(Reactions.Atom s, submols) ->
    let colour =
      (match s with
      | "C" -> c
      | "H" -> h
      | "O" -> o
      | "P" -> p
      | _   -> failwith "Chemistry.molecule_to_graph: unknown atomic compound")
    in
    extend_graph_with_colour m graph map colour submols
  | Reactions.Node(Reactions.Var v, submols) ->
    let res_c = extend_graph_with_colour m graph map c submols
    and res_h = extend_graph_with_colour m graph map h submols
    and res_o = extend_graph_with_colour m graph map o submols
    and res_p = extend_graph_with_colour m graph map p submols in
    let graphs = res_c @ res_h @ res_o @ res_p in
    List.map (fun (graph, anchor, map) -> 
      (graph, anchor, (v, anchor) :: map)
    ) graphs

and extend_graph_with_colour m graph map colour submols =
  let (graph, anchor) = Graph.add_node_with_colour graph colour in
  List.fold_left (fun graphs (link, submol) ->
    extend_graphs_with_sub_molecule graphs link submol
  ) [(graph, anchor, map)] submols

and extend_graphs_with_sub_molecule graphs link submol =
  List.fold_left (fun acc (graph, anchor, map) ->
    let extended_graphs = molecule_to_graphs submol graph map in
    link_extended_graphs_to_root extended_graphs anchor link acc
  ) [] graphs

and link_extended_graphs_to_root extended_graphs anchor link acc =
  List.fold_left (fun acc (g, anchor', map) ->
    let g = Molecule.add_edge g anchor (link_of_smiles link) anchor' in
    (g, anchor, map) :: acc
  ) acc extended_graphs


let molecule_to_graphs m = molecule_to_graphs m Molecule.empty []


(* Converts Reactions.smiles_ast to a set of molecules. Each variable is
   replaced by all compatible Atom.t elements, taking into account arity. *)
(* let rec concretize_molecule m = *)
(*   match m with *)
(*   | Reactions.Node(atom, submols) -> *)
(*     match atom with *)
(*     | Reaction.Atom s -> *)
(*       match s with *)
(*       | "C" *)
(*       | "H" *)
(*       | "O" *)
(*       | "P" *)
(*     | Reaction.Var s *)
(* Convert reaction from the parser to a proper reaction. *)


(* Given a node, returns its "signature", which uniquely identifies
   the set of its possible extensions. *)
let typeof (info : (Atom.t, Link.t) Graph.info) =
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
      let acc = List.fold_left (fun acc (seed, _) ->
        let info = Graph.get_info input seed in
        (typeof info) :: acc
      ) acc mapping
      in
      loop tail acc
  in
  let seeds = loop schemes [] in
  Prelude.filter_duplicates seeds

(* Molecule.t list -> Molecule.t Generator.mset -> (Molecule.t * Generator.Canonical.t) list *)
let saturate_all_seeds seeds mset =
  List.rev_map (fun seed ->
    let molseed, _  = Molecule.add_node_with_colour Molecule.empty seed in
    let completions = Generator.enumerate molseed mset (Generator.CanonicalSet.empty, 0) in
    (seed, completions)
  ) seeds
  
(* A special version of fold_fsection that tries to mimize memory allocation *)
let rec fold_fsection_aux gen f index acc =
  match index with
  | [] -> acc
  | i :: tl ->
    let col, card = gen i in
    let acc =
      Generator.CanonicalSet.fold (fun x acc' ->
        List.fold_left (fun acc' thread ->         
          (f i x thread) :: acc'
        ) acc' acc
      ) col []
    in
    fold_fsection_aux gen f tl acc

let fold_fsection_tr gen f index facc = fold_fsection_aux gen f index [facc]

let rec iter_fsection gen f index current writef =
    match index with
    | [] -> writef current
    | i :: tl ->
      let col, card = gen i in
      Generator.CanonicalSet.iter (fun x ->
        iter_fsection gen f tl (f i x current) writef
      ) col

let instantiate_scheme seeds writef { input; output; mapping } =
  iter_fsection
    (fun (vin, _) -> List.assoc (typeof (Graph.get_info input vin)) seeds)
    (fun (vin, vout) (graphtling, _) { input; output; mapping } -> 
      let input  = Graph.graft input graphtling  vin (Graph.v_of_int 0) in
      let output = Graph.graft output graphtling vout (Graph.v_of_int 0) in
      { input; output; mapping }
    )
    mapping
    { input; output; mapping }
    writef

let instantiate_schemes schemes ingredients (writef : reaction -> unit) =
  let seeds = extract_seeds schemes in
  let sat   = saturate_all_seeds seeds ingredients in
  List.iter (instantiate_scheme sat writef) schemes

let enumerate seed mset writef =
  let canonical, _  = Generator.enumerate seed mset (Generator.CanonicalSet.empty, 0) in
  Generator.CanonicalSet.iter (fun (x,_) -> writef x) canonical
(* let instantiate_schemes schemes multiset = *)
(*   let seeds = extract_seeds schemes in *)
(*   let seed_table = List.map (fun atom -> *)
(*     let m = Molecule.empty in *)
(*     let n = Molecule.add_node_with_colour m atom in *)
(*     (atom, Generator.enumerate n multiset Generator.Canonical.empty) *)
(*   ) seeds in *)
(*   seed_table *)
