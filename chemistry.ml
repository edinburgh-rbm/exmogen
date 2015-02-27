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

(* -------------------------------------------------------------------------- *)
(* Reaction instantiation *)
(* -------------------------------------------------------------------------- *)

let c = Atom.({ atom = C; arity = 4 })
let h = Atom.({ atom = H; arity = 1 })
let o = Atom.({ atom = O; arity = 2 })
let p = Atom.({ atom = P; arity = 1 })

let carbon, cv =
  let g = Molecule.empty in
  Molecule.add_node_with_colour g c

let hydrogen, hv =
  let g = Molecule.empty in
  Molecule.add_node_with_colour g h

let oxygen : Molecule.t =
  let g = Molecule.empty in
  fst (Molecule.add_node_with_colour g o)

let phosphate : Molecule.t =
  let g = Molecule.empty in
  fst (Molecule.add_node_with_colour g p)


let atoms_enum = [ c; h; o; p ]

let atom_of_string s =
  match s with
  | "C" -> Some c
  | "H" -> Some h
  | "O" -> Some o
  | "Phos" -> Some p
  | _   -> None

let link_of_smiles = function
  | Reactions.Simple -> Link.Simple
  | Reactions.Double -> Link.Double
  | Reactions.Triple -> Link.Triple

exception NonConvertibleMolecule of string

(* Maps Reactions.smart_ast to actual graphs, instantiating all
   variables for concrete atoms. 

           /!\ Does not check for arity /!\  

   Produces a map variable -> node along the way. If the molecule
   cannot be converted, raises NonConvertibleMolecule
*)
let rec molecule_to_graphs m graph map =
  match m with
  | Reactions.Node(Reactions.Atom s, submols) ->
    (match atom_of_string s with
    | None -> 
      raise (NonConvertibleMolecule s)
    | Some colour ->
      extend_graph_with_colour m graph map colour submols
    )    
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


(* Converts a molecule into a graph, as [molecule_to_graphs],
   but replaces variables with actual concrete nodes found in
   the map. Produces a vertex -> vertex map. *)
let rec instantiate_molecule m graph string_map vertex_map =
  match m with
  | Reactions.Node(Reactions.Atom s, submols) ->
    (match atom_of_string s with
    | None        -> raise (NonConvertibleMolecule s)
    | Some colour ->
      extend_graph_with_colour' m graph string_map vertex_map colour submols)
  | Reactions.Node(Reactions.Var v, submols) ->
    let colour, source_vertex =
      try List.assoc v string_map with
      | Not_found -> failwith "Chemistry.instantiate_molecule: variable not found in given map"
    in
    let (graph, anchor, vertex_map) = extend_graph_with_colour' m graph string_map vertex_map colour submols in
    (graph, anchor, (source_vertex, anchor) :: vertex_map)

and extend_graph_with_colour' m graph string_map vertex_map colour submols =
  let (graph, anchor) = Graph.add_node_with_colour graph colour in
  List.fold_left (fun (graph, anchor, vertex_map) (link, submol) ->
    let (g', anchor', vertex_map) = instantiate_molecule submol graph string_map vertex_map in
    let g = Molecule.add_edge g' anchor (link_of_smiles link) anchor' in
    (g, anchor, vertex_map)
  ) (graph, anchor, vertex_map) submols


(* Checks that the graph produced from a parsed molecule respects the arities
   of each atomic species. *)  
let check_arity m =
  Molecule.fold (fun v lab adjs acc ->
    let arity = List.length adjs in
    arity <= lab.Atom.arity && acc
  ) m true

(* Checks that any variable appears at most once in a molecule. *)
let check_linearity map =
  let map' = List.sort (fun (x,_) (y,_) -> String.compare x y) map in
  map' = (Prelude.filter_duplicates' map')

(* Convert a parsed molecule into a set of compatible graphs,
   filters out molecules which do not verify the arity constraint. 
   TODO: failure of linearity should raise an error. *)
let molecule_to_graphs m = 
  let outcomes = molecule_to_graphs m Molecule.empty [] in
  List.filter (fun (g, anchor, map) ->
    check_arity g &&
    check_linearity map
  ) outcomes

let molecule_is_trivial =
  let open Reactions in
  function
  | Node(Atom(s), []) ->
      (match atom_of_string s with
      | None   -> true
      | Some _ -> false
      )
  | _ -> false
        
let isolate_nontrivial_molecule (l : Reactions.smiles_ast list) =
  let open Reactions in
  let (trivial, non_trivial) = List.partition molecule_is_trivial l in
  match non_trivial with
  | _ :: _ :: _ -> failwith "Chemistry.isolate_nontrivial_molecule: more than one non-trivial molecule"
  | _ ->
    let trivial = List.map (function 
      | Node(Atom(s), []) -> s
      | _ -> failwith "Chemistry.isolate_nontrivial_molecule: bug found"
    ) trivial in
    (trivial, non_trivial)


(* A reaction scheme is given by a reaction input, a reaction output and
   an (injective) mapping from input stubs to output stubs. If everything
   is to make sense, the mapping should in fact be bijective. *)
type nontrivial_reactants =
  { input   : Molecule.t;
    output  : Molecule.t;
    mapping : (Graph.vertex * Graph.vertex) list
  }

type reaction = 
  { nontrivial_reactants : nontrivial_reactants option;
    trivial_inputs       : string list;
    trivial_outputs      : string list
  }

(* Converts a parsed reaction into a list of concrete reactions. *)
let instantiate_reaction { Reactions.input; output } =
  let (trivial_inputs, non_trivial_input)   = isolate_nontrivial_molecule input in
  let (trivial_outputs, non_trivial_output) = isolate_nontrivial_molecule output in
  match non_trivial_input, non_trivial_output with
  | [], [] ->
    [{ nontrivial_reactants = None;
       trivial_inputs       = trivial_inputs;
       trivial_outputs      = trivial_outputs }]
  | [input], [output] ->
    let inputs = molecule_to_graphs input in
    List.map (fun (lhs, hook, string_map) ->
      let string_map = List.map (fun (str, v) -> (str, (Molecule.get_colour lhs v, v))) string_map in
      let (rhs, hook', vertex_map) = instantiate_molecule output Molecule.empty string_map [] in
      let nontrivial = 
        { input   = lhs;
          output  = rhs;
          mapping = vertex_map } in
      { nontrivial_reactants = Some nontrivial;
        trivial_inputs       = trivial_inputs;
        trivial_outputs      = trivial_outputs }
    ) inputs    
  | _ ->
    failwith "Chemistry.instantiate_reaction: mismatching number of non-trivial reactants"     



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
    | { nontrivial_reactants = Some { input; mapping } } :: tail ->
      let acc = List.fold_left (fun acc (seed, _) ->
        let info = Graph.get_info input seed in
        (typeof info) :: acc
      ) acc mapping
      in
      loop tail acc
    | _ :: tail->
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


let print_reaction writer reaction =
  match reaction.nontrivial_reactants with
  | None ->
    let trivial_inputs  = Prelude.to_sseq (fun x -> x) " + " reaction.trivial_inputs
    and trivial_outputs = Prelude.to_sseq (fun x -> x) " + " reaction.trivial_outputs in
    writer (Printf.sprintf "%s <-> %s;\n" trivial_inputs trivial_outputs)
  | Some { input; output } ->
    let smiles_in  = 
      match to_smiles input with
      | None -> 
        (Molecule.print input;
         failwith "Error in SMILES output")
      | Some x -> x 
    in
    let smiles_out = 
      match to_smiles output with 
      | None -> 
        (Molecule.print input;
         failwith "Error in SMILES output")
      | Some x -> x
    in
    let inputs  = Prelude.to_sseq (fun x -> x) " + " (smiles_in :: reaction.trivial_inputs)
    and outputs = Prelude.to_sseq (fun x -> x) " + " (smiles_out :: reaction.trivial_outputs) in
    writer (Printf.sprintf "%s <-> %s;\n" inputs outputs)


let instantiate_scheme seeds writef reaction =
  match reaction.nontrivial_reactants with
  | Some { input; output; mapping } ->
    iter_fsection
      (fun (vin, _) -> List.assoc (typeof (Graph.get_info input vin)) seeds)
      (fun (vin, vout) (graphtling, _) { input; output; mapping } -> 
        let input  = Graph.graft input graphtling  vin (Graph.v_of_int 0) in
        let output = Graph.graft output graphtling vout (Graph.v_of_int 0) in
        { input; output; mapping }
      )
      mapping
      { input; output; mapping }
      (function nontriv -> writef { reaction with nontrivial_reactants = (Some nontriv) })
  | None ->
    ()

let instantiate_schemes schemes ingredients (writef : reaction -> unit) =
  let seeds = extract_seeds schemes in
  let sat   = saturate_all_seeds seeds ingredients in
  List.iter (instantiate_scheme sat writef) schemes

let instantiate_reactions (reactions : Reactions.reactions) ingredients (writef : string -> unit) =
  let schemes =
    List.fold_left (fun acc reaction -> 
      List.rev_append (instantiate_reaction reaction) acc      
    ) [] reactions
  in
  instantiate_schemes schemes ingredients (print_reaction writef)

let enumerate seed mset writef =
  let canonical, _ = Generator.enumerate seed mset (Generator.CanonicalSet.empty, 0) in
  Generator.CanonicalSet.iter (fun (x,_) -> writef x) canonical
(* let instantiate_schemes schemes multiset = *)
(*   let seeds = extract_seeds schemes in *)
(*   let seed_table = List.map (fun atom -> *)
(*     let m = Molecule.empty in *)
(*     let n = Molecule.add_node_with_colour m atom in *)
(*     (atom, Generator.enumerate n multiset Generator.Canonical.empty) *)
(*   ) seeds in *)
(*   seed_table *)
