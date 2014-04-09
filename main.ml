
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

(*  Nodes in the graphs are coloured by C,H,O or P *)
module Atom =
  struct

    type t = C | H | O | P

    let compare (x : t) (y : t) = compare x y

    let inhabited = C

    let print = function
      | C -> "C"
      | H -> "H"
      | O -> "O"
      | P -> "P"

  end

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
    let free_electrons = function
      | C -> 4
      | H -> 1
      | O -> 2
      | P -> 1

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
       returns all ways to saturate it. *)
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
    let compatibility nc lc nc' =
      match nc, nc' with
      | O, O -> false
      | _    -> true

  end

(* SMILES ad-hoc output -- TODO make this generic *)

module NodeIdSet =
  Set.Make
    (struct
      type t      = int
      let compare (x : int) (y : int) = 
        if x < y then -1
        else if x > y then 1
        else 0
     end)

(* assuming acyclicity & connectedness *)
let rec to_smiles g already_explored current_node =
  if NodeIdSet.mem current_node already_explored then
    None
  else    
    let set = NodeIdSet.add current_node already_explored in
    let { Graph.clr; adj; deg } = Graph.get_info g current_node in
    match clr with
    | Atom.H -> None
    | Atom.P ->
      Some "O(P(=O)(O)(O))"
    | _ ->
      let str = 
        List.fold_left (fun acc (bond, target) ->
          match to_smiles g set target with
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
        
(* Instantiate the exhaustive generators with the particular modules
   given above *)
module Unrooted = UnrootedTree.Make(Atom)(Link)(Gram)

module Canon =
  struct

    type t = Unrooted.t

    type canonical = Unrooted.R.Encoded.t

    let canonical t =
      snd (Unrooted.canonical_root t)

    let compare x y = Unrooted.R.Encoded.tree_compare x y

  end

module G = Growable.Enumerate(Unrooted)(Canon)

let to_smiles =
  let open Unrooted.R in
  let rec aux tree =
    match tree with
    | ECNode(x, cs) ->
      match x with
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


(* start from a single carbon *)
let seed =
  let g = Unrooted.empty in
  Unrooted.add_node_with_colour g Atom.C


(* carbon pattern *)
let carbon =
  let g = Unrooted.empty in
  Unrooted.add_node_with_colour g Atom.C

(* hydrogen pattern *)
let hydrogen =
  let g = Unrooted.empty in
  Unrooted.add_node_with_colour g Atom.H

(* oxygen pattern *)
let oxygen =
  let g = Unrooted.empty in
  Unrooted.add_node_with_colour g Atom.O

(* phosphate pattern *)
let phosphate =
  let g = Unrooted.empty in
  Unrooted.add_node_with_colour g Atom.P

(* We allow to graft up to & carbons and 16 hydrogens on the seed *)
let mset =
  [ (carbon, 3); (hydrogen, 10); (oxygen, 4); (phosphate, 4); ]

let timer  = Prelude.create_timer ()
let _      = Prelude.start_timer timer
    
(* Peform the actual enumeration *)
let result = G.enumerate seed mset G.Canonical.empty

let time   = Prelude.get_timer timer

let result = G.Canonical.elements result

let s = List.map (fun (res, _) ->  Unrooted.R.print (Unrooted.root res 0)) result 
(* let _ = List.iter (Printf.printf "%s\n" ) s *)


let _ =
  List.iter (fun (g, _) -> 
    match to_smiles (Unrooted.root g 0) with 
    | None   -> () 
    | Some s -> 
      Printf.printf "%s\n" s
  ) result

let _ =  
  Printf.printf "generation time: %f seconds\n" time

let _ =  
  Printf.printf "cumultative time spent in automorphism computation: %f seconds\n" (!Unrooted.Auto.cmlt)

let _ =  
  Printf.printf "number of automorphism checks: %s\n" (Int64.to_string !Unrooted.Auto.auto_count)




(* What follows is debugging code, do not read *)

(*
let _ = 
  Printf.printf "max auto count: %d\n%!" !Unrooted.m
*)

(* -----------------------------
   Test automorphism detection *)

(* module Auto = Auto.Make *)
(*   (struct type t = int     *)
(*           let compare = compare  *)
(*           let print = string_of_int *)
(*           let inhabited = 0 *)
(*    end) *)
(*   (struct type t = string  *)
(*           let compare = String.compare *)
(*           let print x = x *)
(*           let inhabited = "" *)
(*    end) *)
  
(* let graph =  *)
(*   let g = Graph.empty in *)
(*   let g = Graph.add_node_with_colour g 1 in *)
(*   let g = Graph.add_node_with_colour g 0 in *)
(*   let g = Graph.add_node_with_colour g 0 in *)
(*   let g = Graph.add_node_with_colour g 0 in *)
(*   let g = Graph.add_edge g 0 "" 1 in *)
(*   let g = Graph.add_edge g 1 "" 2 in *)
(*   let g = Graph.add_edge g 2 "" 3 in *)
(*   Graph.add_edge g 3 "" 0 *)

(* let automorphisms = Auto.compute_automorphisms graph *)

(* let _ = List.iter (fun x -> Printf.printf "%s\n" (Perm.ArrayBased.print x)) (Prelude.filter_duplicates automorphisms) *)

(* let _ =   *)
(*   Printf.printf "number of automorphism checks: %s\n" (Int64.to_string !Auto.auto_count) *)

(* let _ =   *)
(*   Printf.printf "cumultative time spent in automorphism computation: %f seconds\n" (!Auto.cmlt) *)


(*
let _ =
  Perm.ArrayBased.size := 3

module PermTest = Bsgs.Make2(Perm.ArrayBased)

open PermTest


let generators = 
  [ [| 1; 0; 2 |];
    [| 0; 2; 1 |]
      
  ]

let partial_bsgs = compute_partial_subgroup_chain generators

let _ = Prelude.log (print partial_bsgs)
  
let _ = schreier_sims_aux partial_bsgs.chain 0
let _ = Prelude.log "----------"
let _ = schreier_sims_aux partial_bsgs.chain 1

let _ =
  let g = (Perm.of_concrete [| 1; 0; 2 |]) in
  match strip partial_bsgs.chain 1 g with
  | Ok w ->
    let w = Prelude.to_sseq Perm.print "." w in 
    Prelude.log (Printf.sprintf "** for gen %s, decomposition %s" (Perm.print g) w)
  | DropOut(i, residue) ->
    Prelude.log (Printf.sprintf "** for gen %s, residue %s at %d" (Perm.print g) (Perm.print residue) i)

(*let _ = schreier_sims_aux partial_bsgs.chain 0*)

let _ = Prelude.log (print partial_bsgs)
*)
