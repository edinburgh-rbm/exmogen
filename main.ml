
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

    (* The outcome lists could be hoisted.  *)
    let growth_policy nc llc =
      let e = electrons 0 llc in
      let c = match nc with
        | C -> 4 - e
        | H -> 1 - e
        | O -> 2 - e
        | P -> 1 - e
      in match c with
      | 0 -> []
      | 1 -> [Simple]
      | 2 -> [Simple; Double]
      | 3 -> [Simple; Double; Triple]
      | 4 -> [Simple; Double; Triple]
      | _ -> failwith "inconsistent stuff happening"

    let compatibility _ _ _ = true

  end


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
let oxygen =
  let g = Unrooted.empty in
  Unrooted.add_node_with_colour g Atom.P

(* Allow 3 carbons and 10 hydrogens (+ the seed) to combine *)

let mset =
  [ (carbon, 2); (hydrogen, 8) ]

let timer  = Prelude.create_timer ()
let _      = Prelude.start_timer timer
    
let result = G.enumerate seed mset G.Canonical.empty

let time   = Prelude.get_timer timer

let result = G.Canonical.elements result

let s = List.map (fun (res, _) ->  Unrooted.R.print (Unrooted.root res 0)) result

let _ = List.iter (Printf.printf "%s\n" ) s

let _ =  
  Printf.printf "generation time: %f seconds\n" time

let _ =  
  Printf.printf "cumultative time spent in automorphism computation: %f seconds\n" (!Unrooted.Auto.cmlt)


(* -----------------------------
   Test automorphism detection *)

module Auto = Auto.Make
  (struct type t = int    
          let compare = compare 
          let print = string_of_int
          let inhabited = 0
   end)
  (struct type t = string 
          let compare = String.compare
          let print x = x
          let inhabited = ""
   end)
  

let graph = 
  let g = Graph.empty in
  let g = Graph.add_node_with_colour g 1 in
  let g = Graph.add_node_with_colour g 0 in
  let g = Graph.add_node_with_colour g 0 in
  let g = Graph.add_node_with_colour g 0 in
  let g = Graph.add_edge g 0 "" 1 in
  let g = Graph.add_edge g 1 "" 2 in
  let g = Graph.add_edge g 2 "" 3 in
  Graph.add_edge g 3 "" 0

let automorphisms = Auto.compute_automorphisms graph

let _ = List.iter (fun x -> Printf.printf "%s\n" (Perm.ArrayBased.print x)) (Prelude.filter_duplicates automorphisms)


let _ =  
  Printf.printf "cumultative time spent in automorphism computation: %f seconds\n" (!Auto.cmlt)



