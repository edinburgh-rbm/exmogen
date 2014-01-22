open Prelude

(* Labelled trees. We expect labels to be totally ordered. 
*)

module NodeColoured(Lab : Ordered) =
struct

  (* Rooted, ordered, node-coloured trees. Note that coloured edges can
     be faithfuly encoded as node-coloured trees.
  *)

  (* Type of rooted ordered node-coloured (RONC) trees *)
  type t = Node of Lab.t * t list

  (* Lexicographic order on RONCs given total order on colours *)
  let rec tree_compare t1 t2 =
    match t1, t2 with
    | Node(a, la), Node(b, lb) ->
      let c = Lab.compare a b in
      if c <> 0 then c
      else
        lexicographic_compare la lb

  and lexicographic_compare l1 l2 =
    match l1, l2 with
    | [], [] -> 0
    | [], _  -> -1
    | _, []  -> 1
    | t1 :: tail1, t2 :: tail2 ->
      let c = tree_compare t1 t2 in
      if c <> 0 then c
      else
        lexicographic_compare tail1 tail2

  (* Canonicalisation of RONCs by sorting *)        
  let rec sort tree =
    match tree with
    | Node(a, leaves) ->
      let leaves = List.map sort leaves in
      Node(a, List.sort tree_compare leaves)
      
end

(* Rooted Node-Coloured Edge-Coloured trees *)
module NodeEdgeColoured
  (NLab : Ordered)        (* node labels are ordered *)
  (LLab : Ordered)        (* edge labels are ordered *)
  =
struct
  
  (* Concrete representation of edge-coloured trees. *)
  type t = ECNode of NLab.t * (LLab.t * t) list

  (* We can build a lexicographic ordering from our parameters. *)
  module Product =
    struct
      
      type t = NLab.t * LLab.t

      let compare (n, l) (n', l') =
        let c = NLab.compare n n' in
        if c <> 0 then c
        else LLab.compare l l'

      let inhabited = (NLab.inhabited, LLab.inhabited)

      let print (n, l) = Printf.sprintf "(%s, %s)" (NLab.print n) (LLab.print l)

    end

  (* Instantiate RONCs with [Product] *)
  module Encoded = NodeColoured(Product)
      
  (* Conversion to RONCs. We have a conversion function
      [n](l) : t -> LLab.t -> Encoded.t     
     . intial case: initiate [r](LLab.inhabited) on the root r
     . inductive case, node n with input label l0, subtrees st
       for each subtree -l-> n' in st, produce a RONC [n'](l) by mapping [.](.) to st
     the result RONC has root node (n, l0) with subtrees map [.](.) st
  *)
  let rec to_ronc tree l0 =
    match tree with
    | ECNode(n, cs) ->
      let subtrees = List.map (fun (l, n') -> to_ronc n' l) cs in
      Encoded.Node((n, l0), subtrees)

  let to_ronc tree = to_ronc tree LLab.inhabited

  let rec print = function
    | ECNode(n, cs) ->
      let css = List.map (fun (l, n') -> (LLab.print l, print n')) cs in
      let css = List.fold_right (fun (ls, ns') acc -> Printf.sprintf "[%s ->%s]; %s" ls ns' acc) css "" in
      Printf.sprintf "*(%s, %s)" (NLab.print n) css

end


(* (\* Random RONC generation - not fair in any way *\)     *)
(* let rec random_tree decay = *)
(*   let len = int_of_float (Random.float (3.0 *. decay)) in *)
(*   Node(Random.int 5, mk_list decay len) *)

(* and mk_list decay i =  *)
(*   if i = 0 then [] *)
(*   else (random_tree decay) :: (mk_list decay (i-1)) *)
