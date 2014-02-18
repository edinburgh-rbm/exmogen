open Prelude
open RootedTree

(* Unrooted, Node-coloured, Edge-coloured trees *)

(* We use a persistent implementation of graphs to represent unrooted trees. 
   Some auxilliary data structures need to be defined. *)


(* The arity of a node is specified locally as a function of its neighborhood. 
   In order to keep things abstract and having in mind to satisfy the signature Growable.GrowableType,
   we parameterise the module by two functions :
   1) a function mapping any neighborhood to a list of coloured edges by which it can connect to something else
      (this allows to take care of standard arities)
   2) a function asserting whether two nodes can connect through a previously proposed edge
      (this allows to encode some very simple regular constraints on the path languages of the tree)
*)

module type GrammarType =
  sig

    (* type of node colours *)
    type nc

    (* type of label colours *)
    type lc

    (* given a node with colour nc, and a list of coloured edges to neighbours, produce
       a list of opportunities for connections among which /only one/ can be taken. We
       assume that the behaviour of [growth_policy] is "monotonic": once it has stopped
       proposing stuff, it will stay that way. *)
    val growth_policy : nc -> lc list -> lc list

    (* validity of an edge (nc, lc, nc). *)
    val compatibility : nc -> lc -> nc -> bool

  end

module Make
  (NLab : Ordered)        (* node labels are ordered *)
  (LLab : Ordered)        (* edge labels are ordered *)
  (Gram : GrammarType
   with type nc = NLab.t
   and  type lc = LLab.t)
  =
struct  

  (* ---------------------------------- *)
  (* Instantiate automorphism detection *)
  module Auto = Auto.Make(NLab)(LLab)


  (* ---------------- *)
  (* Type definitions *)

  (* An insertion point onto the graph *)
  type plug = (int * (NLab.t, LLab.t) Graph.info) * LLab.t

  type t = (NLab.t, LLab.t) Graph.t

  (* -------- *)
  (* Wrappers *)

  let empty = Graph.empty
    
  let add_node_with_colour = Graph.add_node_with_colour

  (* --------------------------- *)
  (* Printing to DOT file format *)

  let to_dot file_name graph_name graph print_node =
    Graph.to_dot file_name graph_name graph print_node LLab.print

  (* -----------------------------------*)
  (* Conversion from unrooted to rooted *)

  (* Instantiate rooted tree module *)

  module R = RootedTree.NodeEdgeColoured(NLab)(LLab)

  type canonical = unit

  (* Given a vertex, transform the unrooted tree into a rooted one. 
     We have to take into account that the undirectedness is implemented
     using two directed edges, inducing local pseudo-loops. *)
  let rec root graph v last =
    let { Graph.clr; adj } = Graph.get_info graph v in
    match last with
    | None ->
      let subtrees = List.map (fun (l, x) -> (l, root graph x (Some v))) adj in
      R.ECNode(clr, subtrees)
    | Some v' ->
      (* In our unrooted trees the subtrees are a priori not ordered, so
       * we can use fold_left. *)
      let subtrees = List.fold_left (fun acc (l, elt) ->
        if elt = v' then acc
        else (l, root graph elt (Some v)) :: acc
      ) [] adj in
      R.ECNode(clr, subtrees)

  (* wrapper *)
  let root tree v = root tree v None

  (* -------------------------- *)
  (* Finding the canonical root *)

  (* TODO: we would like to compute the cumulative distance in an
     incremental way. *)

  let compute_adjacency_matrix a graph =
    for i = 0 to Array.length a - 1 do
      let n = Graph.get_neighbours graph i in
      let n = List.filter (fun (_,x) -> x > i) n in
      List.iter (fun (_, j) ->
        a.(i).(j) <- 1;
        a.(j).(i) <- 1
      ) n
    done

  (* Do it the dense way. For small graphs this should be ok. 
     Observe that we take care to not allocate too much memory. 
     This is basically Floyd-Warshall.
  *)
  let compute_distances graph =
    let dim    = Graph.size graph in
    let adj    = Matrix.alloc dim dim in
    let tmp    = Matrix.alloc dim dim in
    let iteree = Matrix.alloc dim dim in
    let dist   = Matrix.alloc dim dim in
    compute_adjacency_matrix adj graph;
    Matrix.copy_in_place adj tmp;
    Matrix.copy_in_place adj iteree;
    Matrix.copy_in_place adj dist;
    let depth   = ref 1 in
    while !depth < dim - 1 do
      for i = 0 to dim - 1 do
        for j = i+1 to dim - 1 do
          if iteree.(i).(j) > 0 && tmp.(i).(j) = 0 && dist.(i).(j) = 0 then
            (dist.(i).(j) <- !depth;
             dist.(j).(i) <- !depth)
          else ()
        done
      done;
      Matrix.copy_in_place iteree tmp;
      Matrix.multiply iteree tmp adj;
      incr depth
    done;
    Matrix.free adj;
    Matrix.free tmp;
    Matrix.free iteree;
    dist

  let compute_distances graph =
    let dim    = Graph.size graph in
    let dist   = Matrix.alloc dim dim in
    Matrix.init dist dim;
    compute_adjacency_matrix dist graph;
    for i = 0 to dim - 1 do
      dist.(i).(i) <- 0
    done;
    for k = 0 to dim - 1 do
      let distk = dist.(k) in
      for i = 0 to dim - 1 do
        let disti = dist.(i) in
        for j = 0 to dim - 1 do
          let d = disti.(k) + distk.(j) in
          if disti.(j) > d then
            disti.(j) <- d
          else ()
        done
      done
    done;
    dist

  let cumulative_distance graph =
    let dist = compute_distances graph in
    let res  = Array.init (Graph.size graph) (fun i ->
      Array.fold_left (+) 0 dist.(i)
    ) in
    Matrix.free dist;
    res

  (* minimal indice/s/ in an integer array *)
  let minimal_indices array =
    let min_bucket = ref [] in
    let min_elt    = ref max_int in
    for i = 0 to Array.length array - 1 do
      if array.(i) < !min_elt then
        (min_bucket := [i];
         min_elt := array.(i))
      else if array.(i) = !min_elt then
        min_bucket := i :: !min_bucket
    done;
    !min_bucket

  (* Computing the canonical root. *)
  let canonical_root tree =
    let c = cumulative_distance tree in
    let i = minimal_indices c in
    match i with
    | [] ->
      failwith "less than 1 minimal indices - something is seriously wrong"
    | [x] ->
      (x, R.Encoded.sort (R.to_ronc (root tree x)))
    | [x; y] ->
      let rx = R.Encoded.sort (R.to_ronc (root tree x)) in
      let ry = R.Encoded.sort (R.to_ronc (root tree y)) in
      let c  = R.Encoded.tree_compare rx ry in
      if c = 1 then
        (y, ry)
      else
      (* when c = 0, isomorphic trees -- we don't care which one we return *)
        (x, rx)
    | _ ->
      let m = 
        Printf.sprintf 
          "more than 2 minimal indices - something is seriously wrong (%s sz %d)" 
          (strof_iarr c) 
          (Graph.size tree)
      in
      let _ = to_dot "error.dot" "erroneous" tree (fun clr i ->
        Printf.sprintf "\"%i = %s/%d\"" i (NLab.print clr) c.(i)
      )
      in
      failwith m
        
  (* ------------------------------------------- *)
  (* Satisfy the Growable.GrowableType signature *)

  let propose graph =
    Graph.NodeIdMap.fold (fun v i acc ->
      let (v, { Graph.clr; adj }) as info = (v, Graph.get_info graph v) in
      let adjc  = List.map fst adj in
      let links = Gram.growth_policy clr adjc in
      List.fold_left (fun acc lc -> (info, lc) :: acc) acc links
    ) (Graph.info graph) []


  let m = ref 0

  let propose graph =
    let autos = Auto.compute_automorphisms graph in
    m := max !m (List.length autos);
    let (acc, _) = Graph.NodeIdMap.fold (fun v i (acc, cover) ->
      if
        (* let _ = Printf.printf "_%!" in *)
        IntSet.mem v cover then (acc, cover)
      else
        (* let _ = Printf.printf ".%!" in *)
        let (_, { Graph.clr; adj }) as info = (v, Graph.get_info graph v) in
        let adjc  = List.map fst adj in
        let links = Gram.growth_policy clr adjc in
        let cover = List.fold_left (fun cover perm ->
          IntSet.union cover (Perm.ArrayBased.orbit perm v)
        ) cover autos in
        let acc   = List.fold_left (fun acc lc -> (info, lc) :: acc) acc links in
        (acc, cover)
    ) (Graph.info graph) ([], IntSet.empty)
    in
    acc

  (* Compute the disjoint union of two graphs. This implies shifting the nodes
     ids of graph2 by graph1.size. TODO incremental update of canonical root *)
  let disjoint_union g1 g2 =
    let shift  = (Graph.size g1) in
    (* add up nodes *)
    let info = Graph.NodeIdMap.fold (fun id2 { Graph.clr; adj; deg } map1 ->
      (* shift neighbour relationship *)
      let info2 = { Graph.clr;
                    deg;
                    adj = (List.map (fun (lc, id2') -> (lc, id2' + shift)) adj) 
                  } 
      in
      Graph.NodeIdMap.add (id2 + shift) info2 map1
    ) (Graph.info g2) (Graph.info g1) in
    { Graph.size = (Graph.size g1) + (Graph.size g2);
      info;
      (*buds = g1.buds @ buds2; *)
      root = (Graph.root g1)
    }

  (* TODO: incremental update of canonical root *)
  let merge g1 plug1 plug2 g2 =
    let g = disjoint_union g1 g2 in
    let ((v1, i1), l1) = plug1
    and ((v2, i2), l2) = plug2 in
    Graph.add_edge g v1 l1 (v2 + (Graph.size g1))
  (* update the root *)

  let compatible 
      ((v1, { Graph.clr = clr1; adj = adj1}), lc1) 
      ((v2, { Graph.clr = clr2; adj = adj2 }), lc2) =
    (LLab.compare lc1 lc2 = 0) && Gram.compatibility clr1 lc1 clr2
      
  let canonical g = ()
    
    
end

