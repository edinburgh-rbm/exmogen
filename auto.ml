open Prelude


module Make
  (NLab : Ordered)
  (LLab : Ordered) =
struct

  (* Automorphism group discovery for (undirected) simple 
   * colored graphs - should work for pretty much any kind 
   * of graph. *)


  (* ---------- *)
  (* Type decls *)
  
  type cell       = int list

  type partition  = cell list

  type dpartition = {
    dom   : partition;
    codom : partition
  }

  (* ------------------------------------------------------ *)
  (* Implementing Saucy-like automorphism group computation *)

  let make_root size =
    let l = mk_ints 0 (size-1) in
    {
      dom   = [l];
      codom = [l]
    }

  let print partition =
    let p = function
      | [x] -> Printf.printf "%d " x
      | _ -> failwith "Auto.print: partition not discrete"
    in
    List.iter p partition.dom;
    Printf.printf "\n";
    List.iter p partition.codom;
    Printf.printf "\n"

  let rec cells_equal (a : cell) (b : cell) =
    match (a, b) with
    | [], [] -> true
    | [], _
    | _, [] -> false
    | xa :: la, xb :: lb ->
      xa = xb && (cells_equal la lb)

  let rec partitions_isomorphic pa pb =  
    match (pa, pb) with
    | [], [] -> true
    | [], _
    | _, [] -> false
    | [_] :: la, [_] :: lb -> partitions_isomorphic la lb
    | a :: la, b :: lb ->
      (List.length a = List.length b) && partitions_isomorphic la lb

  (* generates all selections of elements in [codom], separating
     out the selected element and the rest of the list [codom]. 
     Produces a list of new [dpartition] pairs, with [domain] as the
     domain. *)
  let rec pick domain codomain_prefix codomain_suffix cell result zipped_cell =
    match cell with
    | [] -> result
    | image :: codom' ->
      let cell     = List.rev_append zipped_cell codom' in
      let codomain = List.rev_append codomain_prefix (cell :: [image] :: codomain_suffix) in
      let dparti   = { dom = domain; codom = codomain } in
      pick domain codomain_prefix codomain_suffix codom' (dparti :: result) (image :: zipped_cell)

  (* split first non-atomic cell, produce all possible childrens *)
  let rec produce_children domain codomain dacc cacc =
    match domain, codomain with
    | _, []
    | [], _ -> None
    | dcell :: dtail, ccell :: ctail ->
      match dcell, ccell with
      | [], [] -> failwith "Auto.produce_children: empty cells"
      | [_], [_] -> 
        produce_children dtail ctail (dcell :: dacc) (ccell :: cacc)
      | elt :: dcell', _ :: _ ->
        (* first non-singleton cell - split it *)
        let domain   = List.rev_append dacc (dcell' :: [elt] :: dtail) in
        Some (pick domain cacc ctail ccell [] [])
      | _ -> failwith "Auto.produce_children: cell not matching in size"

  (* Convert a discrete partition to an array-based permutation *)
  let to_perm size partition =
    let arr = Array.create size 0 in
    try
      List.iter2 (fun cell1 cell2 ->
        match cell1, cell2 with
        | [x], [y] -> arr.(x) <- y
        | _ -> 
          failwith "Auto.to_perm: partition is not discrete"
      ) partition.dom partition.codom;
      arr
    with Invalid_argument _ ->
      failwith "Auto.to_perm: partition has invalid size"

  exception FastExit

  (* Test whether a permutation presented as a discrete partition is 
   * a graph automorphism. This function assumes that the graph is
   * colored but /simple/. *)
  let rec is_automorphism_aux 
      (graph : (NLab.t, LLab.t) Graph.t) 
      (sigma : int array) 
      (queue : int list) 
      (explored : bool array) 
      =
    match queue with
    | [] -> ()
    | node :: queue ->
      if explored.(node) then
        is_automorphism_aux graph sigma queue explored
      else
        let info  = Graph.get_info graph node in
        let info' = Graph.get_info graph sigma.(node) in
        if info.Graph.clr = info'.Graph.clr && List.length info.Graph.adj = List.length info'.Graph.adj then
          (let queue = List.fold_left (fun acc (lab, n) ->
            if List.mem (lab, sigma.(n)) info'.Graph.adj then
              n :: acc
            else
              raise FastExit
           ) queue info.Graph.adj in
           explored.(node) <- true;
           is_automorphism_aux graph sigma queue explored)
        else
          raise FastExit
            
  let is_automorphism graph sigma =
    (* pick any node *)
    let (node, _) = Graph.NodeIdMap.choose graph.Graph.info in
    let visited   = Array.make (Array.length sigma) false in
    try
      (is_automorphism_aux graph sigma [node] visited;
       true)
    with
      FastExit -> false


  (* ------------------------------------- *)
  (* Implementation of partition refinment *)

  (* Instantiate multiset module for links *)
  module Mset = Prelude.Multiset(LLab)

  (* Total order on graph features *)
  let feature_compare ((nc, ld) : NLab.t * Mset.t) ((nc', ld') : NLab.t * Mset.t) =
    let c = NLab.compare nc nc' in
    if c = 0 then
      Mset.compare ld ld'        
    else c


  let rec mem_assoc' key = function
    | [] -> None
    | (clr, x) :: tail ->
      if x = key then Some clr
      else mem_assoc' key tail

  let rec compute_edgecounts vertex view acc =
    match view with
    | [] -> acc
    | (v, info, count) :: tail ->
      (match mem_assoc' vertex info.Graph.adj with
      | None -> 
        compute_edgecounts vertex tail acc
      | Some clr ->
        count := Mset.add clr !count; 
        compute_edgecounts vertex tail (Mset.add clr acc)
      )

  (* Compute local degrees of each vertex in a cell. *)
  (* TODO: edge colours. We don't need just a count, we need a multiset of edge colours. *)
  let rec compute_local_view (graph : (NLab.t, LLab.t) Graph.t) view cell =
    match cell with
    | [] -> List.map (fun (vertex, info, count) -> (vertex, (info.Graph.clr, !count))) view
    | vertex :: cell ->
      let mset  = compute_edgecounts vertex view Mset.empty in
      (*
      let count = List.fold_left (fun acc (v, info, count) ->
        if mem_assoc' vertex info.Graph.adj then
          (count := !count + 1;
           acc+1)
        else
          acc
      ) 0 view in *)
      compute_local_view graph ((vertex, Graph.get_info graph vertex, ref mset) :: view) cell

  let rec partition_views dom_view codom_view dom codom dom_cells codom_cells =
    match (dom_view, codom_view) with
    | (v, f) :: [], (v', f') :: [] ->
      if feature_compare f f' = 0 then
        Some ((v :: dom) :: dom_cells, (v' :: codom) :: codom_cells)
      else
        None
    | (u, f) :: ((v, g) :: _ as dom_tail), (u', f') :: ((v', g') :: _ as codom_tail) ->
      if feature_compare f f' = 0 then
        if feature_compare f g = 0 then
          partition_views dom_tail codom_tail (u :: dom) (u' :: codom) dom_cells codom_cells
        else
          partition_views dom_tail codom_tail [] [] ((u :: dom) :: dom_cells) ((u' :: codom) :: codom_cells)
      else
        None
    | _ -> failwith "Auto.partition_views: views have not the same size."


  (* Precondition: dom and codom have the same size. This function will
   * use information from the graph to split the cells into "equivalence" classes,
   * where two vertices are equivalent if they have the same color and the same
   * local degree. *)
  let refine_cell graph dom codom domp codomp =
    let dom_view    = compute_local_view graph [] dom in
    let dom_view    = List.sort (fun (_, f) (_, f') -> feature_compare f f') dom_view in
    let codom_view  = compute_local_view graph [] codom in
    let codom_view  = List.sort (fun (_, f) (_, f') -> feature_compare f f') codom_view in
    match partition_views dom_view codom_view [] [] domp codomp with
    | None -> raise FastExit
    | Some parts -> parts

  (* Precondition: same number of cells in domain and codomain partitions *)
  let rec refine_partition_aux graph dom codom =
    match dom, codom with
    | [], [] -> ([], [])
    | domcell :: domtail, codomcell :: codomtail ->
      let (dom', codom') = refine_partition_aux graph domtail codomtail in
      refine_cell graph domcell codomcell dom' codom'
    | _ ->
      failwith "Auto.refine_partition: partitions are ill-formed"

  let refine_partition graph dpartition =
    let (dom, codom) = refine_partition_aux graph dpartition.dom dpartition.codom in
    { dom; codom }
w
  let rec compute_automorphisms_aux graph root acc =
    try 
      let root = refine_partition graph root in
      match produce_children root.dom root.codom [] [] with
      | None -> (* discrete *)
        let root' = to_perm (Graph.size graph) root in
        if is_automorphism graph root' then
          root' :: acc
        else
          acc
      | Some children ->      
        List.fold_left (fun acc child ->
          compute_automorphisms_aux graph child acc
        ) acc children
    with
      FastExit -> acc

  let timer = Prelude.create_timer ()
  let cmlt  = ref 0.0

  let compute_automorphisms graph =
    let _ = Prelude.start_timer timer in
    let res = compute_automorphisms_aux graph (make_root (Graph.size graph)) [] in
    cmlt := !cmlt +. (Prelude.get_timer timer);
    res
    

end
