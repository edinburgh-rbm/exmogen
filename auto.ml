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
  
  type cell       = IntSet.t

  type partition  = cell list

  type dpartition = {
    dom   : partition;
    codom : partition
  }

  type exploration =
  | Leaf
  | Exhaustive   of dpartition list
  | CosetPruning of int * dpartition * dpartition list

  (* Used to quickly escape loops and nested recursions *)
  exception FastExit

  (* Used in coset pruning *)
  exception SolutionFound of (int array list)

  (* ------------------------------------------------------ *)
  (* Implementing Saucy-like automorphism group computation *)

  let make_root size =
    let l = mk_ints 0 (size-1) in
    let s = [List.fold_right IntSet.add l IntSet.empty] in
    { dom   = s;
      codom = s  }

  let print partition =
    let p x = Printf.printf "%d " (IntSet.choose x) in
    List.iter p partition.dom;
    Printf.printf "\n";
    List.iter p partition.codom;
    Printf.printf "\n"

  let rec cells_equal (a : cell) (b : cell) = IntSet.equal a b
    
  let rec partitions_isomorphic pa pb =
    match (pa, pb) with
    | [], [] -> true
    | [], _
    | _, [] -> false
    | a :: la, b :: lb ->
      (IntSet.cardinal a = IntSet.cardinal b) && partitions_isomorphic la lb
        
  let search_fixed_point domain_prefix domain_suffix codomain_prefix codomain_suffix cell cocell =
    let isect = IntSet.inter cell cocell in
    if IntSet.is_empty isect then
      let elt    = IntSet.choose cell in
      let fixed  = IntSet.singleton elt in
      let cell'  = IntSet.remove elt cell in
      let dom    = List.rev_append domain_prefix (fixed :: cell' :: domain_suffix) in
      let result = IntSet.fold (fun codomelt acc ->
        let cocell' = IntSet.remove codomelt cocell in
        let cofixed = IntSet.singleton codomelt in
        { dom   = dom;
          codom = List.rev_append codomain_prefix (cofixed :: cocell' :: codomain_suffix) } :: acc
      ) cocell [] in
      Exhaustive result
    else
      let elt   = IntSet.choose isect in
      let fixed = IntSet.singleton elt in
      let cell' = IntSet.remove elt cell in
      let dom   = List.rev_append domain_prefix (fixed :: cell' :: domain_suffix) in
      let cocell' = IntSet.remove elt cocell in
      let fixing_partition = { 
        dom   = dom; 
        codom = List.rev_append codomain_prefix (fixed :: cocell' :: codomain_suffix) } in
      let result = IntSet.fold (fun codomelt acc ->
        let cocell' = IntSet.remove codomelt cocell in
        let cofixed = IntSet.singleton codomelt in
        { dom   = dom;
          codom = List.rev_append codomain_prefix (cofixed :: cocell' :: codomain_suffix) } :: acc
      ) cocell' [] in
      CosetPruning(elt, fixing_partition, result)

  (* split first non-atomic cell, produce all possible childrens *)
  let rec produce_children domain codomain dacc cacc =
    match domain, codomain with
    | [], [] -> Leaf
    | dcell :: dtail, ccell :: ctail ->
      if IntSet.is_empty dcell && IntSet.is_empty ccell then
        failwith "Auto.produce_children: empty cells"
      else if IntSet.is_singleton dcell && IntSet.is_singleton ccell then
        produce_children dtail ctail (dcell :: dacc) (ccell :: cacc)
      else     
        search_fixed_point dacc dtail cacc ctail dcell ccell   
    | _ -> failwith "Auto.produce_children: partitions not matching in size"

  (* Convert a discrete partition to an array-based permutation.
   * Assumes discreteness of partition. *)
  let to_perm size partition =
    let arr = Array.create size 0 in
    try      
      List.iter2 (fun cell1 cell2 ->
        arr.(IntSet.choose cell1) <- IntSet.choose cell2
      ) partition.dom partition.codom;
      arr
    with Invalid_argument _ ->
      failwith "Auto.to_perm: partition has invalid size"


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

  let auto_count = ref Int64.zero
            
  let is_automorphism graph sigma =
    auto_count := Int64.add !auto_count Int64.one;
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

  let compute_edgecounts vertex view = compute_edgecounts vertex view Mset.empty

  (* The following code ignores the colours of edges. It is faster but distinguishes less
   * features of the graph. In simple experiments, it appears to be enough ... *)
(*
  let feature_compare ((nc, ld) : NLab.t * int) ((nc', ld') : NLab.t * int) =
    let c = NLab.compare nc nc' in
    if c = 0 then
      compare ld ld'
    else c

  let rec mem' key = function
    | [] -> false
    | (clr, x) :: tail ->
      x = key || mem' key tail

  let rec compute_edgecounts vertex view acc =
    match view with
    | [] -> acc
    | (v, info, count) :: tail ->
      if mem' vertex info.Graph.adj then
        compute_edgecounts vertex tail acc
      else
        (incr count;
         compute_edgecounts vertex tail (acc+1))

  let compute_edgecounts vertex view = compute_edgecounts vertex view 0
*)

  (* Compute local degrees of each vertex in a cell. *)
  (* Also, edge colours. We don't need just a count, we need a multiset of edge colours. *)
  let compute_local_view (graph : (NLab.t, LLab.t) Graph.t) cell =
    let result = IntSet.fold (fun vertex view ->
      let counter = compute_edgecounts vertex view in
      ((vertex, Graph.get_info graph vertex, ref counter) :: view) 
    ) cell [] in
    List.map (fun (v, i, c) -> (v, (i.Graph.clr, !c))) result

  let rec partition_views dom_view codom_view dom codom dom_cells codom_cells =
    match (dom_view, codom_view) with
    | (v, f) :: [], (v', f') :: [] ->
      if feature_compare f f' = 0 then
        Some ((IntSet.add v dom) :: dom_cells, (IntSet.add v' codom) :: codom_cells)
      else
        None
    | (u, f) :: ((v, g) :: _ as dom_tail), (u', f') :: ((v', g') :: _ as codom_tail) ->
      if feature_compare f f' = 0 then
        if feature_compare f g = 0 then
          (* stay in the same feature class: accumulate in current domain and codomain cells *)
          partition_views dom_tail codom_tail (IntSet.add u  dom) (IntSet.add u' codom) dom_cells codom_cells
        else
          (* Change feature class: push current cells in current dpartition *)
          partition_views dom_tail codom_tail IntSet.empty IntSet.empty ((IntSet.add u dom) :: dom_cells) ((IntSet.add u' codom) :: codom_cells)
      else
        None (* There is no way the input cell can yield an automorphism *)
    | _ -> failwith "Auto.partition_views: views have not the same size."


  (* Precondition: dom and codom have the same size. This function will
   * use information from the graph to split the cells into "equivalence" classes,
   * where two vertices are equivalent if they have the same color and the same
   * local degree. *)
  let refine_cell graph domc codomc domp codomp =
    let dom_view    = compute_local_view graph domc in
    let dom_view    = List.sort (fun (_, f) (_, f') -> feature_compare f f') dom_view in
    let codom_view  = compute_local_view graph codomc in
    let codom_view  = List.sort (fun (_, f) (_, f') -> feature_compare f f') codom_view in
    match partition_views dom_view codom_view IntSet.empty IntSet.empty domp codomp with
    | None -> raise FastExit
    | Some parts -> parts

  (* Precondition: same number of cells in domain and codomain partitions *)
  let rec refine_partition_aux graph domp codomp =
    match domp, codomp with
    | [], [] -> ([], [])
    | domcell :: domtail, codomcell :: codomtail ->
      let (dom', codom') = refine_partition_aux graph domtail codomtail in
      refine_cell graph domcell codomcell dom' codom'
    | _ ->
      failwith "Auto.refine_partition: partitions are ill-formed"

  let refine_partition graph dpartition =
    let (dom, codom) = refine_partition_aux graph dpartition.dom dpartition.codom in
    { dom; codom }

  let rec compute_automorphisms_aux graph root exhaustive acc =
    try 
      let root = refine_partition graph root in
      match produce_children root.dom root.codom [] [] with
      | Leaf -> (* discrete *)
        let root' = to_perm (Graph.size graph) root in
        if is_automorphism graph root' then
          if exhaustive then
            root' :: acc
          else
            raise (SolutionFound (root' :: acc))
        else
          acc
      | Exhaustive children ->
        List.fold_left (fun acc child ->
          compute_automorphisms_aux graph child exhaustive acc
        ) acc children
      | CosetPruning(fixedpoint, fixedpart, others) ->
        let acc = compute_automorphisms_aux graph fixedpart exhaustive acc in
        List.fold_left (fun acc child ->
          try
            compute_automorphisms_aux graph child false acc
          with
          | SolutionFound acc -> acc
        ) acc others
    with
      FastExit -> acc

  let timer = Prelude.create_timer ()
  let cmlt  = ref 0.0

  let compute_automorphisms graph =
    let _ = Prelude.start_timer timer in
    let res = compute_automorphisms_aux graph (make_root (Graph.size graph)) true [] in
    cmlt := !cmlt +. (Prelude.get_timer timer);
    res
    

end
