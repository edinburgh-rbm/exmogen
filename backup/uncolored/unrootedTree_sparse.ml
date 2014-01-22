open RootedTree

(* We assume [edges] is symmetric, connected and induces no cycles. 
   Moreover, we assume the vertex set is of the shape [0; size-1], i.e.
   a contiguous set of integers starting from 0. *)
type 'a t = {
  size    : int;
  edges   : (int, int list) Hashtbl.t;
  colours : (int, 'a) Hashtbl.t
}

let get_colour tree v =
  Hashtbl.find tree.colours v

let get_neighbours tree v =
  Hashtbl.find tree.edges v

(* Given a vertex, transform the unrooted tree into a rooted one. *)
let rec root tree v =
  let c = get_colour tree v in
  let n = get_neighbours tree v in
  let subtrees = List.map (root tree) n in
  Node(c, subtrees)

(* Compute the distance between any two vertices - uniquely defined by
 * basic tree property. *)
let compute_dist tree i j =
  

(* Compute the distance matrix between any two vertices. *)
let compute_distances tree =
  let m = Array.create_matrix tree.size tree.size 0 in
  for i = 0 to tree.size - 1 do
    let n = get_neighbours tree i in
    let n = List.filter (fun x -> x > i) n in
    List.iter (fun j ->
      let d = compute_dist tree i j in
      m.(i).(j) <- d;
      m.(j).(i) <- d
    ) n
  done

(* Find the canonical root. *)
