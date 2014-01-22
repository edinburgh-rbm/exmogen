open RootedTree

(* We assume [edges] is symmetric, connected and induces no cycles. 
   Moreover, we assume the vertex set is of the shape [0; size-1], i.e.
   a contiguous set of integers starting from 0. *)
type 'a t = {
  mutable size    : int;
  mutable edges   : (int, int list) Hashtbl.t;
  mutable colours : (int, 'a) Hashtbl.t
}

let empty () = {
  size    = 0;
  edges   = Hashtbl.create 50;
  colours = Hashtbl.create 50;
}

let get_colour tree v =
  Hashtbl.find tree.colours v

let get_neighbours tree v =
  Hashtbl.find tree.edges v

let add_vertex_with_colour tree clr =
  let v = tree.size in
  tree.size <- tree.size + 1;
  Hashtbl.add tree.edges v [];
  Hashtbl.add tree.colours v clr

let add_edge tree v1 v2 =
  if v1 < tree.size && v2 < tree.size then
    (let n1 = get_neighbours tree v1 in
     let n2 = get_neighbours tree v2 in
     Hashtbl.replace tree.edges v1 (v2 :: n1);
     Hashtbl.replace tree.edges v2 (v1 :: n2))
  else
    failwith "add_edge: invalid arguments"

(* Given a vertex, transform the unrooted tree into a rooted one. *)
let rec root tree v path =
  let c = get_colour tree v in
  let n = get_neighbours tree v in
  match path with
  | [] ->
    let subtrees = List.map (fun x -> root tree x (v :: path)) n in
    Node(c, subtrees)
  | v' :: _ ->
    let subtrees = List.fold_left (fun acc elt ->
      if elt = v' then acc
      else (root tree elt (v :: path)) :: acc
    ) [] n in
    Node(c, subtrees)

let root tree v = root tree v []

let to_adjacency_matrix tree =
  let a = Array.create_matrix tree.size tree.size 0 in
  for i = 0 to Array.length a - 1 do
    let n = get_neighbours tree i in
    let n = List.filter (fun x -> x > i) n in
    List.iter (fun j ->
      a.(i).(j) <- 1;
      a.(j).(i) <- 1
    ) n
  done;
  a

(* Do it the dense way. For small graphs this should be ok. *)
let compute_distances tree =
  let adj    = to_adjacency_matrix tree in
  let tmp    = Matrix.copy adj in
  let iteree = Matrix.copy adj in
  let (l, c) = Matrix.dim adj in
  let dist   = Matrix.copy adj in
  let updates = ref 0 in
  let depth   = ref 1 in
  while !updates < ((l-1) * c /2) do
    for i = 0 to l - 1 do
      for j = i+1 to c - 1 do
        if iteree.(i).(j) > 0 && tmp.(i).(j) = 0 && dist.(i).(j) = 0 then
          (dist.(i).(j) <- !depth;
           dist.(j).(i) <- !depth;
           updates := !updates + 2)
        else ()
      done
    done;
    Matrix.copy_in_place iteree tmp;
    Matrix.multiply iteree tmp adj;
    incr depth
  done;
  dist

let cumulative_distance tree =
  let dist = compute_distances tree in
  Array.init tree.size (fun i ->
    Array.fold_left (+) 0 dist.(i)
  )

(* minimal /indices/ in an integer array *)
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
let canonical_root label_compare tree =
  let c = cumulative_distance tree in
  let i = minimal_indices c in
  match i with
  | [] -> 
    failwith "less than 1 minimal indices - something is seriously wrong"
  | [x] ->
    (x, root tree x)
  | [x; y] ->
    let rx = sort label_compare (root tree x) in
    let ry = sort label_compare (root tree y) in
    let c  = tree_compare label_compare rx ry in
    if c = 1 then
      (y, ry)
    else
     (* when c = 0, isomorphic trees -- we don't care which one we return *)
      (x, rx)
  | _ ->
    failwith "less than 1 minimal indices - something is seriously wrong"    


(* tests *)

let test =
  let t = empty () in
  add_vertex_with_colour t 0;
  add_vertex_with_colour t 1;
  add_vertex_with_colour t 2;
  add_vertex_with_colour t 3;
  add_vertex_with_colour t 4;
  add_edge t 0 1;
  add_edge t 0 2;
  add_edge t 2 3;
  add_edge t 2 4;
  t


let a = to_adjacency_matrix test

let d = compute_distances test

let c = cumulative_distance test

let _ = Printf.printf "%s\n%!" (Matrix.print string_of_int d)

let _ =
  for i = 0 to Array.length c - 1 do
    Printf.printf "%d " c.(i)
  done;
  Printf.printf "\n"

let c = canonical_root compare test
