open Prelude

type vertex = int

module type NodeIdMapType =
  sig

    type 'a t

    val empty : 'a t

    val find : vertex -> 'a t -> 'a

    val add : vertex -> 'a -> 'a t -> 'a t

    val iter : (vertex -> 'a -> unit) -> 'a t -> unit

    val fold : (vertex -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  end

(* Persistent integer map.
   For now, we use standard Ocaml maps. Possible more efficient alternatives are:
   . Patricia trees (JC Filliatre's for instance) -- Drop-in replacement
   . Peristent hash tables/AVLs, etc              -- See Reins or Core lib ?
   . Persistent arrays (assuming we are careful w.r.t. vertex naming)
*)

module AVLIdMap : NodeIdMapType =
  Map.Make(struct
    type t      = vertex
    let compare (x : vertex) (y : vertex) = 
      if x < y then -1
      else if x > y then 1
      else 0
  end)

(* Alternate implementation. Less efficient than map. *)
module ListIdMap : NodeIdMapType =
struct

  type 'a t = (vertex * 'a) list

  let empty = []

  let find = List.assoc

  let rec remove v a =
    match a with
    | [] -> []
    | ((v', x) as pair) :: tl -> 
      if v = v' then
        remove v tl
      else
        pair :: (remove v tl)

  let add v x a =
    if List.mem_assoc v a then
      (v, x) :: (remove v a)
    else
      (v, x) :: a

  let rec iter f a =
    match a with
    | [] -> ()
    | (v, x) :: tl -> 
      (f v x;
       iter f tl)

  let rec fold f a acc =
    match a with
    | [] -> acc
    | (v, x) :: tl -> 
      fold f tl (f v x acc)
        
end

(* Patricia tree maps. The most efficient so far. *)
module PatIdMap : NodeIdMapType = Ptmap

module NodeIdMap = PatIdMap

let v_of_int : int -> vertex = fun x -> x

let v_to_int : vertex -> int = fun x -> x


type ('n,'l) info = {
  (* Colour of node *)
  clr : 'n;
  (* Adjacency relation supposed symmetric (invariant to be mainted) *)
  adj : ('l * vertex) list;
  (* degree = |adj| *)
  deg : int
}

type ('n, 'l) t = {
  (* Total number of nodes. We assume the vertex set is of the shape [0; size-1], i.e.
     a contiguous set of integers starting from 0. *)
  size  : int;
  (* Map every node id to its info *)
  info  : ('n, 'l) info NodeIdMap.t;
}

let empty = {
  size    = 0;
  info    = NodeIdMap.empty;
}

let size { size } = size

let info { info } = info

let fold f g acc =
  NodeIdMap.fold (fun v info acc ->
    f v info.clr info.adj acc
  ) g.info acc

let get_info graph v =
  NodeIdMap.find v graph.info

let get_colour graph v =
  (NodeIdMap.find v graph.info).clr

let get_neighbours graph v =
  (NodeIdMap.find v graph.info).adj

(* TODO what to do with [buds] *)
let add_node_with_colour graph clr =
  let v = graph.size in
  let info = NodeIdMap.add v { clr; adj = []; deg = 0 } graph.info in
  { size = v + 1; info }, v

let add_node_with_colour2 graph clr =
  let v = graph.size in
  let info = NodeIdMap.add v { clr; adj = []; deg = 0 } graph.info in
  { size = v + 1; info }

let add_edge graph v1 l v2 =
  if v1 < graph.size && v2 < graph.size then
    let { adj = a1; deg = d1 } as i1 = get_info graph v1
    and { adj = a2; deg = d2 } as i2 = get_info graph v2 in
    let info = NodeIdMap.add v1 { i1 with adj = (l, v2) :: a1; deg = d1 + 1 } graph.info in
    let info = NodeIdMap.add v2 { i2 with adj = (l, v1) :: a2; deg = d2 + 1 } info in
    { graph with info }
  else
    failwith "add_edge: invalid arguments"

(* Attach two graphs by computing their disjoint union and merging node [bigv] to node [smallv]
   (in fact, smallv is replaced by bigv) *)
let graft ~big ~small ~bigv ~smallv =
  let big, map =
    fold (fun sv sclr _ (big, vmap) ->
      if sv = smallv then
        (big, (smallv, bigv) :: vmap)
      else
        let bv  = size big in
        let big = add_node_with_colour2 big sclr in
        (big, (sv, bv) :: vmap)
    ) small (big, [])
  in
  fold (fun sv _ sadj big ->
    List.fold_left (fun big (sl, sv') ->
      if sv <= sv' then
        let bv  = List.assoc sv map in
        let bv' = List.assoc sv' map in
        add_edge big bv sl bv'
      else
        big
    ) big sadj
  ) small big 

(* --------------------------- *)
(* Printing to DOT file format *)

let to_dot_aux file_desc graph_name graph print_node print_label =
  let print x   = Printf.fprintf file_desc x in
  print "graph %s {\n" graph_name;
    (* (\*  print "[overlap=false];\n"; *\) *)
    (* print "%d [shape=box];\n" m.i; *)
  NodeIdMap.iter (fun i elt ->
    let s = print_node elt.clr i in
    print "%d [label=%s];\n" i s
  ) graph.info;
  NodeIdMap.iter (fun i elt ->
    List.iter (fun (label, dest) ->
      let label = print_label label in
      print "%d -- %d [label=\"%s\"];\n" i dest label
    ) elt.adj
  ) graph.info;
  print "}\n"

let to_dot file_name graph_name graph print_node print_label =
  let file_desc = open_out file_name in
  to_dot_aux file_desc graph_name graph print_node print_label;
  close_out file_desc

let print graph print_node print_label =
  to_dot_aux stdout "" graph print_node print_label
