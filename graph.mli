type vertex = private int

val v_of_int : int -> vertex
val v_to_int : vertex -> int

module type NodeIdMapType =
  sig
    type 'a t
    val empty : 'a t
    val find : vertex -> 'a t -> 'a
    val add : vertex -> 'a -> 'a t -> 'a t
    val iter : (vertex -> 'a -> unit) -> 'a t -> unit
    val fold : (vertex -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  end

module NodeIdMap : NodeIdMapType

type ('n, 'l) info = { clr : 'n; adj : ('l * vertex) list; deg : int; }

type ('n, 'l) t = {
  size : int;
  info : ('n, 'l) info NodeIdMap.t;
}

val empty : ('a, 'b) t

val size : ('a, 'b) t -> int

val info : ('a, 'b) t -> ('a, 'b) info NodeIdMap.t

val fold : (vertex -> 'a -> ('b * vertex) list -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c

val get_info : ('a, 'b) t -> vertex -> ('a, 'b) info

val get_colour : ('a, 'b) t -> vertex -> 'a

val get_neighbours : ('a, 'b) t -> vertex -> ('b * vertex) list


val add_node_with_colour  : ('a, 'b) t -> 'a -> ('a, 'b) t * vertex

val add_node_with_colour2 : ('a, 'b) t -> 'a -> ('a, 'b) t

val add_edge : ('a, 'b) t -> vertex -> 'b -> vertex -> ('a, 'b) t

val graft : 
  big    : ('a, 'b) t -> 
  small  : ('a, 'b) t -> 
  bigv   : vertex -> 
  smallv : vertex -> 
  ('a, 'b) t

val exists_injection : ('a, 'b) t -> vertex -> ('a, 'b) t -> vertex -> bool

val to_dot_aux : out_channel -> string -> ('a, 'b) t -> ('a -> vertex -> string) -> ('b -> string) -> unit

val to_dot : string -> string -> ('a, 'b) t -> ('a -> vertex -> string) -> ('b -> string) -> unit

val print : ('a, 'b) t -> ('a -> vertex -> string) -> ('b -> string) -> unit
