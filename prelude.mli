module type Ordered =
  sig
    type t
    val compare : t -> t -> int
    val inhabited : t
    val print : t -> string
  end

val ( ++ ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

val to_sseq : ('a -> string) -> string -> 'a list -> string

val strof_ilist : int list -> string

val strof_iarr : int array -> string

val dup : 'a -> int -> 'a list

val mk_ints : int -> int -> int list

val list_max : 'a -> 'a list -> 'a

val list_min : 'a -> 'a list -> 'a

val filter_duplicates' : 'a list -> 'a list

val filter_duplicates : 'a list -> 'a list

val sections : 'a list list list -> 'a list list

val fsection : 'a list -> ('a -> 'b list) -> 'b list list

val fold_fsection :
  ('a -> 'b list) ->
  ('a -> 'b -> 'c -> 'c) -> ('c -> 'd -> 'd) -> 'a list -> 'c -> 'd -> 'd

val fold_fsection_tr :
  ('a -> 'b list) -> ('a -> 'b -> 'c -> 'c) -> 'a list -> 'c -> 'c list

val fold_cartesian : ('a -> 'b -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd

val take_until : 'a Queue.t -> ('a -> bool) -> 'a option

type timer = float ref

val create_timer : unit -> float ref

val start_timer : float ref -> unit

val reset_timer : float ref -> unit

val get_timer : float ref -> float



val log : string -> unit

val chop : int -> 'a list -> 'a list

val sort_uniq : ('a -> 'a -> int) -> 'a list -> 'a list



type 'a mset = ('a * int) list
