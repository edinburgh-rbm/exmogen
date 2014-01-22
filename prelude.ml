(* (Totally) ordered printable structure. *)
module type Ordered =
sig

  type t

  val compare : t -> t -> int

    (* Some element from the ordered set. We do not expect anything
       of this element, besides it being fixed. *)
  val inhabited : t

  val print : t -> string

end

(* function composition f;g *)
let (++) f g = fun x -> g (f x)

(* Pretty printing of lists & arrays *)
let rec to_sseq f sep l =
  match l with
  | [] -> ""
  | [x] -> f x
  | x :: tl ->
    let res = List.fold_right (fun elt acc ->
      sep ^ (f elt) ^ acc
    ) tl "" in
    (f x) ^ res

let strof_ilist = to_sseq string_of_int ","

let strof_iarr x = strof_ilist (Array.to_list x)

let rec dup elt i =
  if i < 0 then
    failwith "dup: count < 0"
  else if i = 0 then
    []
  else
    elt :: (dup elt (i-1))

let rec mk_ints i j =
  if i > j then []
  else
    i :: (mk_ints (i+1) j)


(* Integer map *)

module IntMap =
  Map.Make(struct

    type t = int

    let compare x y =
      if x < y then -1
      else if x > y then 1
      else 0

  end)

(* Dynamic array - exponential reallocator *)
  
module DynArray =
struct

  type 'a t = {
    mutable fill : int;
    mutable arr  : 'a array;
    default      : 'a
  }

  (* assumes full non-empty array in order to get default element *)
  let realloc ({ fill; arr; default } as a) =
    let size     = Array.length arr in
    let new_size = size * 2 + 1 in
    let arr'     = Array.create new_size default in
    (* copy old content to new array *)
    for i = 0 to fill - 1 do
      arr'.(i) <- arr.(i)
    done;
    a.arr <- arr'

  let create sz default = { 
    fill = 0;
    arr  = Array.create sz default;
    default = default
  }

  let get_unsafe { arr } i = arr.(i)

  let get { fill; arr } i =
    if i < fill then
      arr.(i)
    else
      failwith "DynArray.get: out of bounds"

  let set { fill; arr } i x =
    if i < fill then
      arr.(i) <- x
    else
      failwith "Prelude.DynArray.set: index out of range"

  let append a elt =
    if a.fill = Array.length a.arr then
      realloc a
    else ();
    a.arr.(a.fill) <- elt;
    a.fill <- a.fill + 1

end

(* Stuff on queues *)

(* Take the first element, if any, that verifies
   a predicate. *)
let rec take_until queue pred =
  if Queue.is_empty queue then
    None
  else
    let elt = Queue.take queue in
    if pred elt then
      Some elt
    else
      take_until queue pred

(* Stuff on dllists *)
let rec dllist_of_array array =
  if Array.length array = 0 then
    failwith "Prelude.dllist_of_array: empty array"
  else
    let node = Dllist.create array.(0) in
    let acc  = ref node in
    for i = 1 to Array.length array - 1 do
      acc := Dllist.append !acc array.(i)
    done;
    node

(* Subarray module: accessing a contiguous subset of an array *)
module Subarray =
  struct
    
    type 'a t = {
      mutable left  : int; (* inclusive *) 
      mutable right : int; (* inclusive *)
      data          : 'a array
    }

    let create sz elt = {
      left  = 0;
      right = sz - 1;
      data  = Array.create sz elt
    }

    let data { data } = data
      
    let left { left } = left

    let right { right } = right

    let is_empty a =
      a.left > a.right

    let length a = a.right - a.left + 1

    let first a = a.data.(a.left)

    let set_first a x = a.data.(a.left) <- x

    let last a  = a.data.(a.right)

    let set_last a x = a.data.(a.right) <- x

    let push_left a =
      a.left <- a.left + 1

    let pull_right a =
      a.right <- a.right - 1

    let of_array a = {
      left  = 0;
      right = Array.length a - 1;
      data  = Array.copy a
    }

    let map f a = 
      { a with data = Array.map f a.data }

  end
