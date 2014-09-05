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

(* duplicate an elt i times *)
let rec dup elt i =
  if i < 0 then
    failwith "dup: count < 0"
  else if i = 0 then
    []
  else
    elt :: (dup elt (i-1))

(* list of integers from i to j *)
let rec mk_ints i j =
  if i > j then []
  else
    i :: (mk_ints (i+1) j)

(* max of a list *)
let rec list_max m = function
  | [] -> m
  | x :: l ->
    list_max (max m x) l

(* min of a list *)
let rec list_min m = function
  | [] -> m
  | x :: l ->
    list_max (min m x) l

let rec filter_duplicates' = function
  | [] -> []
  | [ x ] -> [ x ]
  | x :: y :: l ->
    if x = y then
      filter_duplicates' (y :: l)
    else
      x :: (filter_duplicates' (y :: l))

let filter_duplicates l =
  filter_duplicates' (List.sort compare l)

(* Transforms a list of words into all possible
   concatenations. I.e. take all the ordered
   sections. *)
let rec sections lists acc =
  match lists with
  | [] -> acc
  | words :: tail ->
    let res = List.fold_left (fun new_acc w ->
      List.fold_left (fun acc accw ->
        (accw @ w) :: acc
      ) new_acc acc
    ) [] words
    in
    sections tail res

let sections l = 
  sections l [[]]


let rec fsection_aux ll acc metacc =
  match ll with
  | [] -> (List.rev acc) :: metacc
  | l :: tll ->
    match l with
    | [] -> metacc
    | x :: tl ->
      let res = fsection_aux tll (x :: acc) metacc in
      fsection_aux (tl :: tll) acc res

let fsection : 'a list -> ('a -> 'b list) -> 'b list list =
  fun l f ->
  let lists = List.map f l in
  fsection_aux lists [] []

let rec fold_fsection gen f g l facc gacc =
  match l with
  | [] -> g facc gacc
  | e :: tll ->
    let l = gen e in
    List.fold_left (fun gacc x ->
      fold_fsection gen f g tll (f e x facc) gacc
    ) gacc l

let rec fold_fsection_aux gen f index acc =
  match index with
  | [] -> acc
  | i :: tl ->
    let col = gen i in
    let acc =
      List.fold_left (fun acc' x ->
        List.fold_left (fun acc' thread ->         
          (f i x thread) :: acc'            
        ) acc' acc
      ) [] col
    in
    fold_fsection_aux gen f tl acc

let fold_fsection_tr gen f index facc = fold_fsection_aux gen f index [facc]

let rec fold_cartesian f l1 l2 l2' acc =
  match l1 with
  | [] -> acc
  | x1 :: tl1 ->
    match l2' with
    | [] -> 
      fold_cartesian f tl1 l2 l2 acc
    | x2 :: tl2 ->
      let acc = f x1 x2 acc in
      fold_cartesian f l1 l2 tl2 acc

let fold_cartesian f l1 l2 acc = f l1 l2 l2 acc

  
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

(* Unix-based timer *)

type timer = float ref

let create_timer () = ref 0.0

let start_timer timer = timer := (Unix.gettimeofday ())

let reset_timer timer = timer := 0.0

let get_timer timer   = 
  let t = Unix.gettimeofday () in
  t -. !timer


(* Logging *)

let log x = Printf.printf "log: %s\n%!" x


let rec chop k l =
  if k = 0 then l else begin
    match l with
    | x::t -> chop (k-1) t
    | _ -> assert false
  end


let sort_uniq cmp l =
  let rec rev_merge l1 l2 accu =
    match l1, l2 with
    | [], l2 -> List.rev_append l2 accu
    | l1, [] -> List.rev_append l1 accu
    | h1::t1, h2::t2 ->
      let c = cmp h1 h2 in
      if c = 0 then rev_merge t1 t2 (h1::accu)
      else if c < 0
      then rev_merge t1 l2 (h1::accu)
      else rev_merge l1 t2 (h2::accu)
  in
  let rec rev_merge_rev l1 l2 accu =
    match l1, l2 with
    | [], l2 -> List.rev_append l2 accu
    | l1, [] -> List.rev_append l1 accu
    | h1::t1, h2::t2 ->
      let c = cmp h1 h2 in
      if c = 0 then rev_merge_rev t1 t2 (h1::accu)
      else if c > 0
      then rev_merge_rev t1 l2 (h1::accu)
      else rev_merge_rev l1 t2 (h2::accu)
  in
  let rec sort n l =
    match n, l with
    | 2, x1 :: x2 :: _ ->
      let c = cmp x1 x2 in
      if c = 0 then [x1]
      else if c < 0 then [x1; x2] else [x2; x1]
    | 3, x1 :: x2 :: x3 :: _ ->
      let c = cmp x1 x2 in
      if c = 0 then begin
        let c = cmp x2 x3 in
        if c = 0 then [x2]
        else if c < 0 then [x2; x3] else [x3; x2]
      end else if c < 0 then begin
        let c = cmp x2 x3 in
        if c = 0 then [x1; x2]
        else if c < 0 then [x1; x2; x3]
        else let c = cmp x1 x3 in
             if c = 0 then [x1; x2]
             else if c < 0 then [x1; x3; x2]
             else [x3; x1; x2]
      end else begin
        let c = cmp x1 x3 in
        if c = 0 then [x2; x1]
        else if c < 0 then [x2; x1; x3]
        else let c = cmp x2 x3 in
             if c = 0 then [x2; x1]
             else if c < 0 then [x2; x3; x1]
             else [x3; x2; x1]
      end
    | n, l ->
      let n1 = n asr 1 in
      let n2 = n - n1 in
      let l2 = chop n1 l in
      let s1 = rev_sort n1 l in
      let s2 = rev_sort n2 l2 in
      rev_merge_rev s1 s2 []
  and rev_sort n l =
    match n, l with
    | 2, x1 :: x2 :: _ ->
      let c = cmp x1 x2 in
      if c = 0 then [x1]
      else if c > 0 then [x1; x2] else [x2; x1]
    | 3, x1 :: x2 :: x3 :: _ ->
      let c = cmp x1 x2 in
      if c = 0 then begin
        let c = cmp x2 x3 in
        if c = 0 then [x2]
        else if c > 0 then [x2; x3] else [x3; x2]
      end else if c > 0 then begin
        let c = cmp x2 x3 in
        if c = 0 then [x1; x2]
        else if c > 0 then [x1; x2; x3]
        else let c = cmp x1 x3 in
             if c = 0 then [x1; x2]
             else if c > 0 then [x1; x3; x2]
             else [x3; x1; x2]
      end else begin
        let c = cmp x1 x3 in
        if c = 0 then [x2; x1]
        else if c > 0 then [x2; x1; x3]
        else let c = cmp x2 x3 in
             if c = 0 then [x2; x1]
             else if c > 0 then [x2; x3; x1]
             else [x3; x2; x1]
      end
    | n, l ->
      let n1 = n asr 1 in
      let n2 = n - n1 in
      let l2 = chop n1 l in
      let s1 = sort n1 l in
      let s2 = sort n2 l2 in
      rev_merge s1 s2 []
  in
  let len = List.length l in
  if len < 2 then l else sort len l
