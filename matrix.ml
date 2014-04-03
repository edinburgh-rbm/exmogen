(* matrix-related stuff *)

type t = int array array

(* Use a custom allocator to cut down on allocations *)

let allocated : (int * int, t) Hashtbl.t = Hashtbl.create 30

let create : int -> int -> t = fun l c -> Array.create_matrix l c 0

let alloc l c =
  let key = (l, c) in
  if Hashtbl.mem allocated key then
    (let x = Hashtbl.find allocated key in
     Hashtbl.remove allocated key;
     x)
  else
    create l c

let free (m : t) =
  let key = (Array.length m, Array.length m.(0)) in
  for i = 0 to Array.length m - 1 do
    for j = 0 to Array.length m.(0) - 1 do
      m.(i).(j) <- 0
    done
  done;
  Hashtbl.add allocated key m

let alloc l c = create l c
  (* let key = (l, c) in *)
  (* if Hashtbl.mem allocated key then *)
  (*   (let x = Hashtbl.find allocated key in *)
  (*    Hashtbl.remove allocated key; *)
  (*    x) *)
  (* else *)
  (*   create l c *)

let free (m : t) = ()
  (* let key = (Array.length m, Array.length m.(0)) in *)
  (* for i = 0 to Array.length m - 1 do *)
  (*   for j = 0 to Array.length m.(0) - 1 do *)
  (*     m.(i).(j) <- 0 *)
  (*   done *)
  (* done; *)
  (* Hashtbl.add allocated key m *)

        
(* let create_elt l c init = Array.create_matrix l c init *)

(* let create_init l c f =  *)
(*   let m = create l c in *)
(*   for i = 0 to Array.length m - 1 do *)
(*     for j = 0 to Array.length m.(0) - 1 do *)
(*       m.(i).(j) <- f i j *)
(*     done *)
(*   done; *)
(*   m *)

(* dimensions of the matrix, lines \times columns *)
let dim a =
  (Array.length a, Array.length a.(0))

let is_zero a =
  let acc = ref true in
  for i = 0 to Array.length a - 1 do
    for j = 0 to Array.length a - 1 do
      acc := !acc && a.(i).(j) = 0
    done
  done;
  !acc

let is_positive a =
  let acc = ref true in
  for i = 0 to Array.length a - 1 do
    for j = 0 to Array.length a - 1 do
      acc := !acc && a.(i).(j) >= 0
    done
  done;
  !acc

let init a elt =
  for i = 0 to Array.length a - 1 do
    let ai = a.(i) in
    for j = 0 to Array.length a.(0) - 1 do
      ai.(j) <- elt
    done
  done

(* a <- a + b *)
let add_into a b =
  if dim a <> dim b then
    failwith "matrix_add_into: invalid arguments"
  else
    for i = 0 to Array.length a - 1 do
      for j = 0 to Array.length a - 1 do
        a.(i).(j) <- a.(i).(j) + b.(i).(j)
      done
    done
   
(* a <- b * c  -- assumes well-sized square matrices *)
let multiply a b c =
  let len = Array.length a in
  for i = 0 to len - 1 do
    for j = 0 to len - 1 do
      a.(i).(j) <- 0;
      for k = 0 to len - 1 do
        a.(i).(j) <- a.(i).(j) + b.(i).(k) * c.(k).(j)
      done
    done
  done

(* a <- b * c  -- assumes well-sized square matrices *)
let multiply a b c =
  let len = Array.length a in
  for i = 0 to len - 1 do
    let ai = a.(i) in
    let bi = b.(i) in
    for j = 0 to len - 1 do
      ai.(j) <- 0;
      for k = 0 to len - 1 do
        ai.(j) <- ai.(j) + bi.(k) * c.(k).(j)
      done
    done
  done


let copy_in_place a b =
  let (l, c) = dim a in
  for i = 0 to l - 1 do
    for j = 0 to c - 1 do
      b.(i).(j) <- a.(i).(j)
    done
  done

let copy a =
  let (l, c) = dim a in
  let b = create l c in
  copy_in_place a b;
  b

let multiply' b c =
  let a = copy b in
  multiply a b c;
  a

let print f m =
  Array.fold_left (fun acc line ->
    let line = Array.map f line in
    Array.fold_left (fun acc elt ->
      acc^"  "^elt
    ) (acc^"\n") line
  ) "" m
