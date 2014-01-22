module A :
  sig
    type t = private int

    val of_int : int -> t


  end =
  struct
    
    type t = int

    let of_int x = x

  end

let x = A.of_int 3

let y = [| 0; 1; 2; 3 |].((x :> int))
