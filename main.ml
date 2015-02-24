open Chemistry

let c = Atom.({ atom = C; arity = 4 })
let h = Atom.({ atom = H; arity = 1 })
let o = Atom.({ atom = O; arity = 2 })
let p = Atom.({ atom = P; arity = 1 })

(* Oxydation *)
let r_ch2_oh, hook1 =
  let g, c  = Molecule.add_node_with_colour Molecule.empty c in
  let g, o  = Molecule.add_node_with_colour g o in
  let g, h1 = Molecule.add_node_with_colour g h in
  let g, h2 = Molecule.add_node_with_colour g h in
  let g, h3 = Molecule.add_node_with_colour g h in
  let g     = Molecule.add_edge g c Link.Simple h1 in
  let g     = Molecule.add_edge g c Link.Simple h2 in
  let g     = Molecule.add_edge g c Link.Simple o in
  (Molecule.add_edge g o Link.Simple h3, c)

let r_cho, hook2 =
  let g, c  = Molecule.add_node_with_colour Molecule.empty c in
  let g, o  = Molecule.add_node_with_colour g o in
  let g, h  = Molecule.add_node_with_colour g h in
  let g     = Molecule.add_edge g c Link.Double o in
  (Molecule.add_edge g c Link.Simple h, c)

let oxidation1 = {
  input  = r_ch2_oh;
  output = r_cho;
  mapping = [ (hook1, hook2) ]
}

let carbon, cv =
  let g = Molecule.empty in
  Molecule.add_node_with_colour g c

let hydrogen, hv =
  let g = Molecule.empty in
  Molecule.add_node_with_colour g h

let oxygen : Molecule.t =
  let g = Molecule.empty in
  fst (Molecule.add_node_with_colour g o)

let phosphate : Molecule.t =
  let g = Molecule.empty in
  fst (Molecule.add_node_with_colour g p)

let seed = carbon

let mset5 : Molecule.t Prelude.mset =
  [ (carbon, 5);
    (hydrogen, 20);
    (oxygen, 10);
    (phosphate, 2);
  ]

let mset4 : Molecule.t Prelude.mset =
  [ (carbon, 4);
    (hydrogen, 20);
    (oxygen, 10);
    (phosphate, 2);
  ]

let mset3 : Molecule.t Prelude.mset =
  [ (carbon, 3);
    (hydrogen, 20);
    (oxygen, 10);
    (phosphate, 2);
  ]

let mset2 : Molecule.t Prelude.mset =
  [ (carbon, 2);
    (hydrogen, 20);
    (oxygen, 10);
    (phosphate, 2);
  ]

let bartek3 : Molecule.t Prelude.mset =
  [ (carbon, 2);
    (hydrogen, 20);
    (oxygen, 3);
    (phosphate, 3);
  ]

let bartek4 : Molecule.t Prelude.mset =
  [ (carbon, 3);
    (hydrogen, 20);
    (oxygen, 4);
    (phosphate, 4);
  ]


let print result name =
  let fd = open_out name in
  List.iter
    (List.iter (fun { input; output } ->
      let smiles_in  = match Chemistry.to_smiles input  with None -> failwith "Error in SMILES output" | Some x -> x in
      let smiles_out = match Chemistry.to_smiles output with None -> failwith "Error in SMILES output" | Some x -> x in
      Printf.fprintf fd "%s + NAD <--> %s + NADH\n" smiles_in smiles_out
     ))
    result;
  close_out fd

let mkprinter name =
  let fd = open_out name in
  let write = fun { input; output } ->
      let smiles_in  = match Chemistry.to_smiles input  with None -> failwith "Error in SMILES output" | Some x -> x in
      let smiles_out = match Chemistry.to_smiles output with None -> failwith "Error in SMILES output" | Some x -> x in
      Printf.fprintf fd "%s + NAD <--> %s + NADH\n" smiles_in smiles_out
  in
  (fd, write)

let mkprinterbartek name =
  let fd = open_out name in
  let write = fun mol ->
      let smiles = match Chemistry.to_smiles mol with None -> failwith "Error in SMILES output" | Some x -> x in
      Printf.fprintf fd "%s\n" smiles
  in
  (fd, write)


(* Straightforward molecule generation test. *)
(*
let _ =
  let fd, wr = mkprinterbartek "bartek3COP.mol" in
  let seed, hook = Molecule.add_node_with_colour Molecule.empty c in
  let _ = Chemistry.enumerate seed bartek3 wr in
  close_out fd


let _ =
  let fd, wr = mkprinterbartek "bartek4COP.mol" in
  let seed, hook = Molecule.add_node_with_colour Molecule.empty c in
  let _ = Chemistry.enumerate seed bartek4 wr in
  close_out fd
*)

(* generating reactions *)

let _ =
  let fd, wr = mkprinter "oxy_3C.mol" in
  let result = Chemistry.instantiate_schemes [ oxidation1 ] mset3 wr in
  Printf.printf "Generated %d instances for size 2 radical in %f seconds\n%!" 0 0.0

(* let _ = *)
(*   let fd, wr = mkprinter "oxy_3C.mol" in *)
(*   let result = Chemistry.instantiate_schemes [ oxidation1 ] mset3 wr in *)
(*   Printf.printf "Generated %d instances for size 2 radical in %f seconds\n%!" 0 0.0 *)

(* let _ = *)
(*   let fd, wr = mkprinter "oxy_3C.mol" in *)
(*   let _ = Chemistry.instantiate_schemes [ oxidation1 ] mset3 wr in *)
(*   close_out fd *)

(* let _ = *)
(*   let _      = Prelude.reset_timer timer in *)
(*   let _      = Prelude.start_timer timer in *)
(*   let result = Chemistry.instantiate_schemes [ oxidation1 ] mset2 in *)
(*   let time   = Prelude.get_timer timer in *)
(*   let size   = List.length (List.hd result) in *)
(*   let _      = Printf.printf "Generated %d instances for size 2 radical in %f seconds\n%!" size time in *)
(*   print result "oxy_2C.mol" *)


(* let _ = *)
(*   let _      = Prelude.reset_timer timer in *)
(*   let _      = Prelude.start_timer timer in *)
(*   let result = Chemistry.instantiate_schemes [ oxidation1 ] mset3 in *)
(*   let time   = Prelude.get_timer timer in *)
(*   let size   = List.length (List.hd result) in *)
(*   let _      = Printf.printf "Generated %d instances for size 3 radical in %f seconds\n%!" size time in *)
(*   print result "oxy_3C.mol" *)


(* let _ = *)
(*   let _      = Prelude.reset_timer timer in *)
(*   let _      = Prelude.start_timer timer in *)
(*   let result = Chemistry.instantiate_schemes [ oxidation1 ] mset4 in *)
(*   let time   = Prelude.get_timer timer in *)
(*   let size   = List.length (List.hd result) in *)
(*   let _      = Printf.printf "Generated %d instances for size 4 radical in %f seconds\n%!" size time in *)
(*   print result "oxy_4C.mol" *)


(* let _ = *)
(*   let _      = Prelude.reset_timer timer in *)
(*   let _      = Prelude.start_timer timer in *)
(*   let result = Chemistry.instantiate_schemes [ oxidation1 ] mset5 in *)
(*   let time   = Prelude.get_timer timer in *)
(*   let size   = List.length (List.hd result) in *)
(*   let _      = Printf.printf "Generated %d instances for size 5 radical in %f seconds\n%!" size time in *)
(*   print result "oxy_5C.mol" *)


(* let result = Generator.enumerate seed mset Generator.Canonical.empty *)
(* let result = Generator.Canonical.elements result *)

(* let s = List.map (fun (res, _) -> Molecule.R.print (Molecule.root res (Graph.v_of_int 0))) result *)
(* let _ = List.iter (Printf.printf "%s\n" ) s *)

(* let _ = *)
(*   List.iter (fun (g, _) ->  *)
(*     match to_smiles (Unrooted.root g 0) with  *)
(*     | None   -> ()  *)
(*     | Some s ->  *)
(*       Printf.printf "%s\n" s *)
(*   ) result *)


(* let time   = Prelude.get_timer timer *)

(* let result = G.Canonical.elements result *)

(* (\* hydrogen pattern *\) *)
(* let hydrogen = *)
(*   let g = Unrooted.empty in *)
(*   Unrooted.add_node_with_colour g Atom.H *)

(* (\* oxygen pattern *\) *)
(* let oxygen = *)
(*   let g = Unrooted.empty in *)
(*   Unrooted.add_node_with_colour g Atom.O *)

(* (\* phosphate pattern *\) *)
(* let phosphate = *)
(*   let g = Unrooted.empty in *)
(*   Unrooted.add_node_with_colour g Atom.P *)

(* let (>>=) g f = fun x -> f g x *)

(* (\* -CH3 *\) *)
(* let ch3 = *)
(*   let g = Unrooted.empty in *)
(*   let g = Unrooted.add_node_with_colour g Atom.C in *)
(*   let g = Unrooted.add_node_with_colour g Atom.H in *)
(*   let g = Unrooted.add_node_with_colour g Atom.H in *)
(*   let g = Unrooted.add_node_with_colour g Atom.H in *)
(*   let g = Graph.add_edge g 0 Link.Simple 1 in *)
(*   let g = Graph.add_edge g 0 Link.Simple 2 in *)
(*   Graph.add_edge g 0 Link.Simple 3 *)

(* let ch2p = *)
(*   let g = Unrooted.empty in *)
(*   let g = Unrooted.add_node_with_colour g Atom.C in *)
(*   let g = Unrooted.add_node_with_colour g Atom.H in *)
(*   let g = Unrooted.add_node_with_colour g Atom.H in *)
(*   let g = Unrooted.add_node_with_colour g Atom.P in *)
(*   let g = Graph.add_edge g 0 Link.Simple 1 in *)
(*   let g = Graph.add_edge g 0 Link.Simple 2 in *)
(*   Graph.add_edge g 0 Link.Simple 3 *)

(* let ch2oh = *)
(*   let g = Unrooted.empty in *)
(*   let g = Unrooted.add_node_with_colour g Atom.C in *)
(*   let g = Unrooted.add_node_with_colour g Atom.H in *)
(*   let g = Unrooted.add_node_with_colour g Atom.H in *)
(*   let g = Unrooted.add_node_with_colour g Atom.O in *)
(*   let g = Unrooted.add_node_with_colour g Atom.H in *)
(*   let g = Graph.add_edge g 0 Link.Simple 1 in *)
(*   let g = Graph.add_edge g 0 Link.Simple 2 in *)
(*   let g = Graph.add_edge g 0 Link.Simple 3 in *)
(*   Graph.add_edge g 3 Link.Simple 4 *)

(* let cooh = *)
(*   let g = Unrooted.empty in *)
(*   let g = Unrooted.add_node_with_colour g Atom.C in *)
(*   let g = Unrooted.add_node_with_colour g Atom.O in *)
(*   let g = Unrooted.add_node_with_colour g Atom.O in *)
(*   let g = Unrooted.add_node_with_colour g Atom.H in *)
(*   let g = Graph.add_edge g 0 Link.Double 1 in *)
(*   let g = Graph.add_edge g 0 Link.Simple 2 in *)
(*   Graph.add_edge g 2 Link.Simple 3 *)
    
(* let cho = *)
(*   let g = Unrooted.empty in *)
(*   let g = Unrooted.add_node_with_colour g Atom.C in *)
(*   let g = Unrooted.add_node_with_colour g Atom.H in *)
(*   let g = Unrooted.add_node_with_colour g Atom.O in *)
(*   let g = Graph.add_edge g 0 Link.Simple 1 in *)
(*   Graph.add_edge g 0 Link.Double 2 *)

(* let cop = *)
(*   let g = Unrooted.empty in *)
(*   let g = Unrooted.add_node_with_colour g Atom.C in *)
(*   let g = Unrooted.add_node_with_colour g Atom.O in *)
(*   let g = Unrooted.add_node_with_colour g Atom.P in *)
(*   let g = Graph.add_edge g 0 Link.Double 1 in *)
(*   Graph.add_edge g 0 Link.Simple 2 *)

(* let ch2 = *)
(*   let g = Unrooted.empty in *)
(*   let g = Unrooted.add_node_with_colour g Atom.C in *)
(*   let g = Unrooted.add_node_with_colour g Atom.H in *)
(*   let g = Unrooted.add_node_with_colour g Atom.H in *)
(*   let g = Graph.add_edge g 0 Link.Simple 1 in *)
(*   Graph.add_edge g 0 Link.Simple 2 *)

(* let ch_oh = *)
(*   let g = Unrooted.empty in *)
(*   let g = Unrooted.add_node_with_colour g Atom.C in *)
(*   let g = Unrooted.add_node_with_colour g Atom.H in *)
(*   let g = Unrooted.add_node_with_colour g Atom.O in *)
(*   let g = Unrooted.add_node_with_colour g Atom.H in *)
(*   let g = Graph.add_edge g 0 Link.Simple 1 in *)
(*   let g = Graph.add_edge g 0 Link.Simple 2 in *)
(*   Graph.add_edge g 2 Link.Simple 3 *)

(* let co = *)
(*   let g = Unrooted.empty in *)
(*   let g = Unrooted.add_node_with_colour g Atom.C in *)
(*   let g = Unrooted.add_node_with_colour g Atom.O in *)
(*   Graph.add_edge g 0 Link.Double 1 *)

(* let chp = *)
(*   let g = Unrooted.empty in *)
(*   let g = Unrooted.add_node_with_colour g Atom.C in *)
(*   let g = Unrooted.add_node_with_colour g Atom.H in *)
(*   let g = Unrooted.add_node_with_colour g Atom.P in *)
(*   let g = Graph.add_edge g 0 Link.Simple 1 in *)
(*   Graph.add_edge g 0 Link.Simple 2 *)

(* let ch = *)
(*   let g = Unrooted.empty in *)
(*   let g = Unrooted.add_node_with_colour g Atom.C in *)
(*   let g = Unrooted.add_node_with_colour g Atom.H in *)
(*   Graph.add_edge g 0 Link.Simple 1 *)

(* let c_oh = *)
(*   let g = Unrooted.empty in *)
(*   let g = Unrooted.add_node_with_colour g Atom.C in *)
(*   let g = Unrooted.add_node_with_colour g Atom.O in *)
(*   let g = Unrooted.add_node_with_colour g Atom.H in *)
(*   let g = Graph.add_edge g 0 Link.Simple 1 in *)
(*   Graph.add_edge g 1 Link.Simple 2 *)

(* let cp = *)
(*   let g = Unrooted.empty in *)
(*   let g = Unrooted.add_node_with_colour g Atom.C in *)
(*   let g = Unrooted.add_node_with_colour g Atom.P in *)
(*   Graph.add_edge g 0 Link.Simple 1 *)

(* let mset = *)
(*   [ (\*(carbon, 0); *\) *)
(*     (ch3, 4); *)
(*     (ch2p, 4); ] *)
(*     (\* (ch2oh, 1); *\) *)
(*     (\* (cooh, 1); *\) *)
(*     (\* (cho, 1); *\) *)
(*     (\* (cop, 1); *\) *)
(*     (\* (ch2, 1); *\) *)
(*     (\* (ch_oh, 1); *\) *)
(*     (\* (co, 1); *\) *)
(*     (\* (chp, 1); *\) *)
(*     (\* (ch, 1); *\) *)
(*     (\* (c_oh, 1); *\) *)
(*     (\* (cp, 1) ] *\) *)

(* (\* *)
(* -CH=    -C(OH)=    -CP= *)
(* =C= *)
(* *\) *)

(* (\* We allow to graft up to & carbons and 16 hydrogens on the seed *\) *)
(* (\* *)
(* let mset = *)
(*   [ (carbon, 3); (hydrogen, 10); (oxygen, 4); (phosphate, 4); ] *)
(* *\) *)


(* let timer  = Prelude.create_timer () *)
(* let _      = Prelude.start_timer timer *)
    
(* (\* Peform the actual enumeration *\) *)
(* let result = G.enumerate seed mset G.Canonical.empty *)

(* let time   = Prelude.get_timer timer *)

(* let result = G.Canonical.elements result *)

(* let s = List.map (fun (res, _) ->  Unrooted.R.print (Unrooted.root res 0)) result  *)
(* (\* let _ = List.iter (Printf.printf "%s\n" ) s *\) *)


(* let _ = *)
(*   List.iter (fun (g, _) ->  *)
(*     match to_smiles (Unrooted.root g 0) with  *)
(*     | None   -> ()  *)
(*     | Some s ->  *)
(*       Printf.printf "%s\n" s *)
(*   ) result *)

(* let _ =   *)
(*   Printf.printf "generation time: %f seconds\n" time *)

(* let _ =   *)
(*   Printf.printf "cumultative time spent in automorphism computation: %f seconds\n" (!Unrooted.Auto.cmlt) *)

(* let _ =   *)
(*   Printf.printf "number of automorphism checks: %s\n" (Int64.to_string !Unrooted.Auto.auto_count) *)

(* What follows is debugging code, do not read *)

(*
let _ = 
  Printf.printf "max auto count: %d\n%!" !Unrooted.m
*)

(* -----------------------------
   Test automorphism detection *)

(* module Auto = Auto.Make *)
(*   (struct type t = int     *)
(*           let compare = compare  *)
(*           let print = string_of_int *)
(*           let inhabited = 0 *)
(*    end) *)
(*   (struct type t = string  *)
(*           let compare = String.compare *)
(*           let print x = x *)
(*           let inhabited = "" *)
(*    end) *)
  
(* let graph =  *)
(*   let g = Graph.empty in *)
(*   let g = Graph.add_node_with_colour g 1 in *)
(*   let g = Graph.add_node_with_colour g 0 in *)
(*   let g = Graph.add_node_with_colour g 0 in *)
(*   let g = Graph.add_node_with_colour g 0 in *)
(*   let g = Graph.add_edge g 0 "" 1 in *)
(*   let g = Graph.add_edge g 1 "" 2 in *)
(*   let g = Graph.add_edge g 2 "" 3 in *)
(*   Graph.add_edge g 3 "" 0 *)

(* let automorphisms = Auto.compute_automorphisms graph *)

(* let _ = List.iter (fun x -> Printf.printf "%s\n" (Perm.ArrayBased.print x)) (Prelude.filter_duplicates automorphisms) *)

(* let _ =   *)
(*   Printf.printf "number of automorphism checks: %s\n" (Int64.to_string !Auto.auto_count) *)

(* let _ =   *)
(*   Printf.printf "cumultative time spent in automorphism computation: %f seconds\n" (!Auto.cmlt) *)


(*
let _ =
  Perm.ArrayBased.size := 3

module PermTest = Bsgs.Make2(Perm.ArrayBased)

open PermTest


let generators = 
  [ [| 1; 0; 2 |];
    [| 0; 2; 1 |]
      
  ]

let partial_bsgs = compute_partial_subgroup_chain generators

let _ = Prelude.log (print partial_bsgs)
  
let _ = schreier_sims_aux partial_bsgs.chain 0
let _ = Prelude.log "----------"
let _ = schreier_sims_aux partial_bsgs.chain 1

let _ =
  let g = (Perm.of_concrete [| 1; 0; 2 |]) in
  match strip partial_bsgs.chain 1 g with
  | Ok w ->
    let w = Prelude.to_sseq Perm.print "." w in 
    Prelude.log (Printf.sprintf "** for gen %s, decomposition %s" (Perm.print g) w)
  | DropOut(i, residue) ->
    Prelude.log (Printf.sprintf "** for gen %s, residue %s at %d" (Perm.print g) (Perm.print residue) i)

(*let _ = schreier_sims_aux partial_bsgs.chain 0*)

let _ = Prelude.log (print partial_bsgs)
*)
