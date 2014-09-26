#ocamlc -c -g -dtypes float3.ml mat3.ml springmodel.ml utils.ml
#ocamlc -c graph.mli
#ocamlc -g -dtypes -I /home/piotr/.opam/4.00.1/lib/ocaml graphics.cma float3.ml mat3.ml springmodel.ml utils.ml graph.ml generation.ml display.ml smiles.ml cycles.ml main.ml -o genmol

# ocamlopt -g -o main -annot unix.cmxa dllist.ml bitv.ml prelude.ml rootedTree.ml matrix.ml group.ml perm.ml bsgs.ml graph.ml auto.ml unrootedTree.ml growable.ml main.ml

ocamlopt.opt ptmap.mli graph.mli

#ocamlopt.opt -inline 50 -o main -annot unix.cmxa prelude.ml ptmap.ml canon.ml rootedTree.ml matrix.ml graph.ml unrootedTree.ml growable.ml chemistry.ml main.ml

ocamlopt.opt -inline 100 -o main -annot unix.cmxa prelude.ml ptmap.ml canon.ml rootedTree.ml matrix.ml graph.ml growable.ml unrootedTree.ml chemistry.ml main.ml
