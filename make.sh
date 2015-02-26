#ocamlc -c -g -dtypes float3.ml mat3.ml springmodel.ml utils.ml
#ocamlc -c graph.mli
#ocamlc -g -dtypes -I /home/piotr/.opam/4.00.1/lib/ocaml graphics.cma float3.ml mat3.ml springmodel.ml utils.ml graph.ml generation.ml display.ml smiles.ml cycles.ml main.ml -o genmol

# ocamlopt -g -o main -annot unix.cmxa dllist.ml bitv.ml prelude.ml rootedTree.ml matrix.ml group.ml perm.ml bsgs.ml graph.ml auto.ml unrootedTree.ml growable.ml main.ml

ocamlopt.opt ptmap.mli prelude.mli canonicalSet.mli growable.mli rootedTree.mli graph.mli unrootedTree.mli chemistry.mli

ocamlopt reactions.mli

menhir --infer smilesParser.mly

ocamlopt.opt smilesParser.mli

ocamllex smilesLexer.mll

ocamlopt.opt -annot -inline 100 -o main -annot\
 smilesParser.ml smilesLexer.ml\
 unix.cmxa prelude.ml ptmap.ml canonicalSet.ml rootedTree.ml matrix.ml graph.ml growable.ml unrootedTree.ml chemistry.ml output.ml reactions.ml main.ml
