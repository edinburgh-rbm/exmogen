exmogen
=======

Exhaustive molecule generator

TODOs/Ideas
===========
* Cyclic molecules. Need a full-fledged graph canonicalizer.
  See auto-dix.
* Bottom-up generation:
  given a bunch of constructors (atoms with arities)
  . start from depth 0 trees i.e. atoms with 1 free electron
    by definition the automorphism group of a depth 0 tree is trivial
  . generate all depth 1 trees by exhaustive combinations
    at this stage we should be able to use local automorphism group
    from the non-trivial atoms (e.g. carbon) to compute the automorphism
    group of the whole molecule
  . iterate: the inductive step is to take all depth D-1 trees,
    each given with its local automorphism group
    then generate all possible depth D trees, computing the new
    automorphism group using some kind of product of groups
    (for this, see Serre's work)
* In chemistry.ml: check for bug in compatibility function
* Try Patricia trees for integer maps
* Incremental update of canonical root?