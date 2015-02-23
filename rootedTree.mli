module NodeEdgeColoured :
  functor (NLab : Prelude.Ordered) (LLab : Prelude.Ordered) ->
    sig

      type t = ECNode of NLab.t * (LLab.t * t) list
      type canonical

      val canonical : t -> canonical
      val compare   : canonical -> canonical -> int
      val print     : t -> string
    end
