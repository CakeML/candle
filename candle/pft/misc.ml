(* From bool.ml (see pft.ml on why we cannot load bool.ml) *)
let PROVE_HYP ath bth =
  if exists (aconv (concl ath)) (hyp bth)
  then EQ_MP (DEDUCT_ANTISYM_RULE ath bth) ath
  else bth;;
