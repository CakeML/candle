(*
  This file roughly takes the place of hol.ml for replaying recorded proofs.
  It loads the early HOL Light/Candle files before bool.ml.
  bool.ml is special, as it is the first file that creates a definition, which
  is something we want to leave fully up to the replayed proofs.

  To load the state for proof replaying, use

  #use "candle/pft/pft.ml";;

  instead of

  #use "hol.ml";;
*)

loads "candle/build/insulate.ml";;
loads "candle/nums.ml";;
loads "candle/pretty.ml";;
loads "candle/ocaml.ml";;

loads "system.ml";;
loads "bignum_num.ml";;
loads "lib.ml";;

loads "candle/kernel.ml";;

loads "basics.ml";;
loads "nets.ml";;

loads "printer.ml";;
loads "preterm.ml";;
loads "parser.ml";;

loads "equal.ml";;
