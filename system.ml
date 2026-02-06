(* ========================================================================= *)
(* Some miscellaneous OCaml system hacking before we get started.            *)
(*                                                                           *)
(*              (c) Copyright, John Harrison 1998-2014                       *)
(* ========================================================================= *)

(* ------------------------------------------------------------------------- *)
(* Set up a printer for num.                                                 *)
(* ------------------------------------------------------------------------- *)

let quotexpander s =
  if s = "" then failwith "Empty quotation" else
  let c = Cake.String.sub s 0 in
  if c = ':' then
    "parse_type \"" ^
    string_escaped (Cake.String.substring s 1 (Cake.String.size s - 1)) ^ "\""
  else if c = ';' then "parse_qproof \"" ^ string_escaped s ^ "\"" else
  let n = Cake.String.size s - 1 in
  if Cake.String.substring s n 1 = ":"
  then "\"" ^ string_escaped (Cake.String.substring s 0 n) ^ "\""
  else "parse_term \"" ^ string_escaped s ^ "\"";;

let _ = Cakeml.unquote := quotexpander;;
