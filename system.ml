(* ========================================================================= *)
(* Some miscellaneous OCaml system hacking before we get started.            *)
(*                                                                           *)
(*              (c) Copyright, John Harrison 1998-2014                       *)
(* ========================================================================= *)

(* ------------------------------------------------------------------------- *)
(* Set up a quotation expander for the `...` quotes.                         *)
(* This includes the case `;...` to support miz3, even if that isn't loaded. *)
(* Other quotations ending in `...:` are treated just as (escaped) strings,  *)
(* so they can be parsed in a type context etc.                              *)
(* ------------------------------------------------------------------------- *)

let quotexpander s =
  if s = "" then failwith "Empty quotation" else
  let c = String.sub s 0 in
  if c = ':' then
    "parse_type \"" ^
    string_escaped (String.substring s 1 (String.size s - 1)) ^ "\""
  else if c = ';' then "parse_qproof \"" ^ string_escaped s ^ "\"" else
  let n = String.size s - 1 in
  if String.substring s n 1 = ":"
  then "\"" ^ string_escaped (String.substring s 0 n) ^ "\""
  else "parse_term \"" ^ string_escaped s ^ "\"";;

let _ = Cakeml.unquote := quotexpander;;
