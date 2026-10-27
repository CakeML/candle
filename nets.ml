(* ========================================================================= *)
(* Term nets: reasonably fast lookup based on term matchability.             *)
(*                                                                           *)
(*       John Harrison, University of Cambridge Computer Laboratory          *)
(*                                                                           *)
(*            (c) Copyright, University of Cambridge 1998                    *)
(*              (c) Copyright, John Harrison 1998-2007                       *)
(* ========================================================================= *)

needs "basics.ml";;

(* ------------------------------------------------------------------------- *)
(* Term nets are a finitely branching tree structure; at each level we       *)
(* have a set of branches and a set of "values". Linearization is            *)
(* performed from the left of a combination; even in iterated                *)
(* combinations we look at the head first. This is probably fastest, and     *)
(* anyway it's useful to allow our restricted second order matches: if       *)
(* the head is a variable then then whole term is treated as a variable.     *)
(* ------------------------------------------------------------------------- *)

type term_label = Vnet                          (* variable (instantiable)   *)
                 | Lcnet of (string * int)      (* local constant            *)
                 | Cnet of (string * int)       (* constant                  *)
                 | Lnet of int;;                (* lambda term (abstraction) *)

(* ------------------------------------------------------------------------- *)
(* Edges out of a net node are split by label kind. The Vnet edge is unique  *)
(* (so it's a direct field, looked up in O(1)); the others are persistent    *)
(* balanced trees keyed by (string * int) or int with monomorphic            *)
(* comparators. This gives O(log n) per-edge lookup.                         *)
(*                                                                           *)
(* Cake.Map.empty takes a comparator, so using it directly in empty_net      *)
(* would make empty_net monomorphic under CakeML's value restriction. None   *)
(* represents an empty map here; the Cake.Map is allocated on first insert.  *)
(* ------------------------------------------------------------------------- *)

type 'a net =
  Netnode of
    'a net option *
    ((string * int),'a net) map option *
    ((string * int),'a net) map option *
    (int,'a net) map option *
    'a list;;

let si_compare =
  Cake.Pair.compare Cake.String.compare Cake.Int.compare;;

(* ------------------------------------------------------------------------- *)
(* The empty net.                                                            *)
(* ------------------------------------------------------------------------- *)

let empty_net = Netnode(None,None,None,None,[]);;

let net_map_lookup k m =
  match m with
    None -> None
  | Some m -> Cake.Map.lookup m k;;

let net_map_update cmp k upd m =
  match m with
    None ->
      Some (Cake.Map.singleton cmp k (upd empty_net))
  | Some m ->
      let child0 =
        match Cake.Map.lookup m k with
          None -> empty_net
        | Some child -> child in
      Some (Cake.Map.insert m k (upd child0));;

let net_map_merge merge m1 m2 =
  match m1,m2 with
    None,None -> None
  | Some _,None -> m1
  | None,Some _ -> m2
  | Some m1,Some m2 ->
      Some (Cake.Map.unionWith (fun n1 n2 -> merge(n1,n2)) m1 m2);;

(* ------------------------------------------------------------------------- *)
(* Insert a new element into a net.                                          *)
(* ------------------------------------------------------------------------- *)

let enter lconsts (tm,elem) net =
  let label_to_store (lconsts:term list) (tm:term) : term_label * term list =
    let op,args = strip_comb tm in
    if is_const op then Cnet(fst(dest_const op),length args),args
    else if is_abs op then
      let bv,bod = dest_abs op in
      let bod' = if mem bv lconsts then vsubst [genvar(type_of bv),bv] bod
                 else bod in
      Lnet(length args),bod'::args
    else if mem op lconsts then Lcnet(fst(dest_var op),length args),args
    else Vnet,[] in
  let rec net_update (lconsts:term list) elem
                     (tms:term list) net =
    let Netnode(vnet,cnets,lcnets,lnets,tips) = net in
    match tms with
      [] -> Netnode(vnet,cnets,lcnets,lnets,tips @ [elem])
    | (tm::rtms) ->
        let label,ntms = label_to_store lconsts tm in
        let upd child = net_update lconsts elem (ntms@rtms) child in
        match (label:term_label) with
          Vnet ->
            let child0 =
              match vnet with Some c -> c | None -> empty_net in
            Netnode(Some(upd child0),cnets,lcnets,lnets,tips)
        | Cnet k ->
            Netnode(vnet,net_map_update si_compare k upd cnets,
                    lcnets,lnets,tips)
        | Lcnet k ->
            Netnode(vnet,cnets,
                    net_map_update si_compare k upd lcnets,
                    lnets,tips)
        | Lnet k ->
            Netnode(vnet,cnets,lcnets,
                    net_map_update Cake.Int.compare k upd lnets,tips) in
  net_update lconsts elem [tm] net;;

(* ------------------------------------------------------------------------- *)
(* Look up a term in a net and return possible matches.                      *)
(* ------------------------------------------------------------------------- *)

let lookup tm net =
  let label_for_lookup (tm:term) : term_label * term list =
    let op,args = strip_comb tm in
    if is_const op then Cnet(fst(dest_const op),length args),args
    else if is_abs op then Lnet(length args),(body op)::args
    else Lcnet(fst(dest_var op),length args),args in
  let rec follow (tms:term list) net =
    let Netnode(vnet,cnets,lcnets,lnets,tips) = net in
    match tms with
      [] -> tips
    | (tm::rtms) ->
        let label,ntms = label_for_lookup tm in
        let collection =
          let child =
            match (label:term_label) with
              Cnet k -> net_map_lookup k cnets
            | Lcnet k -> net_map_lookup k lcnets
            | Lnet k -> net_map_lookup k lnets
            | Vnet -> None in
          match child with
            None -> []
          | Some vchild -> follow (ntms@rtms) vchild in
        match vnet with
          None -> collection
        | Some vchild -> collection @ follow rtms vchild in
  follow [tm] net;;

(* ------------------------------------------------------------------------- *)
(* Function to merge two nets (code from Don Syme's hol-lite).               *)
(* ------------------------------------------------------------------------- *)

let rec merge_nets (n1,n2) =
  let Netnode(vnet1,cnets1,lcnets1,lnets1,tips1) = n1
  and Netnode(vnet2,cnets2,lcnets2,lnets2,tips2) = n2 in
  let merge_opt a b =
    match a,b with
      Some x, Some y -> Some (merge_nets (x,y))
    | Some _, None -> a
    | None, _ -> b in
  Netnode
    (merge_opt vnet1 vnet2,
     net_map_merge merge_nets cnets1 cnets2,
     net_map_merge merge_nets lcnets1 lcnets2,
     net_map_merge merge_nets lnets1 lnets2,
     tips1 @ tips2);;
