(* ========================================================================= *)
(* FIRST ORDER LOGIC LITERALS                                                *)
(* ========================================================================= *)

module Literal = struct

open Useful;;
open Order

(* ------------------------------------------------------------------------- *)
(* A type for storing first order logic literals.                            *)
(* ------------------------------------------------------------------------- *)

type polarity = bool;;

type literal = polarity * Atom.atom;;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

let polarity ((pol,_) : literal) = pol;;

let atom ((_,atm) : literal) = atm;;

let name lit = Atom.name (atom lit);;

let arguments lit = Atom.arguments (atom lit);;

let arity lit = Atom.arity (atom lit);;

let positive lit = polarity lit;;

let negative lit = not (polarity lit);;

let negate (pol,atm) : literal = (not pol, atm)

let relation lit = Atom.relation (atom lit);;

let functions lit = Atom.functions (atom lit);;

let functionNames lit = Atom.functionNames (atom lit);;

(* Binary relations *)

let mkBinop rel (pol,a,b) : literal = (pol, Atom.mkBinop rel (a,b));;

let destBinop rel ((pol,atm) : literal) =
    match Atom.destBinop rel atm with (a,b) -> (pol,a,b);;

let isBinop rel = can (destBinop rel);;

(* Formulas *)

let toFormula = function
    (true,atm) -> Formula.Atom atm
  | (false,atm) -> Formula.Not (Formula.Atom atm);;

let fromFormula = function
    (Formula.Atom atm) -> (true,atm)
  | (Formula.Not (Formula.Atom atm)) -> (false,atm)
  | _ -> raise (Error "Literal.fromFormula");;

(* ------------------------------------------------------------------------- *)
(* The size of a literal in symbols.                                         *)
(* ------------------------------------------------------------------------- *)

let symbols ((_,atm) : literal) = Atom.symbols atm;;

(* ------------------------------------------------------------------------- *)
(* A total comparison function for literals.                                 *)
(* ------------------------------------------------------------------------- *)

let compare = prodCompare boolCompare Atom.compare;;

let equal (p1,atm1) (p2,atm2) = p1 = p2 && Atom.equal atm1 atm2;;

(* ------------------------------------------------------------------------- *)
(* Subterms.                                                                 *)
(* ------------------------------------------------------------------------- *)

let subterm lit path = Atom.subterm (atom lit) path;;

let subterms lit = Atom.subterms (atom lit);;

let replace ((pol,atm) as lit) path_tm =
      let atm' = Atom.replace atm path_tm
    in
      if Portable.pointerEqual (atm,atm') then lit else (pol,atm')
    ;;

(* ------------------------------------------------------------------------- *)
(* Free variables.                                                           *)
(* ------------------------------------------------------------------------- *)

let freeIn v lit = Atom.freeIn v (atom lit);;

let freeVars lit = Atom.freeVars (atom lit);;

(* ------------------------------------------------------------------------- *)
(* Substitutions.                                                            *)
(* ------------------------------------------------------------------------- *)

let subst sub ((pol,atm) as lit) : literal =
      let atm' = Atom.subst sub atm
    in
      if Portable.pointerEqual (atm',atm) then lit else (pol,atm')
    ;;

(* ------------------------------------------------------------------------- *)
(* Matching.                                                                 *)
(* ------------------------------------------------------------------------- *)

let matchLiterals sub ((pol1,atm1) : literal) (pol2,atm2) =
      let _ = pol1 = pol2 || raise (Error "Literal.match")
    in
      Atom.matchAtoms sub atm1 atm2
    ;;

(* ------------------------------------------------------------------------- *)
(* Unification.                                                              *)
(* ------------------------------------------------------------------------- *)

let unify sub ((pol1,atm1) : literal) (pol2,atm2) =
      let _ = pol1 = pol2 || raise (Error "Literal.unify")
    in
      Atom.unify sub atm1 atm2
    ;;

(* ------------------------------------------------------------------------- *)
(* The equality relation.                                                    *)
(* ------------------------------------------------------------------------- *)

let mkEq l_r : literal = (true, Atom.mkEq l_r);;

let destEq = function
    ((true,atm) : literal) -> Atom.destEq atm
  | (false,_) -> raise (Error "Literal.destEq");;

let isEq = can destEq;;

let mkNeq l_r : literal = (false, Atom.mkEq l_r);;

let destNeq = function
    ((false,atm) : literal) -> Atom.destEq atm
  | (true,_) -> raise (Error "Literal.destNeq");;

let isNeq = can destNeq;;

let mkRefl tm = (true, Atom.mkRefl tm);;

let destRefl = function
    (true,atm) -> Atom.destRefl atm
  | (false,_) -> raise (Error "Literal.destRefl");;

let isRefl = can destRefl;;

let mkIrrefl tm = (false, Atom.mkRefl tm);;

let destIrrefl = function
    (true,_) -> raise (Error "Literal.destIrrefl")
  | (false,atm) -> Atom.destRefl atm;;

let isIrrefl = can destIrrefl;;

let sym (pol,atm) : literal = (pol, Atom.sym atm);;

let lhs ((_,atm) : literal) = Atom.lhs atm;;

let rhs ((_,atm) : literal) = Atom.rhs atm;;

(* ------------------------------------------------------------------------- *)
(* Special support for terms with type annotations.                          *)
(* ------------------------------------------------------------------------- *)

let typedSymbols ((_,atm) : literal) = Atom.typedSymbols atm;;

let nonVarTypedSubterms ((_,atm) : literal) = Atom.nonVarTypedSubterms atm;;

(* ------------------------------------------------------------------------- *)
(* Parsing and pretty-printing.                                              *)
(* ------------------------------------------------------------------------- *)

let toString literal = Formula.toString (toFormula literal);;


module Ordered =
struct type t = literal let compare = fromCompare compare end

module Map = Mmap.Make (Ordered);;

module Set =
struct
  include Mset.Make (Ordered);;

  let negateMember lit set = member (negate lit) set;;

  let negate =
        let f (lit,set) = add set (negate lit)
      in
        foldl f empty
      ;;

  let relations =
        let f (lit,set) = Name_arity.Set.add set (relation lit)
      in
        foldl f Name_arity.Set.empty
      ;;

  let functions =
        let f (lit,set) = Name_arity.Set.union set (functions lit)
      in
        foldl f Name_arity.Set.empty
      ;;

  let freeIn v = exists (freeIn v);;

  let freeVars =
        let f (lit,set) = Name.Set.union set (freeVars lit)
      in
        foldl f Name.Set.empty
      ;;

  let freeVarsList =
        let f (lits,set) = Name.Set.union set (freeVars lits)
      in
        Mlist.foldl f Name.Set.empty
      ;;

  let symbols =
        let f (lit,z) = symbols lit + z
      in
        foldl f 0
      ;;

  let typedSymbols =
        let f (lit,z) = typedSymbols lit + z
      in
        foldl f 0
      ;;

  let subst sub lits =
        let substLit (lit,(eq,lits')) =
              let lit' = subst sub lit
              in let eq = eq && Portable.pointerEqual (lit,lit')
            in
              (eq, add lits' lit')

        in let (eq,lits') = foldl substLit (true,empty) lits
      in
        if eq then lits else lits'
      ;;

  let conjoin set =
      Formula.listMkConj (List.map toFormula (toList set));;

  let disjoin set =
      Formula.listMkDisj (List.map toFormula (toList set));;

  let toString cl =
    "{" ^ String.concat ", " (List.map toString (toList cl)) ^ "}"

end

module Set_ordered =
struct type t = Set.set let compare = fromCompare Set.compare end

module Set_map = Mmap.Make (Set_ordered);;

module Set_set = Mset.Make (Set_ordered);;

end
