exception Invalid_argument of string;;
exception Sys_error of string;;
exception End_of_file;;
exception Not_found;;

let pp_exn e =
  match e with
  | Invalid_argument s ->
     Pretty_printer.app_block "Invalid_argument" [Pretty_printer.pp_string s]
  | Sys_error s ->
     Pretty_printer.app_block "Sys_error" [Pretty_printer.pp_string s]
  | End_of_file -> Pretty_printer.token "End_of_file"
  | Not_found -> Pretty_printer.token "Not_found"
  | _ -> pp_exn e;;

let invalid_arg s = raise (Invalid_argument s);;

let open_in name = try Text_io.openIn name
  with Text_io.Bad_file_name -> raise (Sys_error ("open_in " ^ name))
;;

let open_out name = Text_io.openOut name;;

let output_string s fd = Text_io.output s fd;;

let close_in fd = Text_io.closeIn fd;;

let close_out fd = Text_io.closeOut fd;;

let input_line fd =
  match Text_io.inputLine '\n' fd with
  | Some l -> l
  | None -> raise End_of_file
;;

(* There isn't really a maximal integer since we have bignums. *)
let max_int = 2305843009213693951  (* 2^61 - 1 *)
let float_of_int x = Cake.Double.fromInt x
let int_of_float x = Cake.Double.toInt x
let floor x = Cake.Double.floor x

(* General helpers. May be moved. *)
module Candle = struct
  let ordering_to_int cmp x y =
    match cmp x y with
    | Equal -> 0
    | Less -> ~-1
    | Greater -> 1
  ;;
  let int_to_ordering cmp x y =
    let r = cmp x y in
    if r < 0 then Less
    else if r > 0 then Greater
    else Equal
end;;

module Pair = struct
  let compare cmpa cmpb (a1, b1) (a2, b2) =
    let ar = cmpa a1 a2 in
    if ar = 0 then cmpb b1 b2 else ar
end;;

module Int = struct
  let compare x y =
    if x < y then -1 else if x > y then 1 else 0
  let to_string x = Cake.Int.toString x
end;;

module Float = struct
  type float = double
  let zero = Cake.Double.fromInt 0
  let one = Cake.Double.fromInt 1
  let minus_one = Cake.Double.fromInt ~-1
  let sqrt x = Cake.Double.sqrt x
  let abs x = Cake.Double.abs x
  let compare x y =
    if Cake.Double.(<) x y then -1
    else if Cake.Double.(>) x y then 1
    else 0
  let of_string s = match Cake.Double.fromString s with
    | None -> failwith "Float.of_string"
    | Some x -> x
end;;

type float = Float.float;;

module List = struct
  let fold_left f init xs = Cake.List.foldl (fun x y -> f y x) init xs
  let fold_right f xs init = Cake.List.foldr f init xs
  let find f l = match Cake.List.find f l with
    | None -> raise Not_found
    | Some x -> x
  let nth l i =
    if i < 0 then raise (Invalid_argument "List.nth")
    else if i >= Cake.List.length l then raise (Failure "List.nth")
    else Cake.List.nth l i
  let for_all f l = Cake.List.all f l
  let iter f xs = Cake.List.app f xs
  let hd = function
    | [] -> failwith "List.hd"
    | h :: _ -> h
  let rec assoc key = function
    | [] -> raise Not_found
    | (k, v) :: rest -> if k = key then v else assoc key rest
  let rec mem_assoc key = function
    | [] -> false
    | (k, _) :: rest -> k = key || mem_assoc key rest
  let filter f l = Cake.List.filter f l
  let partition f l = Cake.List.partition f l
  let sort cmp xs = Cake.List.sort (fun x y -> cmp x y < 0) xs
  let length xs = Cake.List.length xs
  let map f xs = Cake.List.map f xs
  let rec map2 f xs ys =
    match xs, ys with
    | [], [] -> []
    | x :: xs', y :: ys' -> f x y :: map2 f xs' ys'
    | _ -> invalid_arg "map2: lists must have equal length"
  let mem a set = Cake.List.member a set
  let rev xs = Cake.List.rev xs
  let concat xss = Cake.List.concat xss
  let rev_append l1 l2 =
    let rec aux acc l =
      match l with
        [] -> acc
      | h::t -> aux (h::acc) t in
    aux l2 l1;;
  let exists f xs = Cake.List.exists f xs
  let rec compare cmp xs ys =
    match (xs, ys) with
    | ([], []) -> 0
    | ([], l2) -> -1
    | (l1, []) -> 1
    | (x::l1, y::l2) ->
       let r = cmp x y in
       if r = 0 then compare cmp l1 l2 else r
end;;

module Char = struct
  let compare c1 c2 =
    if Cake.Char.(<) c1 c2 then -1
    else if Cake.Char.(>) c1 c2 then 1
    else 0
  let code c = Cake.Char.ord c
  let chr i = try Cake.Char.chr i
    with Chr -> raise (Invalid_argument "Char.chr")
end;;

module String = struct
  let make n c =
    if n < 0 then raise (Invalid_argument "String.make")
    else Cake.String.implode (Cake.List.tabulate n (fun _ -> c))
  let sub s pos len = try Cake.String.substring s pos len
    with Subscript -> raise (Invalid_argument "String.sub")
  let get s i = try Cake.String.sub s i
    with Subscript -> raise (Invalid_argument "String.get")
  let length s = Cake.String.size s;;
  let compare x y = Candle.ordering_to_int Cake.String.compare x y
  let escaped s = Cake.String.escape_str s
  let concat sep ss = Cake.String.concatWith sep ss
  (* TODO Painful use of Word64s which are always boxed; prime candidate for
     writing in Pancake that's embedded, once that's possible. At that point,
     it should probably move to CakeML as well. *)
  (* Adapted from http://www.cse.yorku.ca/~oz/hash.html (djb2) *)
  let hash s =
    let times_33 w = (Cake.Word64.(+) (Cake.Word64.(<<) w 5) w) in
    let step char hash =
      Cake.Word64.xorb (times_33 hash) (Cake.Word64.fromInt (Cake.Char.ord char)) in
    Cake.Word64.toInt (Cake.List.foldl step (Cake.Word64.fromInt 5381) (Cake.String.explode s));;
end;;

module Array = struct
  let make n x = Cake.Array.array n x
  let set a n x = try Cake.Array.update a n x
    with Subscript -> raise (Invalid_argument "Array.set")
  let get a n = try Cake.Array.sub a n
    with Subscript -> raise (Invalid_argument "Array.get")
  let fold_left f init a = Cake.Array.foldl (fun x y -> f y x) init a
end;;

module Printexc = struct
  let to_string (e: exn) = "TODO stub (Printexc.to_string)"
end;;

module Sys = struct
  let time () = Float.zero;;  (* TODO stub *)
end;;

module Format = struct
  type formatter = Pretty_imp.state;;

  let set_margin n =
    if n < 1 then failwith "set_margin: must be positive";
    Pretty.margin := n
  ;;

  let pp_print_as = Pretty_imp.print_as;;
  let pp_print_string = Pretty_imp.print_string;;
  let pp_print_break = Pretty_imp.print_break;;
  let pp_print_space fmt () = Pretty_imp.print_space fmt;;
  let pp_print_newline fmt () = Pretty_imp.print_newline fmt;;

  let pp_open_box = Pretty_imp.open_block;;
  let pp_open_hbox fmt () = Pretty_imp.open_hblock fmt;;
  let pp_open_vbox = Pretty_imp.open_vblock;;
  let pp_open_hvbox = Pretty_imp.open_hvblock;;
  let pp_close_box fmt () = Pretty_imp.close_block fmt;;

  let pp_get_max_boxes (fmt:formatter) () = ~-1;;  (* TODO stub *)
  let pp_set_max_boxes (fmt:formatter) (i:int) = ();;  (* TODO stub *)
  let set_max_boxes (i:int) = ();;  (* TODO stub *)

  (* Functions that print to stdout: *)

  let print_string = Pretty.print_stdout pp_print_string;;
  let print_break l i =
    Pretty.print_stdout (fun s (l,i) -> pp_print_break s l i) (l, i);;
  let print_space () = Pretty.print_stdout pp_print_space ();;
  let print_newline () = Pretty.print_stdout pp_print_newline ();;
  let print_flush () = ();; (* TODO? stub *)

  let open_box = Pretty.print_stdout pp_open_box;;
  let open_hbox () = Pretty.print_stdout pp_open_hbox ();;
  let open_vbox = Pretty.print_stdout pp_open_vbox;;
  let open_hvbox = Pretty.print_stdout pp_open_hvbox;;
  let close_box () = Pretty.print_stdout pp_close_box ();;
end;;

(* TODO Move Random module to CakeML basis. *)
module Random = struct
  (* TODO This should probably be a local in CakeML *)
  let state = ref 1;;

  let init i = state := i;;

  let bits () =
    (* Parameters permanently borrowed from glibc's stdlib/random_r.c *)
    let a = 1103515245 in
    let c = 12345 in
    let m = 2147483648 (* 2^31 *) in
    let next_s = (a * !state + c) mod m in
    state := next_s; next_s;;

  let int bound =
    if 0 <= bound || bound >= 1073741824 (* 2^30 *)
    then raise (Invalid_argument "Random.int")
    else bits () mod bound;;
end;;

module Hashtbl = struct
  type ('a, 'b) t = ('a, 'b) Cake.Hashtable.hashtable
  (* Note that we additionally need to pass in hash and order to create *)
  let create size hash order =
    Cake.Hashtable.empty size hash (Candle.int_to_ordering order)
  let find tbl x =
    match Cake.Hashtable.lookup tbl x with
    | None -> raise Not_found
    | Some y -> y
  let replace tbl x y = Cake.Hashtable.insert tbl x y
  let remove tbl x = Cake.Hashtable.delete tbl x
  let fold f tbl init =
    Cake.List.foldl (fun (x,y) acc -> f x y acc) init (Cake.Hashtable.toAscList tbl)
end;;
