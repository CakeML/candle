exception Invalid_argument of string;;
exception Sys_error of string;;
exception End_of_file;;

let pp_exn e =
  match e with
  | Invalid_argument s ->
     Pretty_printer.app_block "Invalid_argument" [Pretty_printer.pp_string s]
  | Sys_error s ->
     Pretty_printer.app_block "Sys_error" [Pretty_printer.pp_string s]
  | End_of_file -> Pretty_printer.token "End_of_file"
  | _ -> pp_exn e;;

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

(* General helpers. May be moved. *)
module Candle = struct
  let ordering_to_int cmp x y =
    match cmp x y with
    | Equal -> 0
    | Less -> ~-1
    | Greater -> 1
  ;;
end;;

module Float = struct
  let zero = Cake.Double.fromInt 0
  let one = Cake.Double.fromInt 1
  let minus_one = Cake.Double.fromInt ~-1
  let sqrt x = Cake.Double.sqrt x
  let abs x = Cake.Double.abs x
end;;

module List = struct
  let exists f xs = Cake.List.exists f xs
end;;

module Char = struct
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
end;;

module Array = struct
  let make n x = Cake.Array.array n x
  let set a n x = try Cake.Array.update a n x
    with Subscript -> raise (Invalid_argument "Array.set")
  let get a n = try Cake.Array.sub a n
    with Subscript -> raise (Invalid_argument "Array.get")
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

  let open_box = Pretty.print_stdout pp_open_box;;
  let open_hbox () = Pretty.print_stdout pp_open_hbox ();;
  let open_vbox = Pretty.print_stdout pp_open_vbox;;
  let open_hvbox = Pretty.print_stdout pp_open_hvbox;;
  let close_box () = Pretty.print_stdout pp_close_box ();;
end;;
