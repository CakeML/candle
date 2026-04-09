let here = "candle/pft/";;
let extract_footer_path = here ^ "extract_footer";;

let debug = ref true;;
let dprintln s = if !debug then Format.print_string s; Format.print_newline();;
let dprint s = if !debug then Format.print_string s;;

let trace_path = here ^ "candle-preamble.pft.bin";;

let command_stream = Text_io.openIn trace_path;;

let decode_uleb128 : Text_io.instream -> int =
  let zero     = Cake.Word8.fromInt   0 in
  let lower7   = Cake.Word8.fromInt 127 in
  let high_bit = Cake.Word8.fromInt 128 in
  let rec decode_uleb128 acc shift fd =
    match Text_io.input1 fd with
    | None -> failwith "decode_uleb128: EOF"
    | Some char ->
       let byte = Cake.Word8.fromChar char in
       let int = Cake.Word8.toInt (Cake.Word8.andb byte lower7) in
       let acc = int * shift + acc in
       let shift = shift * 128 in
       let done_ = Cake.Word8.(=) zero (Cake.Word8.andb byte high_bit) in
       if done_ then acc else decode_uleb128 acc shift fd
  in decode_uleb128 0 1;;

let process_footer trace_path =
  let cmd = String.concat " " [extract_footer_path; trace_path] in
  let r = Sys.command cmd in
  let _ = if r <> 0 then failwith ("process_footer: failed to extract footer") in
  let stream = Text_io.openIn (trace_path ^ ".footer") in
  try
    let n_ty = decode_uleb128 stream in
    let n_tm = decode_uleb128 stream in
    let n_th = decode_uleb128 stream in
    let n_ci = decode_uleb128 stream in
    Text_io.closeIn stream;
    (n_ty, n_tm, n_th, n_ci)
  with e ->
    Text_io.closeIn stream;
    (match e with
     | Failure s -> failwith ("process_footer: " ^ s)
     | _ -> raise e);;

let expect_char fd char =
  match Text_io.input1 fd with
  | None -> failwith "expect_char: EOF"
  | Some char' ->
     if char = char' then ()
     else failwith ("expect_char: unexpected " ^ String.make 1 char);;

let expect_pft fd =
  expect_char fd 'P'; expect_char fd 'F'; expect_char fd 'T';
  expect_char fd '\000';;

let expect_version fd v =
  if (decode_uleb128 fd) = v then ()
  else failwith ("expect_version: unsupported version " ^ string_of_int v);;

let read_exactly fd n =
  let bytes = Bytes.create n in
  let rec loop i =
    if i = n then () else
    match Text_io.input1 fd with
    | None -> failwith "read_exactly: EOF"
    | Some c -> Bytes.set bytes i (Cake.Word8.fromChar c); loop (i + 1)
  in
  if n < 0 then failwith ("read_exactly: negative argument")
  else loop 0; Bytes.to_string bytes;;

let decode_string fd =
  let s_len = decode_uleb128 fd in
  read_exactly fd s_len;;

let next_command fd = Text_io.input1 fd;;

(* --- candle-preamble --- *)

let (n_ty, n_tm, n_th, n_ci) = process_footer trace_path;;

(* Initial values for the arrays *)
let xvar = mk_var ("x", aty);;
let xrefl = REFL xvar;;

let tys = Array.make n_ty aty;;
let tms = Array.make n_tm xvar;;
let ths = Array.make n_th xrefl;;
let cis = Array.make n_ci (Array.make 0 xrefl);;


let pft_tyvar () =
  let id = decode_uleb128 command_stream in
  let name = decode_string command_stream in
  dprintln (String.concat " " ["TYVAR"; string_of_int id; name]);
  Array.set tys id (mk_vartype name);;

let pft_tyop () =
  let id = decode_uleb128 command_stream in
  let name = decode_string command_stream in
  let n_args = decode_uleb128 command_stream in
  let rec loop i ids acc =
    if i <= 0 then (rev ids, rev acc) else
      let id = decode_uleb128 command_stream in
      loop (i - 1) (id::ids) (Array.get tys id::acc) in
  let ids, args = loop n_args [] [] in
  dprintln (String.concat " "
            (["TYOP"; string_of_int id; name; string_of_int n_args]
             @ (map string_of_int ids)));
  Array.set tys id (mk_type (name, args));;

let pft_const () =
  let id = decode_uleb128 command_stream in
  let name = decode_string command_stream in
  let type_id = decode_uleb128 command_stream in
  let ty = Array.get tys type_id in
  dprintln (String.concat " "
              ["CONST"; string_of_int id; name; string_of_int type_id]);
  Array.set tms id (mk_mconst (name, ty));;

let pft_var () =
  let id = decode_uleb128 command_stream in
  let name = decode_string command_stream in
  let type_id = decode_uleb128 command_stream in
  let tm = Array.get tys type_id in
  dprintln (String.concat " "
              ["VAR"; string_of_int id; name; string_of_int type_id]);
  Array.set tms id (mk_var (name, ty));;


let rec command_loop () =
  match next_command command_stream with
  | None -> ()
  | Some cmd ->
     let cmd_str = string_of_int (Char.code cmd) in
     dprint (cmd_str ^ ": ");
     if cmd = Char.chr 1 then pft_tyvar ()
     else if cmd = Char.chr 2 then pft_tyop ()
     else if cmd = Char.chr 3 then pft_var ()
     else if cmd = Char.chr 4 then pft_const ()
     else failwith ("command_loop: unsupported command: " ^ cmd_str);
     command_loop ();;

let _ = expect_pft command_stream;;
let _ = expect_version command_stream 1;;

let ruleset = decode_string command_stream;;
let _ =
  if ruleset <> "candle" then failwith ("unsupported ruleset: " ^ ruleset);;

let _ = command_loop ();;

let _ = Text_io.closeIn command_stream;;
