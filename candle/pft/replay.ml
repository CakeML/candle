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
  let ty = Array.get tys type_id in
  dprintln (String.concat " "
              ["VAR"; string_of_int id; name; string_of_int type_id]);
  Array.set tms id (mk_var (name, ty));;

let pft_abs () =
  let id = decode_uleb128 command_stream in
  let var_id = decode_uleb128 command_stream in
  let body_id = decode_uleb128 command_stream in
  let var_tm = Array.get tms var_id in
  let body_tm = Array.get tms body_id in
  dprintln (String.concat " "
              ["ABS"; string_of_int id; string_of_int var_id;
               string_of_int body_id]);
  Array.set tms id (mk_abs (var_tm, body_tm));;

let pft_comb () =
  let id = decode_uleb128 command_stream in
  let rator_id = decode_uleb128 command_stream in
  let rand_id = decode_uleb128 command_stream in
  let rator_tm = Array.get tms rator_id in
  let rand_tm = Array.get tms rand_id in
  dprintln (String.concat " "
              ["COMB"; string_of_int id; string_of_int rator_id;
               string_of_int rand_id]);
  Array.set tms id (mk_comb (rator_tm, rand_tm));;

let pft_assume () =
  let id = decode_uleb128 command_stream in
  let tm_id = decode_uleb128 command_stream in
  dprintln (String.concat " " ["ASSUME"; string_of_int id; string_of_int tm_id]);
  let tm = Array.get tms tm_id in
  Array.set ths id (ASSUME tm)

let pft_new_specification () =
  let id = decode_uleb128 command_stream in
  let th_id = decode_uleb128 command_stream in
  let n_names = decode_uleb128 command_stream in
  let rec loop i names =
    if i <= 0 then rev names else
      let name = decode_string command_stream in
      loop (i - 1) (name::names) in
  let names = loop n_names [] in
  dprintln (String.concat " "
              (["new_specification"; string_of_int id; string_of_int th_id;
                string_of_int n_names] @ names));
  let th = Array.get ths th_id in
  Array.set ths id (new_specification names th);;

let rec command_loop () =
  match next_command command_stream with
  | None -> ()
  | Some cmd ->
     let cmd_str = string_of_int (Char.code cmd) in
     dprint (cmd_str ^ ": ");
     if cmd = Char.chr 0x01 then pft_tyvar ()
     else if cmd = Char.chr 0x02 then pft_tyop ()
     else if cmd = Char.chr 0x03 then pft_var ()
     else if cmd = Char.chr 0x04 then pft_const ()
     else if cmd = Char.chr 0x05 then pft_comb ()
     else if cmd = Char.chr 0x06 then pft_abs ()
     else if cmd = Char.chr 0x15 then pft_assume ()
     else if cmd = Char.chr 0x30 then pft_new_specification ()
     else failwith ("command_loop: unsupported command: " ^ cmd_str);
     command_loop ();;

let _ = expect_pft command_stream;;
let _ = expect_version command_stream 1;;

let ruleset = decode_string command_stream;;
let _ =
  if ruleset <> "candle" then failwith ("unsupported ruleset: " ^ ruleset);;

let _ = command_loop ();;

let _ = Text_io.closeIn command_stream;;
