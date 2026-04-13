let here = "candle/pft/";;
let extract_footer_path = here ^ "extract_footer";;

let debug = true;;
let dprintln s = if debug then Format.print_string s; Format.print_newline();;
let dprint s = if debug then Format.print_string s;;

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

let saved_ths = ref ([]: (string * thm) list);;
let save_th name th = saved_ths := (name, th)::(!saved_ths);;
let load_th name = assoc name (!saved_ths);;

let pft_tyvar () =
  let id = decode_uleb128 command_stream in
  let name = decode_string command_stream in
  dprintln (String.concat " " ["TYVAR"; string_of_int id; name]);
  Array.set tys id (Kernel.mk_vartype name);;

let pft_tyop () =
  let id = decode_uleb128 command_stream in
  let name = decode_string command_stream in
  let n_args = decode_uleb128 command_stream in
  dprint (String.concat " "
              ["TYOP"; string_of_int id; name; string_of_int n_args]);
  let rec loop i args =
    if i <= 0 then rev args else
      let id = decode_uleb128 command_stream in
      dprint (string_of_int id ^ (if i = 1 then "" else " "));
      loop (i - 1) (Array.get tys id::args) in
  let args = loop n_args [] in
  dprint "\n";
  Array.set tys id (Kernel.mk_type (name, args));;

let pft_const () =
  let id = decode_uleb128 command_stream in
  let name = decode_string command_stream in
  let type_id = decode_uleb128 command_stream in
  dprintln (String.concat " "
              ["CONST"; string_of_int id; name; string_of_int type_id]);
  let ty = Array.get tys type_id in
  Array.set tms id (mk_mconst (name, ty));;

let pft_var () =
  let id = decode_uleb128 command_stream in
  let name = decode_string command_stream in
  let type_id = decode_uleb128 command_stream in
  dprintln (String.concat " "
              ["VAR"; string_of_int id; name; string_of_int type_id]);
  let ty = Array.get tys type_id in
  Array.set tms id (Kernel.mk_var (name, ty));;

let pft_abs () =
  let id = decode_uleb128 command_stream in
  let var_id = decode_uleb128 command_stream in
  let body_id = decode_uleb128 command_stream in
  dprintln (String.concat " "
              ["ABS"; string_of_int id; string_of_int var_id;
               string_of_int body_id]);
  let var_tm = Array.get tms var_id in
  let body_tm = Array.get tms body_id in
  Array.set tms id (Kernel.mk_abs (var_tm, body_tm));;

let pft_comb () =
  let id = decode_uleb128 command_stream in
  let rator_id = decode_uleb128 command_stream in
  let rand_id = decode_uleb128 command_stream in
  dprintln (String.concat " "
              ["COMB"; string_of_int id; string_of_int rator_id;
               string_of_int rand_id]);
  let rator_tm = Array.get tms rator_id in
  let rand_tm = Array.get tms rand_id in
  Array.set tms id (mk_comb (rator_tm, rand_tm));;

let pft_assume () =
  let id = decode_uleb128 command_stream in
  let tm_id = decode_uleb128 command_stream in
  dprintln (String.concat " " ["ASSUME"; string_of_int id; string_of_int tm_id]);
  let tm = Array.get tms tm_id in
  Array.set ths id (Kernel.ASSUME tm)

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
  Array.set ths id (Kernel.new_specification th);;

let pft_new_type_definition () =
  let id = decode_uleb128 command_stream in
  let th_id = decode_uleb128 command_stream in
  let tyname = decode_string command_stream in
  let absname = decode_string command_stream in
  let repname = decode_string command_stream in
  dprintln (String.concat " " ([
                  "new_type_definition"; string_of_int id; tyname; absname;
                  repname]));
  let th = Array.get ths th_id in
  failwith ("todo: pft_new_type_definition" ^
    "(don't know how to handle the two theorems returned by new_basic_type_definition)")
    (* ;Kernel.new_basic_type_definition (tyname, (absname, (repname, th))) *)
;;

let pft_compute_init () = failwith "todo: pft_compute_init"
let pft_compute () = failwith "todo: pft_compute"

let pft_save () =
  let name = decode_string command_stream in
  let th_id = decode_uleb128 command_stream in
  dprintln (String.concat " " ["SAVE"; name; string_of_int th_id]);
  let th = Array.get ths th_id in
  save_th name th;;

let pft_load () =
  let th_id = decode_uleb128 command_stream in
  let name = decode_string command_stream in
  dprintln (String.concat " " ["LOAD"; string_of_int th_id; name]);
  let th = load_th name in
  Array.set ths th_id th;;

let pft_sym () =
  let id = decode_uleb128 command_stream in
  let th_id = decode_uleb128 command_stream in
  dprintln (String.concat " " ["SYM"; string_of_int id; string_of_int th_id]);
  let th = Array.get ths th_id in
  Array.set ths id (SYM th);;

let pft_prove_hyp () =
  let id = decode_uleb128 command_stream in
  let th1_id = decode_uleb128 command_stream in
  let th2_id = decode_uleb128 command_stream in
  dprintln (String.concat " " [
                "PROVE_HYP"; string_of_int id; string_of_int th1_id;
                string_of_int th2_id]);
  let th1 = Array.get ths th1_id in
  let th2 = Array.get ths th2_id in
  Array.set ths id (PROVE_HYP th1 th2);;

let pft_alpha_thm () = failwith "todo: pft_alpha_thm";;

let pft_refl () =
  let id = decode_uleb128 command_stream in
  let tm_id = decode_uleb128 command_stream in
  dprintln (String.concat " " ["REFL"; string_of_int id; string_of_int tm_id]);
  let tm = Array.get tms tm_id in
  Array.set ths id (REFL tm);;

let pft_trans () =
  let id = decode_uleb128 command_stream in
  let th1_id = decode_uleb128 command_stream in
  let th2_id = decode_uleb128 command_stream in
  dprintln (String.concat " " [
                "TRANS"; string_of_int id; string_of_int th1_id;
                string_of_int th2_id]);
  let th1 = Array.get ths th1_id in
  let th2 = Array.get ths th2_id in
  Array.set ths id (Kernel.TRANS th1 th2);;

let pft_mk_comb_thm () =
  let id = decode_uleb128 command_stream in
  let th1_id = decode_uleb128 command_stream in
  let th2_id = decode_uleb128 command_stream in
  dprintln (String.concat " " [
                "MK_COMB"; string_of_int id; string_of_int th1_id;
                string_of_int th2_id]);
  let th1 = Array.get ths th1_id in
  let th2 = Array.get ths th2_id in
  Array.set ths id (MK_COMB (th1, th2));;

let pft_abs_thm () =
  let id = decode_uleb128 command_stream in
  let tm_id = decode_uleb128 command_stream in
  let th_id = decode_uleb128 command_stream in
  dprintln (String.concat " " [
                "ABS"; string_of_int id; string_of_int tm_id;
                string_of_int th_id]);
  let tm = Array.get tms tm_id in
  let th = Array.get ths th_id in
  Array.set ths id (ABS tm th);;

let pft_new_const () =
  let name = decode_string command_stream in
  let ty_id = decode_uleb128 command_stream in
  dprintln (String.concat " " ["NEW_CONST"; name; string_of_int ty_id]);
  let ty = Array.get tys ty_id in
  Kernel.new_constant (name, ty);;

let pft_new_type () =
  let name = decode_string command_stream in
  let arity = decode_uleb128 command_stream in
  dprintln (String.concat " " ["NEW_TYPE"; name; string_of_int arity]);
  Kernel.new_type (name, arity);;

let pft_axiom () =
  let id = decode_uleb128 command_stream in
  let tm_id = decode_uleb128 command_stream in
  let name = decode_string command_stream in
  dprintln (String.concat " " [
                "AXIOM"; string_of_int id; string_of_int tm_id; name]);
  let tm = Array.get tms tm_id in
  Array.set ths id (Kernel.new_axiom tm);;

let pft_beta () =
  let id = decode_uleb128 command_stream in
  let tm_id = decode_uleb128 command_stream in
  dprintln (String.concat " " ["BETA"; string_of_int id; string_of_int tm_id]);
  let tm = Array.get tms tm_id in
  Array.set ths id (Kernel.BETA tm);;

let pft_eq_mp () =
  let id = decode_uleb128 command_stream in
  let eq_id = decode_uleb128 command_stream in
  let th_id = decode_uleb128 command_stream in
  dprintln (String.concat " " [
                "EQ_MP"; string_of_int id; string_of_int eq_id;
                string_of_int th_id]);
  let eq = Array.get ths eq_id in
  let th = Array.get ths th_id in
  Array.set ths id (EQ_MP eq th);;

let pft_deduct_antisym_rule () =
  let id = decode_uleb128 command_stream in
  let th1_id = decode_uleb128 command_stream in
  let th2_id = decode_uleb128 command_stream in
  dprintln (String.concat " " [
                "DEDUCT_ANTISYM_RULE"; string_of_int id; string_of_int th1_id;
                string_of_int th2_id]);
  let th1 = Array.get ths th1_id in
  let th2 = Array.get ths th2_id in
  Array.set ths id (DEDUCT_ANTISYM_RULE th1 th2);;

let pft_inst () =
  let id = decode_uleb128 command_stream in
  let th_id = decode_uleb128 command_stream in
  let n_pairs = decode_uleb128 command_stream in
  dprint (String.concat " " [
              "INST"; string_of_int id; string_of_int th_id;
              string_of_int n_pairs]);
  let rec loop i pairs =
    if i <= 0 then rev pairs else
      let id1 = decode_uleb128 command_stream in
      dprint (string_of_int id1 ^ " ");
      let id2 = decode_uleb128 command_stream in
      dprint (string_of_int id2 ^ (if i = 1 then "" else " "));
      let tm1 = Array.get tms id1 in
      let tm2 = Array.get tms id2 in
      loop (i - 1) ((tm2, tm1)::pairs) in
  let pairs = loop n_pairs [] in
  dprint "\n";
  let th = Array.get ths th_id in
  Array.set ths id (Kernel.INST pairs th);;

let pft_inst_type () =
  let id = decode_uleb128 command_stream in
  let th_id = decode_uleb128 command_stream in
  let n_pairs = decode_uleb128 command_stream in
  dprint (String.concat " " [
              "INST_TYPE"; string_of_int id; string_of_int th_id;
              string_of_int n_pairs]);
  let rec loop i pairs =
    if i <= 0 then rev pairs else
      let id1 = decode_uleb128 command_stream in
      dprint (string_of_int id1 ^ " ");
      let id2 = decode_uleb128 command_stream in
      dprint (string_of_int id2 ^ (if i = 1 then "" else " "));
      let ty1 = Array.get tys id1 in
      let ty2 = Array.get tys id2 in
      loop (i - 1) ((ty2, ty1)::pairs) in
  let pairs = loop n_pairs [] in
  dprint "\n";
  let th = Array.get ths th_id in
  Array.set ths id (Kernel.INST_TYPE pairs th);;

let rec command_loop () =
  match next_command command_stream with
  | None -> ()
  | Some cmd_char ->
     let cmd = Char.code cmd_char in
     dprint (string_of_int cmd ^ ": ");
     if cmd = 0x01 then pft_tyvar ()
     else if cmd = 0x02 then pft_tyop ()
     else if cmd = 0x03 then pft_var ()
     else if cmd = 0x04 then pft_const ()
     else if cmd = 0x05 then pft_comb ()
     else if cmd = 0x06 then pft_abs ()
     else if cmd = 0x07 then pft_new_const ()
     else if cmd = 0x08 then pft_new_type ()
     else if cmd = 0x08 then pft_axiom ()
     else if cmd = 0x10 then pft_refl ()
     else if cmd = 0x11 then pft_trans ()
     else if cmd = 0x12 then pft_mk_comb_thm ()
     else if cmd = 0x13 then pft_abs_thm ()
     else if cmd = 0x14 then pft_beta ()
     else if cmd = 0x15 then pft_assume ()
     else if cmd = 0x16 then pft_eq_mp ()
     else if cmd = 0x17 then pft_deduct_antisym_rule ()
     else if cmd = 0x18 then pft_inst ()
     else if cmd = 0x19 then pft_inst_type ()
     else if cmd = 0x20 then pft_sym ()
     else if cmd = 0x21 then pft_prove_hyp ()
     else if cmd = 0x22 then pft_alpha_thm ()
     else if cmd = 0x30 then pft_new_specification ()
     else if cmd = 0x31 then pft_new_type_definition ()
     else if cmd = 0x40 then pft_compute_init ()
     else if cmd = 0x41 then pft_compute ()
     else if cmd = 0x50 then pft_save ()
     else if cmd = 0x51 then pft_load ()
     (* We allocate enough memory to fit the peak number of objects, so we can
        can ignore deletion requests. *)
     else if 0xE0 <= cmd && cmd <= 0xE3 then ()
     else if 0xF0 <= cmd && cmd <= 0xF3 then ()
     else failwith ("command_loop: unsupported command: " ^ string_of_int cmd);
     command_loop ();;

let _ = expect_pft command_stream;;
let _ = expect_version command_stream 1;;

let ruleset = decode_string command_stream;;
let _ =
  if ruleset <> "candle" then failwith ("unsupported ruleset: " ^ ruleset);;

let _ = command_loop ();;

let _ = Text_io.closeIn command_stream;;
