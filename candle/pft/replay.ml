let here = "candle/pft/";;
let extract_footer_path = here ^ "extract_footer";;

let saved_ths = ref ([]: (string * thm) list);;
let save_th name th = saved_ths := (name, th)::(!saved_ths);;
let load_th name = assoc name (!saved_ths);;
let print_saved () =
  do_list (fun (s, th) ->
      print_endline (s ^ ": ");
      Pretty.print_stdout pp_print_colored_thm th;
      print_newline ()
    ) !saved_ths;;

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
    match Text_io.input1 stream with
    | None -> failwith "EOF"
    | Some cmd ->
       if Char.code cmd <> 0xFF then failwith "bad opcode" else
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

(* --- Replay files --- *)

let trace_path = here ^ "merged.candle.pft.bin";;
let command_stream = Text_io.openIn trace_path;;
let _ = print_types_of_subterms := 2;;

let (n_ty, n_tm, n_th, n_ci) = process_footer trace_path;;

(* Initial values for the arrays *)
let xvar = mk_var ("x", aty);;
let xrefl = REFL xvar;;

let tys = Array.make n_ty aty;;
let tms = Array.make n_tm xvar;;
let ths = Array.make n_th xrefl;;
let cis = Array.make n_ci ([]: thm list);;

let cmd_cnt = ref 1;;
let incr_cnt () = cmd_cnt := !cmd_cnt + 1;;
let print_cnt () = print (string_of_int (!cmd_cnt));;

let pft_tyvar () =
  let id = decode_uleb128 command_stream in
  let name = decode_string command_stream in
  let result = Kernel.mk_vartype name in
  Array.set tys id result;;

let pft_tyop () =
  let id = decode_uleb128 command_stream in
  let name = decode_string command_stream in
  let n_args = decode_uleb128 command_stream in
  let rec loop i args =
    if i <= 0 then rev args else
      let id = decode_uleb128 command_stream in
      let ty = Array.get tys id in
      loop (i - 1) (ty::args) in
  let args = loop n_args [] in
  let result = Kernel.mk_type (name, args) in
  Array.set tys id result;;

let pft_const () =
  let id = decode_uleb128 command_stream in
  let name = decode_string command_stream in
  let type_id = decode_uleb128 command_stream in
  let ty = Array.get tys type_id in
  let result = mk_mconst (name, ty) in
  Array.set tms id result;;

let pft_var () =
  let id = decode_uleb128 command_stream in
  let name = decode_string command_stream in
  let type_id = decode_uleb128 command_stream in
  let ty = Array.get tys type_id in
  let result = Kernel.mk_var (name, ty) in
  Array.set tms id result;;

let pft_abs () =
  let id = decode_uleb128 command_stream in
  let var_id = decode_uleb128 command_stream in
  let body_id = decode_uleb128 command_stream in
  let var_tm = Array.get tms var_id in
  let body_tm = Array.get tms body_id in
  let result = Kernel.mk_abs (var_tm, body_tm) in
  Array.set tms id result;;

let pft_comb () =
  let id = decode_uleb128 command_stream in
  let rator_id = decode_uleb128 command_stream in
  let rand_id = decode_uleb128 command_stream in
  let rator_tm = Array.get tms rator_id in
  let rand_tm = Array.get tms rand_id in
  let result = mk_comb (rator_tm, rand_tm) in
  Array.set tms id result;;

let pft_assume () =
  let id = decode_uleb128 command_stream in
  let tm_id = decode_uleb128 command_stream in
  let tm = Array.get tms tm_id in
  let result = Kernel.ASSUME tm in
  Array.set ths id result

let pft_new_specification () =
  let id = decode_uleb128 command_stream in
  let th_id = decode_uleb128 command_stream in
  let n_names = decode_uleb128 command_stream in
  let rec loop i names =
    if i <= 0 then rev names else
      let name = decode_string command_stream in
      loop (i - 1) (name::names) in
  let names = loop n_names [] in
  let th = Array.get ths th_id in
  let result = Kernel.new_specification th in
  Array.set ths id result;;

let pft_new_type_definition () =
  let id = decode_uleb128 command_stream in
  let th_id = decode_uleb128 command_stream in
  let tyname = decode_string command_stream in
  let absname = decode_string command_stream in
  let repname = decode_string command_stream in
  let th = Array.get ths th_id in
  let absth, repth =
    Kernel.new_basic_type_definition (tyname, (absname, (repname, th))) in
  Array.set ths id absth;
  Array.set ths (id + 1) repth;;

let pft_compute_init () =
  let id = decode_uleb128 command_stream in
  let n_eqs = decode_uleb128 command_stream in
  let rec loop i eqs =
    if i <= 0 then rev eqs else
      let eq_id = decode_uleb128 command_stream in
      let eq = Array.get ths eq_id in
      loop (i - 1) (eq::eqs) in
  let eqs = loop n_eqs [] in
  Array.set cis id eqs;;

let pft_compute () =
  let id = decode_uleb128 command_stream in
  let ci_id = decode_uleb128 command_stream in
  let tm_id = decode_uleb128 command_stream in
  let n_ths = decode_uleb128 command_stream in
  let rec loop i eqs =
    if i <= 0 then rev eqs else
      let eq_id = decode_uleb128 command_stream in
      let eq = Array.get ths eq_id in
      loop (i - 1) (eq::eqs) in
  let eqs = Array.get cis ci_id in
  let code_eqs = loop n_ths [] in
  let tm = Array.get tms tm_id in
  let th = Kernel.compute (eqs, code_eqs) tm in
  Array.set ths id th;;

let pft_save () =
  let name = decode_string command_stream in
  let th_id = decode_uleb128 command_stream in
  let th = Array.get ths th_id in
  save_th name th;;

let pft_load () =
  let th_id = decode_uleb128 command_stream in
  let name = decode_string command_stream in
  let th = load_th name in
  Array.set ths th_id th;;

let pft_sym () =
  let id = decode_uleb128 command_stream in
  let th_id = decode_uleb128 command_stream in
  let th = Array.get ths th_id in
  let result = SYM th in
  Array.set ths id result;;

let pft_prove_hyp () =
  let id = decode_uleb128 command_stream in
  let th1_id = decode_uleb128 command_stream in
  let th2_id = decode_uleb128 command_stream in
  let th1 = Array.get ths th1_id in
  let th2 = Array.get ths th2_id in
  let result = PROVE_HYP th1 th2 in
  Array.set ths id result;;

let pft_refl () =
  let id = decode_uleb128 command_stream in
  let tm_id = decode_uleb128 command_stream in
  let tm = Array.get tms tm_id in
  let result = REFL tm in
  Array.set ths id result;;

let pft_trans () =
  let id = decode_uleb128 command_stream in
  let th1_id = decode_uleb128 command_stream in
  let th2_id = decode_uleb128 command_stream in
  let th1 = Array.get ths th1_id in
  let th2 = Array.get ths th2_id in
  let result = Kernel.TRANS th1 th2 in
  Array.set ths id result;;

let pft_mk_comb_thm () =
  let id = decode_uleb128 command_stream in
  let th1_id = decode_uleb128 command_stream in
  let th2_id = decode_uleb128 command_stream in
  let th1 = Array.get ths th1_id in
  let th2 = Array.get ths th2_id in
  let result = MK_COMB (th1, th2) in
  Array.set ths id result;;

let pft_abs_thm () =
  let id = decode_uleb128 command_stream in
  let tm_id = decode_uleb128 command_stream in
  let th_id = decode_uleb128 command_stream in
  let tm = Array.get tms tm_id in
  let th = Array.get ths th_id in
  let result = ABS tm th in
  Array.set ths id result;;

let pft_new_const () =
  let name = decode_string command_stream in
  let ty_id = decode_uleb128 command_stream in
  let ty = Array.get tys ty_id in
  Kernel.new_constant (name, ty);;

let pft_new_type () =
  let name = decode_string command_stream in
  let arity = decode_uleb128 command_stream in
  Kernel.new_type (name, arity);;

let pft_axiom () =
  let id = decode_uleb128 command_stream in
  let tm_id = decode_uleb128 command_stream in
  let name = decode_string command_stream in
  let tm = Array.get tms tm_id in
  let result = Kernel.new_axiom tm in
  Array.set ths id result;;

let pft_beta () =
  let id = decode_uleb128 command_stream in
  let tm_id = decode_uleb128 command_stream in
  let tm = Array.get tms tm_id in
  let result = Kernel.BETA tm in
  Array.set ths id result;;

let pft_eq_mp () =
  let id = decode_uleb128 command_stream in
  let eq_id = decode_uleb128 command_stream in
  let th_id = decode_uleb128 command_stream in
  let eq = Array.get ths eq_id in
  let th = Array.get ths th_id in
  let result = EQ_MP eq th in
  Array.set ths id result;;

let pft_deduct_antisym_rule () =
  let id = decode_uleb128 command_stream in
  let th1_id = decode_uleb128 command_stream in
  let th2_id = decode_uleb128 command_stream in
  let th1 = Array.get ths th1_id in
  let th2 = Array.get ths th2_id in
  let result = DEDUCT_ANTISYM_RULE th1 th2 in
  Array.set ths id result;;

let pft_inst () =
  let id = decode_uleb128 command_stream in
  let th_id = decode_uleb128 command_stream in
  let n_pairs = decode_uleb128 command_stream in
  let rec loop i pairs =
    if i <= 0 then rev pairs else
      let id1 = decode_uleb128 command_stream in
      let id2 = decode_uleb128 command_stream in
      let tm1 = Array.get tms id1 in
      let tm2 = Array.get tms id2 in
      loop (i - 1) ((tm2, tm1)::pairs) in
  let pairs = loop n_pairs [] in
  let th = Array.get ths th_id in
  let result = Kernel.INST pairs th in
  Array.set ths id result;;

let pft_inst_type () =
  let id = decode_uleb128 command_stream in
  let th_id = decode_uleb128 command_stream in
  let n_pairs = decode_uleb128 command_stream in
  let rec loop i pairs =
    if i <= 0 then rev pairs else
      let id1 = decode_uleb128 command_stream in
      let id2 = decode_uleb128 command_stream in
      let ty1 = Array.get tys id1 in
      let ty2 = Array.get tys id2 in
      loop (i - 1) ((ty2, ty1)::pairs) in
  let pairs = loop n_pairs [] in
  let th = Array.get ths th_id in
  let result = Kernel.INST_TYPE pairs th in
  Array.set ths id result;;

let pft_expect () =
  let id = decode_uleb128 command_stream in
  let th = Array.get ths id in
  let actual_hyps = hyp th in
  let actual_concl = concl th in
  let n_hyps = decode_uleb128 command_stream in
  let rec loop i hyps =
    if i <= 0 then rev hyps else
      let hyp_id = decode_uleb128 command_stream in
      let tm = Array.get tms hyp_id in
      loop (i - 1) (tm::hyps) in
  let expected_hyps = loop n_hyps [] in
  let subset_aconv l1 l2 = forall (fun t1 -> exists (aconv t1) l2) l1 in
  let set_eq_aconv l1 l2 = subset_aconv l1 l2 && subset_aconv l2 l1 in
  if not (set_eq_aconv expected_hyps actual_hyps) then failwith "mismatched hypotheses!";
  let concl_id = decode_uleb128 command_stream in
  let expected_concl = Array.get tms concl_id in
  if not (aconv expected_concl actual_concl) then failwith "mismatched conclusion!";
  ();;

let rec command_loop () =
  match next_command command_stream with
  | None -> print "Success!"; ()
  | Some cmd_char ->
     let cmd = Char.code cmd_char in
     if cmd = 0x01 then pft_tyvar ()
     else if cmd = 0x02 then pft_tyop ()
     else if cmd = 0x03 then pft_var ()
     else if cmd = 0x04 then pft_const ()
     else if cmd = 0x05 then pft_comb ()
     else if cmd = 0x06 then pft_abs ()
     else if cmd = 0x07 then pft_new_const ()
     else if cmd = 0x08 then pft_new_type ()
     else if cmd = 0x09 then pft_axiom ()
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
     else if cmd = 0x30 then pft_new_specification ()
     else if cmd = 0x31 then pft_new_type_definition ()
     else if cmd = 0x40 then pft_compute_init ()
     else if cmd = 0x41 then pft_compute ()
     else if cmd = 0x50 then pft_save ()
     else if cmd = 0x51 then pft_load ()
     (* We allocate enough memory to fit the peak number of objects, so we can
        can ignore deletion requests. *)
     else if 0xE0 <= cmd && cmd <= 0xE3 then (
       decode_uleb128 command_stream; ())
     else if cmd = 0xEF then pft_expect ()
     else if 0xF0 <= cmd && cmd <= 0xF3 then (
       decode_uleb128 command_stream;
       decode_uleb128 command_stream; ())
     else if cmd = 0xFF then (
       decode_uleb128 command_stream;
       decode_uleb128 command_stream;
       decode_uleb128 command_stream;
       decode_uleb128 command_stream;
       Text_io.input1 command_stream;
       Text_io.input1 command_stream; ())
     else failwith ("command_loop: unsupported command: " ^ string_of_int cmd);
     incr_cnt ();
     command_loop ();;

let _ = expect_pft command_stream;;
let _ = expect_version command_stream 1;;
let ruleset = decode_string command_stream;;
let _ =
  if ruleset <> "candle" then failwith ("unsupported ruleset: " ^ ruleset);;
let _ = incr_cnt ();;

let _ = command_loop ();;
let _ = print_cnt ();;

let _ = Text_io.closeIn command_stream;;
