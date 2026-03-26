
exception Sys_error of string;;
let pp_exn e =
  match e with
  | Sys_error s ->
     Pretty_printer.app_block "Sys_error" [Pretty_printer.pp_string s]
  | _ -> pp_exn e;;

let chdir dir =
  let len = String.size dir in
  let _ = if len = 0 then raise (Sys_error "Sys.chdir: empty input") else () in
  let bytes = Word8_array.array (len + 1) (Word8.fromInt 0) in
  let _ = Word8_array.copyVec dir 0 len bytes 0 in
  let _ = Runtime.customFFI "chdir" bytes in
  let ret = Word8.toInt (Word8_array.sub bytes 0) in
  if ret <> 0 then raise (Sys_error "Sys.chdir: unsuccessful FFI") else ();;

let _ = chdir "../..";;
