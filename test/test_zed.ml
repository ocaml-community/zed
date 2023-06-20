let () =
  let s = "\247\165\165\165" in
  let (ofs, _, message) = Zed_utf8.next_error s 0 in
  Printf.printf "next_error (scalar value too large) = (%d, _, %S)\n" ofs message

let () =
  let str = "cat" in
  let ofs = (String.length str)  in
  Printf.printf (match Zed_utf8.next_error str ofs with
    | _ -> "OK.\n"
    | exception Zed_utf8.Out_of_bounds -> "Zed_utf8.Out_of_bounds.\n"
    | exception Zed_utf8.Invalid _ -> "Zed_utf8.Invalid.\n")

let of_utf8_exception_handling_test str =
  match Zed_string.of_utf8 str with
  | _ -> Printf.printf "OK\n"
  | exception Zed_string.Invalid _ -> Printf.printf "Zed_string.Invalid raised.\n"
  | exception Zed_utf8.Invalid _ -> Printf.printf "Zed_utf8.Invalid raised.\n"
  | exception _ -> Printf.printf "ERROR, another exception has been raised.\n"

let () =
  let uchar_max_str = "\xf4\x90\x80\x80" in (* U+110000 *)
  let d800_str = "\xed\xa0\x80" in (* U+D800 *)
  let dfff_str = "\xed\xbf\xbf" in (* U+DFFF *)
  of_utf8_exception_handling_test uchar_max_str;
  of_utf8_exception_handling_test d800_str;
  of_utf8_exception_handling_test dfff_str
