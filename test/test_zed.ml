let () =
  let s = "\247\165\165\165" in
  let (ofs, _, message) = Zed_utf8.next_error s 0 in
  Printf.printf "next_error (scalar value too large) = (%d, _, %S)\n" ofs message
