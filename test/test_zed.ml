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

