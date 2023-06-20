(* available in recent alcotest *)
let alcotest_triple ta tb tc =
  let pp ppf (a, b, c) =
          Format.fprintf ppf "(%a, %a, %a)"
          (Alcotest.pp ta) a
          (Alcotest.pp tb) b
          (Alcotest.pp tc) c
  in
  let eq (a1, b1, c1) (a2, b2, c2) =
    Alcotest.equal ta a1 a2
    &&
    Alcotest.equal tb b1 b2
    &&
    Alcotest.equal tc c1 c2
  in
  Alcotest.testable pp eq

let test_next_error =
  let test ~name input offset ~expected =
    (name, `Quick, (fun () ->
      let got =
        match Zed_utf8.next_error input offset with
        | s -> Result.Ok s
        | exception Zed_utf8.Out_of_bounds -> Error "Out_of_bounds"
      in
      Alcotest.check
        Alcotest.(result (alcotest_triple int int string) string)
        __LOC__ expected got))
  in
  ( "next_error",
  [ test
      ~name:"scalar value too large"
      "\247\165\165\165"
      0
      ~expected:(Ok (0, 0, "scalar value too large in UTF8 sequence"))
  ; test ~name:"out of bounds"
      "cat"
      3
      ~expected:(Error "Out_of_bounds")
  ])

let test_of_utf8 =
  let test_invalid ~name input =
    (name, `Quick, fun () ->
     let raised_correctly =
       match Zed_string.of_utf8 input with
       | (_ : Zed_string.t) -> false
       | exception Zed_utf8.Invalid _ -> true
       | exception _ -> false
     in
     Alcotest.check Alcotest.bool __LOC__ true raised_correctly)
  in
  ( "of_utf8",
  [ test_invalid ~name:"uchar_max (U+110000)" "\xf4\x90\x80\x80"
  ; test_invalid ~name:"U+D800" "\xed\xa0\x80"
  ; test_invalid ~name:"U+DFFF" "\xed\xbf\xbf"
  ])

let test_kill_next_word =
  let test =
    ("kill_next_word", `Quick, fun () ->
      (* Test that [kill_next_word] does not raise [Out_of_bounds] *)
      let engine = Zed_edit.create () in
      let cursor = Zed_edit.new_cursor engine in
      let ctxt = Zed_edit.context engine cursor in
      Zed_edit.insert ctxt (Zed_rope.of_string (Zed_string.of_utf8 "hello"));
      Zed_edit.set_mark ctxt;
      Zed_edit.insert ctxt (Zed_rope.of_string (Zed_string.of_utf8 " world"));
      Zed_edit.goto_mark ctxt;
      Zed_edit.kill_next_word ctxt)
  in
  ( "kill_next_word", [ test ] )

let () = Alcotest.run "zed" [test_next_error; test_of_utf8; test_kill_next_word]
