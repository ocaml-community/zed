(*
 * zed_re.ml
 * ---------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Zed, an editor engine.
 *)

open CamomileLibraryDyn.Camomile

module Re = URe.Make(Zed_rope.Text)

type t = Re.compiled_regexp
type match_result = (Zed_rope.Zip.t * Zed_rope.Zip.t) option array option

let compile = Re.compile

let convert_success arr =
  Array.map
    (function
       | Some sub ->
           let rope, zip1, zip2 = Re.SubText.context sub in
           Some(zip1, zip2)
       | None ->
           None)
    arr

let convert = function
  | Some arr ->
      Some(convert_success arr)
  | None ->
      None

let regexp_match ?sem regexp rope idx =
  convert (Re.regexp_match ?sem regexp rope (Zed_rope.Zip.make_f rope idx))

let search_forward ?sem regexp rope idx =
  convert (Re.search_forward ?sem regexp rope (Zed_rope.Zip.make_f rope idx))

let search_backward ?sem regexp rope idx =
  let rec loop zip =
    match Re.regexp_match ?sem regexp rope zip with
      | Some arr ->
          Some(convert_success arr)
      | None ->
          if Zed_rope.Zip.at_bos zip then
            None
          else
            loop (Zed_rope.Zip.move (-1) zip)
  in
  loop (Zed_rope.Zip.make_f rope idx)
