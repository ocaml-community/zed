(*
 * zed_re.ml
 * ---------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Copyright : (c) 2019, ZAN DoYe <zandoye@gmail.com>
 * Licence   : BSD3
 *
 * This file is a part of Zed, an editor engine.
 *)

open CamomileLibraryDefault.Camomile

module Core = struct
  module Re = URe.Make(Zed_rope.Text_core)

  type t = Re.compiled_regexp
  type match_result = (Re.index * Re.index) option array option

  let compile = Re.compile

  let convert_success arr =
    Array.map
      (function
        | Some sub ->
          let _rope, zip1, zip2 = Re.SubText.context sub in
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
    convert (try Re.search_forward ?sem regexp rope (Zed_rope.Zip.make_f rope idx) with Not_found -> None)

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

  let subtext_to_uChars=
    let module CS = Zed_utils.Convert(Re.SubText) in
    CS.to_uChars
end

module Raw = struct
  module Re = URe.Make(Zed_rope.Text_raw)

  type t = Re.compiled_regexp
  type match_result = (Re.index * Re.index) option array option

  let compile = Re.compile

  let convert_success arr =
    Array.map
      (function
        | Some sub ->
          let _rope, zip1, zip2 = Re.SubText.context sub in
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
    convert (Re.regexp_match ?sem regexp rope (Zed_rope.Zip_raw.make_f rope idx))

  let search_forward ?sem regexp rope idx =
    convert (try Re.search_forward ?sem regexp rope (Zed_rope.Zip_raw.make_f rope idx) with Not_found -> None)

  let search_backward ?sem regexp rope idx =
    let rec loop zip =
      match Re.regexp_match ?sem regexp rope zip with
      | Some arr ->
        Some(convert_success arr)
      | None ->
        if Zed_rope.Zip_raw.at_bos zip then
          None
        else
          loop (Zed_rope.Zip_raw.move (-1) zip)
    in
    loop (Zed_rope.Zip_raw.make_f rope idx)

  let subtext_to_uChars=
    let module CS = Zed_utils.Convert(Re.SubText) in
    CS.to_uChars
end

