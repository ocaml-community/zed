(*
 * zed_char.ml
 * -----------
 * Copyright : (c) 2019, ZAN DoYe <zandoye@gmail.com>
 * Licence   : BSD3
 *
 * This file is a part of Zed, an editor engine.
 *)


open CamomileLibraryDefault.Camomile
open Result

type t= Zed_utf8.t

(*
let to_raw t= t.core :: t.combined
let to_array t= Array.of_list (t.core :: t.combined)
*)

type char_prop=
  | Printable of int
  | Other
  | Null

let to_raw= Zed_utf8.explode
let to_array t= Array.of_list (Zed_utf8.explode t)

let zero= String.make 1 (Char.chr 0)

let core t= Zed_utf8.unsafe_extract t 0
let combined t= List.tl (Zed_utf8.explode t)

external id : 'a -> 'a = "%identity"
let of_utf8 : string -> t= id
let to_utf8 : t -> string= id

let prop_uChar uChar=
  match CharInfo_width.width uChar with
  | -1 -> Other
  | 0->
    if UChar.code uChar = 0
    then Null
    else Printable 0
  | w-> Printable w

let prop t= prop_uChar (Zed_utf8.unsafe_extract t 0)

let length= Zed_utf8.length
let size= length

let width t= CharInfo_width.width (Zed_utf8.unsafe_extract t 0)

let out_of_range t i= i < 0 || i >= size t
let get= Zed_utf8.get

let get_opt t i=
  try Some (get t i)
  with _-> None

let append ch mark=
  match prop_uChar mark with
  | Printable 0-> ch ^ (Zed_utf8.singleton mark)
  | _-> failwith "combing mark expected"

let compare_core t1 t2=
  let core1= Zed_utf8.unsafe_extract t1 0
  and core2= Zed_utf8.unsafe_extract t2 0 in
  UChar.compare core1 core2

let compare_raw= Zed_utf8.compare

let compare= compare_raw

let mix_uChar zChar uChar=
  match prop_uChar uChar with
  | Printable 0->
    Ok (zChar ^ (Zed_utf8.singleton uChar))
  | _->
    Error (Zed_utf8.singleton uChar)

let rec first_core uChars=
  match uChars with
  | []-> None, []
  | uChar::tl->
    let prop= prop_uChar uChar in
    match prop with
    | Printable w->
      if w > 0
      then Some (prop, uChar), tl
      else first_core tl
    | Other-> Some (prop, uChar), tl
    | Null-> Some (prop, uChar), tl

let rec subsequent uChars=
  match uChars with
  | []-> [], []
  | uChar::tl->
    let prop= prop_uChar uChar in
    match prop with
    | Printable w->
      if w > 0 then
        [], uChars
      else
        let seq, remain= subsequent tl in
        uChar :: seq, remain
    | _-> [], uChars

let of_uChars uChars=
  match first_core uChars with
  | None, tl-> None, tl
  | Some (Printable _w, uChar), tl->
    let combined, tl= subsequent tl in
    Some (Zed_utf8.implode (uChar::combined)), tl
  | Some (Null, uChar), tl->
    Some (Zed_utf8.singleton uChar) ,tl
  | Some (Other, uChar), tl->
    Some (Zed_utf8.singleton uChar) ,tl

let rec zChars_of_uChars uChars=
  match of_uChars uChars with
  | None, tl-> [], tl
  | Some zChar, tl-> let zChars, tl= zChars_of_uChars tl in
    zChar :: zChars, tl

let unsafe_of_char c=
  Zed_utf8.singleton (UChar.of_char c)

let unsafe_of_uChar uChar= Zed_utf8.singleton uChar

let for_all= Zed_utf8.for_all
let iter= Zed_utf8.iter

module US(US:UnicodeString.Type) = struct
  module Convert = Zed_utils.Convert(US)
  let of_t t= Zed_utf8.explode t |> Convert.of_list
  let to_t us=
    let len= US.length us in
    let rec create i=
      if i < len
      then US.get us i :: create (i+1)
      else []
    in
    let uChars= create 0 in
    of_uChars uChars
  let to_t_exn us=
    match to_t us with
    | Some t, _-> t
    | _-> failwith "to_t_exn"
end

