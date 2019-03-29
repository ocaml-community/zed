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

type t= {
  core: UChar.t;
  combined: UChar.t list;
  width: int;
  size: int;
}

type pos=
  | Core
  | Combined of int

let to_raw t= t.core :: t.combined
let to_array t= Array.of_list (t.core :: t.combined)

type char_prop=
  | Printable of int
  | Other
  | Null

let zero=
  {
    core= UChar.of_int 0;
    combined= [];
    width= 0;
    size= 1;
  }

let prop_uChar uChar=
  match CharInfo_width.width uChar with
  | -1 -> Other
  | 0->
    if UChar.code uChar = 0
    then Null
    else Printable 0
  | w-> Printable w

let prop t= prop_uChar t.core

let length t= 1 + List.length t.combined
let width t= t.width
let size t= t.size
let calc_size' l= List.length l + 1
let calc_size t= calc_size' t.combined
let out_of_range t i= i < 0 || i >= t.size
let get t i=
  if i = 0
  then t.core
  else List.nth t.combined (i-1)
let get_opt t i=
  if i = 0
  then Some t.core
  else
    try Some (List.nth t.combined (i-1))
    with _-> None

let append ch mark=
  match prop_uChar mark with
  | Printable 0-> 
    let combined= List.append ch.combined [mark]
    and size= ch.size + 1 in
    { ch with combined; size }
  | _-> failwith "combing mark expected"

let compare_core t1 t2= UChar.compare t1.core t2.core

let compare_raw t1 t2= Zed_utils.list_compare
  ~compare:UChar.compare
  (to_raw t1) (to_raw t2)

let mix_uChar yChar uChar=
  match prop_uChar uChar with
  | Printable 0->
    let combined= List.append yChar.combined [uChar] in
    Ok { yChar with combined; size= yChar.size + 1 }
  | Other->
    let new_char= { core= uChar; combined= []; width= -1; size= 1 } in
    Error new_char
  | Null->
    let new_char= { core= uChar; combined= []; width= 0; size= 1 } in
    Error new_char
  | Printable w->
    let new_char= { core= uChar; combined= []; width= w; size= 1 } in
    Error new_char

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
  | Some (Printable w, uChar), tl->
    let combined, tl= subsequent tl in
    Some { core= uChar; combined; width= w; size= calc_size' combined }, tl
  | Some (Null, uChar), tl->
    Some { core= uChar; combined= []; width= 0; size= 1 }, tl
  | Some (Other, uChar), tl->
    Some { core= uChar; combined= []; width= -1; size= 1 }, tl

let unsafe_of_char c=
  let core= UChar.of_char c in
  { core;
    combined= [];
    width= CharInfo_width.width core;
    size= 1;
  }

let unsafe_of_uChar uChar=
  { core= uChar;
    combined= [];
    width= CharInfo_width.width uChar;
    size= 1;
  }

module US(US:UnicodeString.Type) = struct
  module Convert = Zed_utils.Convert(US)
  let of_t t= t.core :: t.combined |> Convert.of_list
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

