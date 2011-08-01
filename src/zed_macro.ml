(*
 * zed_macro.ml
 * ------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Zed, an editor engine.
 *)

open React

type 'a t = {
  recording : bool signal;
  set_recording : bool -> unit;
  mutable tmp_macro : 'a list;
  mutable macro : 'a list;
}

let create macro =
  let recording, set_recording = S.create false in
  { recording; set_recording; macro; tmp_macro = [] }

let recording r = r.recording

let get_recording r = S.value r.recording

let set_recording r state =
  if state <> S.value r.recording then
    match state with
      | true ->
          r.tmp_macro <- [];
          r.set_recording true
      | false ->
          r.macro <- List.rev r.tmp_macro;
          r.set_recording false

let add r x =
  if S.value r.recording then
    r.tmp_macro <- x :: r.tmp_macro

let contents r = r.macro
