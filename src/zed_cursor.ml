(*
 * zed_cursor.ml
 * -------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Zed, an editor engine.
 *)

open React

exception Out_of_bounds

type t = {
  position : int signal;
  set_position : int -> unit;
  mutable length : int;
  changes : (int * int * int) event;
  mutable handle_changes : unit event;
}

let create length changes position =
  if position < 0 || position > length then raise Out_of_bounds;
  let position, set_position = S.create position in
  let cursor = { position; set_position; length; changes; handle_changes = E.never } in
  let handle_changes (start, added, removed) =
    let delta = added - removed in
    cursor.length <- cursor.length + delta;
    if cursor.length < 0 then raise Out_of_bounds;
    let position = S.value position in
    (* Move the cursor if it is after the start of the changes. *)
    if position > start then begin
      if delta >= 0 then
        (* Text has been inserted, advance the cursor. *)
        set_position (position + delta)
      else if start - delta <= position then
        (* Text has been removed and the removed block contains the
           cursor, move it at the beginning of the removed block. *)
        set_position start
      else
        (* Text has been removed before the cursor, move back the
           cursor. *)
        set_position (position + delta)
    end
  in
  cursor.handle_changes <- E.map handle_changes changes;
  cursor

let copy cursor = create cursor.length cursor.changes (S.value cursor.position)

let position cursor = cursor.position

let move cursor delta =
  let new_position = S.value cursor.position + delta in
  if new_position < 0 || new_position > cursor.length then
    raise Out_of_bounds
  else
    cursor.set_position new_position
