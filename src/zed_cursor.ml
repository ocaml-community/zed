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
  get_lines : unit -> Zed_lines.t;
  coordinates : (int * int) signal;
  line : int signal;
  column : int signal;
  wanted_column : int signal;
  set_wanted_column : int -> unit;
}

let create length changes get_lines position wanted_column =
  if position < 0 || position > length then raise Out_of_bounds;
  let position, set_position = S.create position in
  let wanted_column, set_wanted_column = S.create wanted_column in
  let compute_coordinates position =
    let lines = get_lines () in
    let index = Zed_lines.line_index lines position in
    (index, position - Zed_lines.line_start lines index)
  in
  let coordinates = S.map compute_coordinates position in
  let cursor = {
    position;
    set_position;
    length;
    changes;
    handle_changes = E.never;
    get_lines;
    coordinates;
    line = S.map fst coordinates;
    column = S.map snd coordinates;
    wanted_column;
    set_wanted_column;
  } in
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
      else if position < start - delta  then
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

let copy cursor =
  create
    cursor.length
    cursor.changes
    cursor.get_lines
    (S.value cursor.position)
    (S.value cursor.wanted_column)

let position cursor = cursor.position
let get_position cursor = S.value cursor.position
let line cursor = cursor.line
let get_line cursor = S.value cursor.line
let column cursor = cursor.column
let get_column cursor = S.value cursor.column
let coordinates cursor = cursor.coordinates
let get_coordinates cursor = S.value cursor.coordinates
let wanted_column cursor = cursor.wanted_column
let get_wanted_column cursor = S.value cursor.wanted_column
let set_wanted_column cursor column = cursor.set_wanted_column column

let move cursor ?(set_wanted_column=true) delta =
  let new_position = S.value cursor.position + delta in
  if new_position < 0 || new_position > cursor.length then
    raise Out_of_bounds
  else begin
    cursor.set_position new_position;
    if set_wanted_column then cursor.set_wanted_column (S.value cursor.column)
  end

let goto cursor ?(set_wanted_column=true) position =
  if position < 0 || position > cursor.length then
    raise Out_of_bounds
  else begin
    cursor.set_position position;
    if set_wanted_column then cursor.set_wanted_column (S.value cursor.column)
  end
