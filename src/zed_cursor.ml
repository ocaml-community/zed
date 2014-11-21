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

type action =
  | User_move of int
  | Text_modification of (int * int * int) (* start, added, removed *)

type t = {
  position : int signal;
  send : action -> unit;
  length : int ref;
  changes : (int * int * int) event;
  get_lines : unit -> Zed_lines.t;
  coordinates : (int * int) signal;
  line : int signal;
  column : int signal;
  wanted_column : int signal;
  set_wanted_column : int -> unit;
}

let create length changes get_lines position wanted_column =
  if position < 0 || position > length then raise Out_of_bounds;
  let length = ref length in
  let user_moves, send = E.create () in
  let update_position position action =
    match action with
    | User_move pos -> pos
    | Text_modification (start, added, removed) ->
      let delta = added - removed in
      length := !length + delta;
      if !length < 0 then raise Out_of_bounds;
      (* Move the cursor if it is after the start of the changes. *)
      if position > start then begin
        if delta >= 0 then
          (* Text has been inserted, advance the cursor. *)
          position + delta
        else if position < start - delta  then
          (* Text has been removed and the removed block contains the
             cursor, move it at the beginning of the removed block. *)
          start
        else
          (* Text has been removed before the cursor, move back the
             cursor. *)
          position + delta
      end else
        position
  in
  let text_modifications = E.map (fun x -> Text_modification x) changes in
  let position =
    S.fold update_position position (E.select [user_moves; text_modifications])
  in
  let compute_coordinates position =
    let lines = get_lines () in
    let index = Zed_lines.line_index lines position in
    (index, position - Zed_lines.line_start lines index)
  in
  let coordinates = S.map compute_coordinates position in
  let wanted_column, set_wanted_column = S.create wanted_column in
  {
    position;
    send;
    length;
    changes;
    get_lines;
    coordinates;
    line = S.map fst coordinates;
    column = S.map snd coordinates;
    wanted_column;
    set_wanted_column;
  }

let copy cursor =
  create
    !(cursor.length)
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
  if new_position < 0 || new_position > !(cursor.length) then
    raise Out_of_bounds
  else begin
    cursor.send (User_move new_position);
    if set_wanted_column then cursor.set_wanted_column (S.value cursor.column)
  end

let goto cursor ?(set_wanted_column=true) position =
  if position < 0 || position > !(cursor.length) then
    raise Out_of_bounds
  else begin
    cursor.send (User_move position);
    if set_wanted_column then cursor.set_wanted_column (S.value cursor.column)
  end
