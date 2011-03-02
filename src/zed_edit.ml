(*
 * zed_edit.ml
 * -----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Zed, an editor engine.
 *)

open React

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type t = {
  text : Zed_rope.t signal;
  (* The contents of the engine. *)

  changes : (Zed_rope.t * int * int * int) event;
  send_changes : (Zed_rope.t * int * int * int) -> unit;
  (* Changes of the contents. *)

  cursor_changes : (int * int) event;
  (* Same as [changes] but contains only [(start, delta)]. This is
     used for creating cursors. *)

  editable : int -> bool;
  (* The editable function of the engine. *)

  move : int -> int -> int;
  (* The move function of the engine. *)
}

(* +-----------------------------------------------------------------+
   | Creation                                                        |
   +-----------------------------------------------------------------+ *)

let create ?(editable=fun pos -> true) ?(move=(+)) () =
  let changes, send_changes = E.create () in
  {
    text = S.hold ~eq:(==) Zed_rope.empty (E.map (fun (text, start, added, removed) -> text) changes);
    changes;
    send_changes;
    cursor_changes = E.map (fun (text, start, added, removed) -> (start, added - removed)) changes;
    editable;
    move;
  }

(* +-----------------------------------------------------------------+
   | State                                                           |
   +-----------------------------------------------------------------+ *)

let text engine = engine.text
let changes engine = engine.changes

(* +-----------------------------------------------------------------+
   | Cursors                                                         |
   +-----------------------------------------------------------------+ *)

let new_cursor engine =
  Zed_cursor.create (Zed_rope.length (S.value engine.text)) engine.cursor_changes 0

(* +-----------------------------------------------------------------+
   | Actions                                                         |
   +-----------------------------------------------------------------+ *)

type context = {
  edit : t;
  cursor : Zed_cursor.t;
  check : bool;
}

let create_context ?(check=true) edit cursor =
  { edit; cursor; check }

module Actions(Context : sig val context : context end) = struct

  open Context

  (* Aliases *)
  let { edit; cursor; check } = Context.context
  let text = S.value edit.text
  let length = Zed_rope.length text
  let position = S.value (Zed_cursor.position cursor)

  let perform action =
    let new_text, new_position, added, removed = action () in
    (* Pass the movement through the [move] function. *)
    let new_position = if check then edit.move position (new_position - position) else new_position in
    (* Start of modifications. *)
    let start = min position new_position in
    if text == new_text then
      (* If the text has not changed, just move the cursor. *)
      Zed_cursor.move cursor (added - removed)
    else if added >= removed then begin
      edit.send_changes (new_text, start, added, removed);
      Zed_cursor.move cursor (added - removed)
    end else begin
      Zed_cursor.move cursor (added - removed);
      edit.send_changes (new_text, start, added, removed);
    end

  (* Each of the followin actions returns the new text, the new
     position, the number of characters added and the number of
     characters removed. *)

  let insert rope =
    (Zed_rope.insert text position rope,
     position + Zed_rope.length rope,
     Zed_rope.length rope, 0)

  let next_char () =
    if position = length then
      (text, position, 0, 0)
    else
      (text, position + 1, 0, 0)

  let prev_char () =
    if position = 0 then
      (text, position, 0, 0)
    else
      (text, position - 1, 0, 0)
end

let insert ctx text =
  let module A = Actions(struct let context = ctx end) in
  A.perform (fun () -> A.insert text)

let next_char ctx =
  let module A = Actions(struct let context = ctx end) in
  A.perform A.next_char

let prev_char ctx =
  let module A = Actions(struct let context = ctx end) in
  A.perform A.prev_char

(* +-----------------------------------------------------------------+
   | Action by names                                                 |
   +-----------------------------------------------------------------+ *)

type action =
  | Next_char
  | Prev_char

let exec ctx act =
  match act with
    | Next_char -> next_char ctx
    | Prev_char -> prev_char ctx

let action_of_name = function
  | "next-char" -> Next_char
  | "prev-char" -> Prev_char
  | _ -> raise Not_found

let name_of_action = function
  | Next_char -> "next-char"
  | Prev_char -> "prev-char"
