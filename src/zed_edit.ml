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
  mutable text : Zed_rope.t;
  (* The contents of the engine. *)

  mutable lines : Zed_lines.t;
  (* The set of line position of [text]. *)

  changes : (int * int * int) event;
  send_changes : (int * int * int) -> unit;
  (* Changes of the contents. *)

  erase_mode : bool signal;
  set_erase_mode : bool -> unit;
  (* The current erase mode. *)

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
  let erase_mode, set_erase_mode = S.create false in
  {
    text = Zed_rope.empty;
    lines = Zed_lines.empty;
    changes;
    send_changes;
    erase_mode;
    set_erase_mode;
    editable;
    move;
  }

(* +-----------------------------------------------------------------+
   | State                                                           |
   +-----------------------------------------------------------------+ *)

let text engine = engine.text
let lines engine = engine.lines
let changes engine = engine.changes
let erase_mode engine = engine.erase_mode
let get_erase_mode engine = S.value engine.erase_mode
let set_erase_mode engine state = engine.set_erase_mode state

(* +-----------------------------------------------------------------+
   | Cursors                                                         |
   +-----------------------------------------------------------------+ *)

let new_cursor engine =
  Zed_cursor.create (Zed_rope.length engine.text) engine.changes 0

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

let goto ctx position =
  Zed_cursor.goto ctx.cursor position

let move ctx delta =
  Zed_cursor.move ctx.cursor delta

let position ctx =
  Zed_cursor.get_position ctx.cursor

let at_bob ctx =
  Zed_cursor.get_position ctx.cursor = 0

let at_eob ctx =
  Zed_cursor.get_position ctx.cursor = Zed_rope.length ctx.edit.text

let insert ctx rope =
  let position = Zed_cursor.get_position ctx.cursor in
  if not ctx.check || ctx.edit.editable position then begin
    let len = Zed_rope.length rope in
    if S.value ctx.edit.erase_mode then begin
      let text_len = Zed_rope.length ctx.edit.text in
      if position + len > text_len then begin
        ctx.edit.text <- Zed_rope.replace ctx.edit.text position (text_len - position) rope;
        ctx.edit.lines <- Zed_lines.replace ctx.edit.lines position (text_len - position) (Zed_lines.of_rope rope);
        ctx.edit.send_changes (position, len, text_len - position)
      end else begin
        ctx.edit.text <- Zed_rope.replace ctx.edit.text position len rope;
        ctx.edit.lines <- Zed_lines.replace ctx.edit.lines position len (Zed_lines.of_rope rope);
        ctx.edit.send_changes (position, len, len);
      end;
      Zed_cursor.move ctx.cursor len
    end else begin
      ctx.edit.text <- Zed_rope.insert ctx.edit.text position rope;
      ctx.edit.lines <- Zed_lines.insert ctx.edit.lines position (Zed_lines.of_rope rope);
      ctx.edit.send_changes (position, len, 0);
      Zed_cursor.move ctx.cursor len
    end
  end

let remove ctx len =
  let position = Zed_cursor.get_position ctx.cursor in
  if not ctx.check || ctx.edit.editable position then begin
    let text_len = Zed_rope.length ctx.edit.text in
    if position + len > text_len then begin
      ctx.edit.text <- Zed_rope.remove ctx.edit.text position (text_len - position);
      ctx.edit.lines <- Zed_lines.remove ctx.edit.lines position (text_len - position);
      ctx.edit.send_changes (position, 0, text_len - position)
    end else begin
      ctx.edit.text <- Zed_rope.remove ctx.edit.text position len;
      ctx.edit.lines <- Zed_lines.remove ctx.edit.lines position len;
      ctx.edit.send_changes (position, 0, len);
    end
  end

let next_char ctx =
  if not (at_eob ctx) then move ctx 1

let prev_char ctx =
  if not (at_bob ctx) then move ctx (-1)

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
