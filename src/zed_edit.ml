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

type clipboard = {
  clipboard_get : unit -> Zed_rope.t;
  clipboard_set : Zed_rope.t -> unit;
}

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

  clipboard : clipboard;
  (* The clipboard for this engine. *)

  mark : Zed_cursor.t;
  (* The cursor that points to the mark. *)

  selection : bool signal;
  set_selection : bool -> unit;
  (* The current selection state. *)
}

(* +-----------------------------------------------------------------+
   | Creation                                                        |
   +-----------------------------------------------------------------+ *)

let create ?(editable=fun pos -> true) ?(move=(+)) ?clipboard () =
  let changes, send_changes = E.create () in
  let erase_mode, set_erase_mode = S.create false in
  let selection, set_selection = S.create false in
  let clipboard =
    match clipboard with
      | Some clipboard ->
          clipboard
      | None ->
          let r = ref Zed_rope.empty in
          { clipboard_get = (fun () -> !r);
            clipboard_set = (fun x -> r := x) }
  in
  {
    text = Zed_rope.empty;
    lines = Zed_lines.empty;
    changes;
    send_changes;
    erase_mode;
    set_erase_mode;
    editable;
    move;
    clipboard;
    mark = Zed_cursor.create 0 changes 0;
    selection;
    set_selection;
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
let mark engine = engine.mark
let selection engine = engine.selection
let get_selection engine = S.value engine.selection
let set_selection engine state = engine.set_selection state

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

let goto ctx new_position =
  if ctx.check then
    let position = Zed_cursor.get_position ctx.cursor in
    Zed_cursor.goto ctx.cursor (ctx.edit.move position (new_position - position))
  else
    Zed_cursor.goto ctx.cursor new_position

let move ctx delta =
  if ctx.check then
    Zed_cursor.goto ctx.cursor (ctx.edit.move (Zed_cursor.get_position ctx.cursor) delta)
  else
    Zed_cursor.move ctx.cursor delta

let position ctx =
  Zed_cursor.get_position ctx.cursor

let at_bol ctx =
  let position = Zed_cursor.get_position ctx.cursor in
  position = Zed_lines.line_start ctx.edit.lines (Zed_lines.line_index ctx.edit.lines position)

let at_eol ctx =
  let position = Zed_cursor.get_position ctx.cursor in
  let index = Zed_lines.line_index ctx.edit.lines position in
  if index = Zed_lines.count ctx.edit.lines then
    position = Zed_rope.length ctx.edit.text
  else
    position = Zed_lines.line_start ctx.edit.lines (index + 1) - 1

let at_bot ctx =
  Zed_cursor.get_position ctx.cursor = 0

let at_eot ctx =
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
      move ctx len
    end else begin
      ctx.edit.text <- Zed_rope.insert ctx.edit.text position rope;
      ctx.edit.lines <- Zed_lines.insert ctx.edit.lines position (Zed_lines.of_rope rope);
      ctx.edit.send_changes (position, len, 0);
      move ctx len
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
  if not (at_eot ctx) then move ctx 1

let prev_char ctx =
  if not (at_bot ctx) then move ctx (-1)

let next_line ctx =
  let position = Zed_cursor.get_position ctx.cursor in
  let index = Zed_lines.line_index ctx.edit.lines position in
  if index = Zed_lines.count ctx.edit.lines then
    goto ctx (Zed_rope.length ctx.edit.text)
  else
    goto ctx (Zed_lines.line_start ctx.edit.lines (index + 1))

let prev_line ctx =
  let position = Zed_cursor.get_position ctx.cursor in
  let index = Zed_lines.line_index ctx.edit.lines position in
  if index = 0 then
    Zed_cursor.goto ctx.cursor 0
  else
    Zed_cursor.goto ctx.cursor (Zed_lines.line_start ctx.edit.lines (index - 1))

let goto_bol ctx =
  goto ctx (Zed_lines.line_start ctx.edit.lines (Zed_lines.line_index ctx.edit.lines (Zed_cursor.get_position ctx.cursor)))

let goto_eol ctx =
  let index = Zed_lines.line_index ctx.edit.lines (Zed_cursor.get_position ctx.cursor) in
  if index = Zed_lines.count ctx.edit.lines then
    goto ctx (Zed_rope.length ctx.edit.text)
  else
    goto ctx (Zed_lines.line_start ctx.edit.lines (index + 1) - 1)

let goto_bot ctx =
  goto ctx 0

let goto_eot ctx =
  goto ctx (Zed_rope.length ctx.edit.text)

let delete_next_char ctx =
  if not (at_eot ctx) then begin
    ctx.edit.set_selection false;
    remove ctx 1
  end

let delete_prev_char ctx =
  if not (at_bot ctx) then begin
    ctx.edit.set_selection false;
    move ctx (-1);
    remove ctx 1
  end

let delete_next_line ctx =
  ctx.edit.set_selection false;
  let position = Zed_cursor.get_position ctx.cursor in
  let index = Zed_lines.line_index ctx.edit.lines position in
  if index = Zed_lines.count ctx.edit.lines then
    remove ctx (Zed_rope.length ctx.edit.text - position)
  else
    remove ctx (Zed_lines.line_start ctx.edit.lines (index + 1) - position)

let delete_prev_line ctx =
  ctx.edit.set_selection false;
  let position = Zed_cursor.get_position ctx.cursor in
  let start = Zed_lines.line_start ctx.edit.lines (Zed_lines.line_index ctx.edit.lines position) in
  goto ctx start;
  let new_position = Zed_cursor.get_position ctx.cursor in
  if new_position < position then remove ctx (position - new_position)

let kill_next_line ctx =
  let position = Zed_cursor.get_position ctx.cursor in
  let index = Zed_lines.line_index ctx.edit.lines position in
  if index = Zed_lines.count ctx.edit.lines then begin
    ctx.edit.clipboard.clipboard_set (Zed_rope.after ctx.edit.text position);
    ctx.edit.set_selection false;
    remove ctx (Zed_rope.length ctx.edit.text - position)
  end else begin
    let len = Zed_lines.line_start ctx.edit.lines (index + 1) - position in
    ctx.edit.clipboard.clipboard_set (Zed_rope.sub ctx.edit.text position len);
    ctx.edit.set_selection false;
    remove ctx len
  end

let kill_prev_line ctx =
  let position = Zed_cursor.get_position ctx.cursor in
  let start = Zed_lines.line_start ctx.edit.lines (Zed_lines.line_index ctx.edit.lines position) in
  goto ctx start;
  let new_position = Zed_cursor.get_position ctx.cursor in
  if new_position <= position then begin
    ctx.edit.clipboard.clipboard_set (Zed_rope.sub ctx.edit.text position (position - new_position));
    ctx.edit.set_selection false;
    remove ctx (position - new_position)
  end

let switch_erase_mode ctx =
  ctx.edit.set_erase_mode (not (S.value ctx.edit.erase_mode))

let set_mark ctx =
  Zed_cursor.goto ctx.edit.mark (Zed_cursor.get_position ctx.cursor);
  ctx.edit.set_selection true

let goto_mark ctx =
  goto ctx (Zed_cursor.get_position ctx.edit.mark)

let copy ctx =
  if S.value ctx.edit.selection then begin
    let a = Zed_cursor.get_position ctx.cursor and b = Zed_cursor.get_position ctx.edit.mark in
    let a = min a b and b = max a b in
    ctx.edit.clipboard.clipboard_set (Zed_rope.sub ctx.edit.text a (b - a));
    ctx.edit.set_selection false
  end

let kill ctx =
  if S.value ctx.edit.selection then begin
    let a = Zed_cursor.get_position ctx.cursor and b = Zed_cursor.get_position ctx.edit.mark in
    let a = min a b and b = max a b in
    ctx.edit.clipboard.clipboard_set (Zed_rope.sub ctx.edit.text a (b - a));
    ctx.edit.set_selection false;
    goto ctx a;
    let a = Zed_cursor.get_position ctx.cursor in
    if a <= b then remove ctx (b - a)
  end

let yank ctx =
  ctx.edit.set_selection false;
  insert ctx (ctx.edit.clipboard.clipboard_get ())

(* +-----------------------------------------------------------------+
   | Action by names                                                 |
   +-----------------------------------------------------------------+ *)

type action =
  | Next_char
  | Prev_char
  | Next_line
  | Prev_line
  | Goto_bol
  | Goto_eol
  | Goto_bot
  | Goto_eot
  | Delete_next_char
  | Delete_prev_char
  | Delete_next_line
  | Delete_prev_line
  | Kill_next_line
  | Kill_prev_line
  | Switch_erase_mode
  | Set_mark
  | Goto_mark
  | Copy
  | Kill
  | Yank

let get_action = function
  | Next_char -> next_char
  | Prev_char -> prev_char
  | Next_line -> next_line
  | Prev_line -> prev_line
  | Goto_bol -> goto_bol
  | Goto_eol -> goto_eol
  | Goto_bot -> goto_bot
  | Goto_eot -> goto_eot
  | Delete_next_char -> delete_next_char
  | Delete_prev_char -> delete_prev_char
  | Delete_next_line -> delete_next_line
  | Delete_prev_line -> delete_prev_line
  | Kill_next_line -> kill_next_line
  | Kill_prev_line -> kill_prev_line
  | Switch_erase_mode -> switch_erase_mode
  | Set_mark -> set_mark
  | Goto_mark -> goto_mark
  | Copy -> copy
  | Kill -> kill
  | Yank -> yank

let doc_of_action = function
  | Next_char -> "move the cursor to the next character."
  | Prev_char -> "move the cursor to the previous character."
  | Next_line -> "move the cursor to the next line."
  | Prev_line -> "move the cursor to the previous line."
  | Goto_bol -> "move the cursor to the beginning of the current line."
  | Goto_eol -> "move the cursor to the end of the current line."
  | Goto_bot -> "move the cursor to the beginning of the text."
  | Goto_eot -> "move the cursor to the end of the text."
  | Delete_next_char -> "delete the character after the cursor."
  | Delete_prev_char -> "delete the character before the cursor."
  | Delete_next_line -> "delete everything until the end of the current line."
  | Delete_prev_line -> "delete everything until the beginning of the current line."
  | Kill_next_line -> "cut everything until the end of the current line."
  | Kill_prev_line -> "cut everything until the beginning of the current line."
  | Switch_erase_mode -> "switch the current erasing mode."
  | Set_mark -> "set the mark to the current position."
  | Goto_mark -> "move the cursor to the mark."
  | Copy -> "copy the current region to the clipboard."
  | Kill -> "cut the current region to the clipboard."
  | Yank -> "paste the contents of the clipboard at current position."

let actions = [
  Next_char, "next-char";
  Prev_char, "prev-char";
  Next_line, "next-line";
  Prev_line, "prev-line";
  Goto_bol, "goto-bol";
  Goto_eol, "goto-eol";
  Goto_bot, "goto-bot";
  Goto_eot, "goto-eot";
  Delete_next_char, "delete-next-char";
  Delete_prev_char, "delete-prev-char";
  Delete_next_line, "delete-next-line";
  Delete_prev_line, "delete-prev-line";
  Kill_next_line, "kill-next-line";
  Kill_prev_line, "kill-prev-line";
  Switch_erase_mode, "switch-erase-mode";
  Set_mark, "set-mark";
  Goto_mark, "goto-mark";
  Copy, "copy";
  Kill, "kill";
  Yank, "yank";
]

let actions_to_names = Array.of_list (List.sort (fun (a1, n1) (a2, n2) -> compare a1 a2) actions)
let names_to_actions = Array.of_list (List.sort (fun (a1, n1) (a2, n2) -> compare n1 n2) actions)

let action_of_name x =
  let rec loop a b =
    if a = b then
      raise Not_found
    else
      let c = (a + b) / 2 in
      let action, name = Array.unsafe_get names_to_actions c in
      match compare x name with
        | d when d < 0 ->
            loop a c
        | d when d > 0 ->
            loop (c + 1) b
        | _ ->
            action
  in
  loop 0 (Array.length names_to_actions)

let name_of_action x =
  let rec loop a b =
    if a = b then
      raise Not_found
    else
      let c = (a + b) / 2 in
      let action, name = Array.unsafe_get actions_to_names c in
      match compare x action with
        | d when d < 0 ->
            loop a c
        | d when d > 0 ->
            loop (c + 1) b
        | _ ->
            name
  in
  loop 0 (Array.length actions_to_names)
