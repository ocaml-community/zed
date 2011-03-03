(*
 * zed_edit.mli
 * ------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Zed, an editor engine.
 *)

(** Edition engines *)

open React

type t
  (** Type of edition engines. *)

(** Type of clipboards. *)
type clipboard = {
  clipboard_get : unit -> Zed_rope.t;
  (** Returns the current contents of the clipboard. *)
  clipboard_set : Zed_rope.t -> unit;
  (** Sets the contents of the clipboard. *)
}

val create : ?editable : (int -> bool) -> ?move : (int -> int -> int) -> ?clipboard : clipboard -> unit -> t
  (** [create ?editable ?move ?clipboard ()] creates a new edition
      engine in the initial state.

      [editable] is used to determine whether the text at given
      position is editable or not.

      [move] is used to alter deplacement of the cursor. It receive
      the current position of the cursor, a distance to move (which
      can be negative) and must returns the new position of the
      cursor. It defaults to [fun pos delta -> pos + delta].

      [clipboard] is the clipboard to use for this engine. If none is
      defined, a new one using a reference is created. *)

(** {6 State} *)

val text : t -> Zed_rope.t
  (** [text edit] returns the signal holding the current contents of
      the buffer. *)

val lines : t -> Zed_lines.t
  (** [lines edit] returns the set of line position of [text edit]. *)

val changes : t -> (int * int * int) event
  (** [changes edit] returns an event which occurs with values of the
      form [(start, added, removed)] when the contents of the engine
      changes. [start] is the start of modifications, [added] is the
      number of characters added and [removed] is the number of
      characters removed. *)

val erase_mode : t -> bool signal
  (** [erase_mode edit] returns the ``erase'' mode of the buffer. In
      this mode character inserted in the buffer erase existing
      ones. *)

val get_erase_mode : t -> bool
  (** [erase_mode edit] returns the current erase mode of the
      buffer. *)

val set_erase_mode : t -> bool -> unit
  (** [set_erase_mode edit state] sets the status of the erase mode
      for the given engine. *)

val mark : t -> Zed_cursor.t
  (** [mark edit] returns the cursor used to for the mark in the given
      engine. *)

val selection : t -> bool signal
  (** [selection edit] returns the signal holding the current
      selection state. If [true], text is being selectionned. *)

val get_selection : t -> bool
  (** [selection edit] returns the current selection state. *)

val set_selection : t -> bool -> unit
  (** [set_selection edit state] sets the selection state. *)

(** {6 Cursors} *)

val new_cursor : t -> Zed_cursor.t
  (** [new_cursor edit] creates a new cursor for the given edition
      engine. The cursor initially points to the beginning of the
      buffer. *)

(** {6 Actions} *)

type context
  (** Type of contexts. Contexts are used to modify an edition
      buffer. *)

val create_context : ?check : bool -> t -> Zed_cursor.t -> context
  (** [create_context ?check edit cursor] creates a new context with
      given parameters. [cursor] is the cursor that will be used for
      all modification of the text. If [check] is [true] (the default)
      then all modification of the text will be checked with the
      [editable] function of the engine and all movement will be
      passed through the [move] function of the engine. *)

val goto : context -> int -> unit
  (** [goto ctx position] moves the cursor to the given position. It
      raises {!Zed_cursor.Out_of_bounds} if the position is outside the
      bounds of the text. *)

val move : context -> int -> unit
  (** [move ctx delta] moves the cursor by the given number of
      characters. It raises {!Zed_cursor.Out_of_bounds} if the current
      plus [delta] is outside the bounds of the text. *)

val position : context -> int
  (** [position ctx] returns the position of the cursor. *)

val at_bol : context -> bool
  (** [at_bol ctx] returns [true] iff the cursor is at the beginning
      of the current line. *)

val at_eol : context -> bool
  (** [at_eol ctx] returns [true] iff the cursor is at the end of the
      current line. *)

val at_bot : context -> bool
  (** [at_bot ctx] returns [true] iff the cursor is at the beginning
      of the text. *)

val at_eot : context -> bool
  (** [at_eot ctx] returns [true] iff the cursor is at the end of the
      text. *)

val insert : context -> Zed_rope.t -> unit
  (** [insert ctx rope] inserts the given rope at current position. *)

val remove : context -> int -> unit
  (** [remove ctx n] removes [n] characters at current position. If
      there is less than [n] characters a current position, it removes
      everything until the end of the text. *)

val next_char : context -> unit
  (** [next_char ctx] moves the cursor to the next character. It does
      nothing if the cursor is at the end of the text. *)

val prev_char : context -> unit
  (** [prev_char ctx] moves the cursor to the previous character. It
      does nothing if the cursor is at the beginning of the text. *)

val next_line : context -> unit
  (** [next_line ctx] moves the cursor to the next line. If the cursor
      is on the last line, it is moved to the end of the buffer. *)

val prev_line : context -> unit
  (** [prev_line ctx] moves the cursor to the previous line. If the
      cursor is on the first line, it is moved to the beginning of the
      buffer. *)

val goto_bol : context -> unit
  (** [goto_bol ctx] moves the cursor to the beginning of the current
      line. *)

val goto_eol : context -> unit
  (** [goto_eol ctx] moves the cursor to the end of the current
      line. *)

val goto_bot : context -> unit
  (** [goto_bot ctx] moves the cursor to the beginning of the text. *)

val goto_eot : context -> unit
  (** [goto_eot ctx] moves the cursor to the end of the text. *)

val delete_next_char : context -> unit
  (** [delete_next_char ctx] deletes the character after the cursor,
      if any. *)

val delete_prev_char : context -> unit
  (** [delete_prev_char ctx] delete the character before the
      cursor. *)

val delete_next_line : context -> unit
  (** [delete_next_line ctx] delete everything until the end of the
      current line. *)

val delete_prev_line : context -> unit
  (** [delete_next_line ctx] delete everything until the beginning of
      the current line. *)

val kill_next_line : context -> unit
  (** [kill_next_line ctx] delete everything until the end of the
      current line and save it to the clipboard. *)

val kill_prev_line : context -> unit
  (** [kill_next_line ctx] delete everything until the beginning of
      the current line and save it to the clipboard. *)

val switch_erase_mode : context -> unit
  (** [switch_erase_mode ctx] switch the current erase mode. *)

val set_mark : context -> unit
  (** [set_mark ctx] sets the mark at current position. *)

val goto_mark : context -> unit
  (** [goto_mark ctx] moves the cursor to the mark. *)

val copy : context -> unit
  (** [copy ctx] copies the current selectionned region to the
      clipboard. *)

val kill : context -> unit
  (** [kill ctx] copies the current selectionned region to the
      clipboard and remove it. *)

val yank : context -> unit
  (** [yank ctx] inserts the contents of the clipboard at current
      position. *)

(** {6 Action by names} *)

(** Type of action requiring no parameters. *)
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

val get_action : action -> (context -> unit)
  (** [get_action action] returns the function associated to the given
      action. *)

val actions : (action * string) list
  (** List of actions with their names. *)

val doc_of_action : action -> string
  (** [doc_of_action action] returns a short description of the
      action. *)

val action_of_name : string -> action
  (** [action_of_name str] converts the given action name into an
      action. Action name are the same as function name but with '_'
      replaced by '-'. It raises [Not_found] if the name does not
      correspond to an action. *)

val name_of_action : action -> string
  (** [name_of_action act] returns the name of the given action. *)
