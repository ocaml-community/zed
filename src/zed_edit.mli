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

type 'a t
  (** Type of edition engines. ['a] is the type of custom data
      attached to the engine in order to extend it. *)

(** Type of clipboards. *)
type clipboard = {
  clipboard_get : unit -> Zed_rope.t;
  (** Returns the current contents of the clipboard. *)
  clipboard_set : Zed_rope.t -> unit;
  (** Sets the contents of the clipboard. *)
}

val create :
  ?editable : (int -> int -> bool) ->
  ?move : (int -> int -> int) ->
  ?clipboard : clipboard ->
  ?match_word : (Zed_rope.t -> int -> int option) ->
  ?locale : string option signal ->
  unit -> 'a t
  (** [create ?editable ?move ?clipboard ()] creates a new edition
      engine in the initial state.

      [editable] is used to determine whether the text at given
      position is editable or not. It takes as argument the position
      and the length of the text to remove.

      [move] is used to alter deplacement of the cursor. It receive
      the current position of the cursor, a distance to move (which
      can be negative) and must returns the new position of the
      cursor. It defaults to [fun pos delta -> pos + delta].

      [clipboard] is the clipboard to use for this engine. If none is
      defined, a new one using a reference is created.

      [match_word] is used to recognize words. It must returns the end
      of the matched word if any.

      [locale] is the locale of this buffer. It is used for case
      mapping. *)

val match_by_regexp : Zed_re.t -> Zed_rope.t -> int -> int option
  (** [match_by_regexp re] creates a word-matching function using a
      regular expression. *)

(** {6 State} *)

val get_data : 'a t -> 'a
  (** [get_data edit] returns the custom data attached to the
      engine. It raises [Not_found] if no data is attached to the
      engine. *)

val set_data : 'a t -> 'a -> unit
  (** [set_data edit data] attach [data] to the engine. *)

val clear_data : 'a t -> unit
  (** [clear_data edit] removes the custom data of engine. *)

val text : 'a t -> Zed_rope.t
  (** [text edit] returns the signal holding the current contents of
      the buffer. *)

val lines : 'a t -> Zed_lines.t
  (** [lines edit] returns the set of line position of [text edit]. *)

val changes : 'a t -> (int * int * int) event
  (** [changes edit] returns an event which occurs with values of the
      form [(start, added, removed)] when the contents of the engine
      changes. [start] is the start of modifications, [added] is the
      number of characters added and [removed] is the number of
      characters removed. *)

val update : 'a t -> Zed_cursor.t list -> unit event
  (** [update edit cursors] returns an event which occurs each the
      rendering of the engine should be updated. *)

val erase_mode : 'a t -> bool signal
  (** [erase_mode edit] returns the ``erase'' mode of the buffer. In
      this mode character inserted in the buffer erase existing
      ones. *)

val get_erase_mode : 'a t -> bool
  (** [erase_mode edit] returns the current erase mode of the
      buffer. *)

val set_erase_mode : 'a t -> bool -> unit
  (** [set_erase_mode edit state] sets the status of the erase mode
      for the given engine. *)

val mark : 'a t -> Zed_cursor.t
  (** [mark edit] returns the cursor used to for the mark in the given
      engine. *)

val selection : 'a t -> bool signal
  (** [selection edit] returns the signal holding the current
      selection state. If [true], text is being selectionned. *)

val get_selection : 'a t -> bool
  (** [selection edit] returns the current selection state. *)

val set_selection : 'a t -> bool -> unit
  (** [set_selection edit state] sets the selection state. *)

(** {6 Cursors} *)

val new_cursor : 'a t -> Zed_cursor.t
  (** [new_cursor edit] creates a new cursor for the given edition
      engine. The cursor initially points to the beginning of the
      buffer. *)

(** {6 Actions} *)

type 'a context
  (** Type of contexts. Contexts are used to modify an edition
      buffer. *)

val context : ?check : bool -> 'a t -> Zed_cursor.t -> 'a context
  (** [context ?check edit cursor] creates a new context with given
      parameters. [cursor] is the cursor that will be used for all
      modification of the text. If [check] is [true] (the default)
      then all modification of the text will be checked with the
      [editable] function of the engine and all movement will be
      passed through the [move] function of the engine. *)

val edit : 'a context -> 'a t
  (** [edit ctx] returns the edition engine used by the given
      context. *)

val cursor : 'a context -> Zed_cursor.t
  (** [cursor ctx] returns the cursor used by this context. *)

val check : 'a context -> bool
  (** [check ctx] returns whether the context has been created with
      the [check] flag. *)

val goto : 'a context -> ?set_wanted_column : bool -> int -> unit
  (** [goto ctx ?set_column position] moves the cursor to the given
      position. It raises {!Zed_cursor.Out_of_bounds} if the position
      is outside the bounds of the text. If [set_wanted_column] is
      [true], the wanted column of the cursor is set to the new
      column. *)

val move : 'a context -> ?set_wanted_column : bool -> int -> unit
  (** [move ctx ?set_wanted_column delta] moves the cursor by the
      given number of characters. It raises
      {!Zed_cursor.Out_of_bounds} if the current plus [delta] is
      outside the bounds of the text. *)

val move_line : 'a context -> int -> unit
  (** [move_line ctx ?set_wanted_column delta] moves the cursor by the
      given number of lines. *)

val position : 'a context -> int
  (** [position ctx] returns the position of the cursor. *)

val line : 'a context -> int
  (** [line ctx] returns the line of the cursor. *)

val column : 'a context -> int
  (** [column ctx] returns the column of the cursor. *)

val at_bol : 'a context -> bool
  (** [at_bol ctx] returns [true] iff the cursor is at the beginning
      of the current line. *)

val at_eol : 'a context -> bool
  (** [at_eol ctx] returns [true] iff the cursor is at the end of the
      current line. *)

val at_bot : 'a context -> bool
  (** [at_bot ctx] returns [true] iff the cursor is at the beginning
      of the text. *)

val at_eot : 'a context -> bool
  (** [at_eot ctx] returns [true] iff the cursor is at the end of the
      text. *)

val insert : 'a context -> Zed_rope.t -> unit
  (** [insert ctx rope] inserts the given rope at current position. *)

val remove : 'a context -> int -> unit
  (** [remove ctx n] removes [n] characters at current position. If
      there is less than [n] characters a current position, it removes
      everything until the end of the text. *)

val newline : 'a context -> unit
  (** Insert a newline character. *)

val next_char : 'a context -> unit
  (** [next_char ctx] moves the cursor to the next character. It does
      nothing if the cursor is at the end of the text. *)

val prev_char : 'a context -> unit
  (** [prev_char ctx] moves the cursor to the previous character. It
      does nothing if the cursor is at the beginning of the text. *)

val next_line : 'a context -> unit
  (** [next_line ctx] moves the cursor to the next line. If the cursor
      is on the last line, it is moved to the end of the buffer. *)

val prev_line : 'a context -> unit
  (** [prev_line ctx] moves the cursor to the previous line. If the
      cursor is on the first line, it is moved to the beginning of the
      buffer. *)

val goto_bol : 'a context -> unit
  (** [goto_bol ctx] moves the cursor to the beginning of the current
      line. *)

val goto_eol : 'a context -> unit
  (** [goto_eol ctx] moves the cursor to the end of the current
      line. *)

val goto_bot : 'a context -> unit
  (** [goto_bot ctx] moves the cursor to the beginning of the text. *)

val goto_eot : 'a context -> unit
  (** [goto_eot ctx] moves the cursor to the end of the text. *)

val delete_next_char : 'a context -> unit
  (** [delete_next_char ctx] deletes the character after the cursor,
      if any. *)

val delete_prev_char : 'a context -> unit
  (** [delete_prev_char ctx] delete the character before the
      cursor. *)

val delete_next_line : 'a context -> unit
  (** [delete_next_line ctx] delete everything until the end of the
      current line. *)

val delete_prev_line : 'a context -> unit
  (** [delete_next_line ctx] delete everything until the beginning of
      the current line. *)

val kill_next_line : 'a context -> unit
  (** [kill_next_line ctx] delete everything until the end of the
      current line and save it to the clipboard. *)

val kill_prev_line : 'a context -> unit
  (** [kill_next_line ctx] delete everything until the beginning of
      the current line and save it to the clipboard. *)

val switch_erase_mode : 'a context -> unit
  (** [switch_erase_mode ctx] switch the current erase mode. *)

val set_mark : 'a context -> unit
  (** [set_mark ctx] sets the mark at current position. *)

val goto_mark : 'a context -> unit
  (** [goto_mark ctx] moves the cursor to the mark. *)

val copy : 'a context -> unit
  (** [copy ctx] copies the current selectionned region to the
      clipboard. *)

val kill : 'a context -> unit
  (** [kill ctx] copies the current selectionned region to the
      clipboard and remove it. *)

val yank : 'a context -> unit
  (** [yank ctx] inserts the contents of the clipboard at current
      position. *)

val capitalize_word : 'a context -> unit
  (** [capitalize_word ctx] capitalizes the first word after the
      cursor. *)

val lowercase_word : 'a context -> unit
  (** [lowercase_word ctx] converts the first word after the cursor to
      lowercase. *)

val uppercase_word : 'a context -> unit
  (** [uppercase_word ctx] converts the first word after the cursor to
      uppercase. *)

val next_word : 'a context -> unit
  (** [next_word ctx] moves the cursor to the end of the next word. *)

val prev_word : 'a context -> unit
  (** [prev_word ctx] moves the cursor to the beginning of the
      previous word. *)

(** {6 Action by names} *)

(** Type of action requiring no parameters. *)
type action =
  | Newline
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
  | Capitalize_word
  | Lowercase_word
  | Uppercase_word
  | Next_word
  | Prev_word
  | Delete_next_word
  | Delete_prev_word

val get_action : action -> ('a context -> unit)
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
