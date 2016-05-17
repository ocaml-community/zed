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
module Clipboard : sig
  type t =
    { get : unit -> Zed_rope.t
    ; set : Zed_rope.t -> unit
    }

  val create : unit -> t
  (** [local ()] creates a new clipboard using a reference. *)
end

val create :
  ?editable  : (int -> int -> bool) ->
  ?clipboard : Clipboard.t ->
  ?undo_size : int ->
  unit -> t
(** [create ?editable ?move ?clipboard ()] creates a new edition
    engine in the initial state.

    [editable] is used to determine whether the text at given
    position is editable or not. It takes as argument the position
    and the length of the text to remove.

    [clipboard] is the clipboard to use for this engine. If none is
    defined, a new one using a reference is created.

    [undo_size] is the size of the undo buffer. It is the number of
    state zed will remember. It defaults to [1000]. *)

(** {6 State} *)

val text : t -> Zed_rope.t
(** [text edit] returns the signal holding the current contents of
    the buffer. *)

val lines : t -> Zed_lines.t
(** [lines edit] returns the set of line position of [text edit]. *)

val get_line : t -> int -> Zed_rope.t
(** [get_line edit n] returns the rope corresponding to the [n]th line
    without the newline character. *)

val changes : t -> (int * int * int) event
(** [changes edit] returns an event which occurs with values of the
    form [(start, added, removed)] when the contents of the engine
    changes. [start] is the start of modifications, [added] is the
    number of characters added and [removed] is the number of
    characters removed. *)

val update : t -> Zed_cursor.t list -> unit event
(** [update edit cursors] returns an event which occurs each the
    rendering of the engine should be updated. *)

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

exception Cannot_edit
(** Exception raised when trying to edit a non-editable portion of a
    buffer. *)

type context
(** Type of contexts. Contexts are used to modify an edition
    buffer. *)

val context : ?check : bool -> t -> Zed_cursor.t -> context
(** [context ?check edit cursor] creates a new context with given
    parameters. [cursor] is the cursor that will be used for all
    modification of the text. If [check] is [true] (the default)
    then all modification of the text will be checked with the
    [editable] function of the engine. *)

val edit : context -> t
(** [edit ctx] returns the edition engine used by the given
    context. *)

val cursor : context -> Zed_cursor.t
(** [cursor ctx] returns the cursor used by this context. *)

val check : context -> bool
(** [check ctx] returns whether the context has been created with
    the [check] flag. *)

val with_check : bool -> context -> context
(** [with_check check ctx] retuns [ctx] with the check flag set to
    [check]. *)

val goto : context -> ?set_wanted_column : bool -> int -> unit
(** [goto ctx ?set_column position] moves the cursor to the given
    position. It raises {!Zed_cursor.Out_of_bounds} if the position
    is outside the bounds of the text. If [set_wanted_column] is
    [true], the wanted column of the cursor is set to the new
    column. *)

val move : context -> ?set_wanted_column : bool -> int -> unit
(** [move ctx ?set_wanted_column delta] moves the cursor by the
    given number of characters. It raises
    {!Zed_cursor.Out_of_bounds} if the current plus [delta] is
    outside the bounds of the text. *)

val move_line : context -> int -> unit
(** [move_line ctx ?set_wanted_column delta] moves the cursor by the
    given number of lines. *)

val position : context -> int
(** [position ctx] returns the position of the cursor. *)

val line : context -> int
(** [line ctx] returns the line of the cursor. *)

val column : context -> int
(** [column ctx] returns the column of the cursor. *)

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

val insert_no_erase : context -> Zed_rope.t -> unit
(** [insert ctx rope] inserts the given rope at current position but
    do not erase text if the buffer is currently in erase mode. *)

val remove_next : context -> int -> unit
(** [remove_next ctx n] removes [n] characters at current
    position. If there is less than [n] characters at current
    position, it removes everything until the end of the text. *)

val remove_prev : context -> int -> unit
(** [remove_prev ctx n] removes [n] characters before current
    position. If there is less than [n] characters before current
    position, it removes everything until the beginning of the
    text. *)

val remove : context -> int -> unit
(** Alias for {!remove_next} *)

val replace : context -> int -> Zed_rope.t -> unit
(** [replace ctx n rope] does the same as:

    {[
      remove ctx n;
      insert_no_erase ctx rope
    ]}

    but in one atomic operation. *)

val newline : context -> unit
(** Insert a newline character. *)

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

val capitalize_word : context -> unit
(** [capitalize_word ctx] capitalizes the first word after the
    cursor. *)

val lowercase_word : context -> unit
(** [lowercase_word ctx] converts the first word after the cursor to
    lowercase. *)

val uppercase_word : context -> unit
(** [uppercase_word ctx] converts the first word after the cursor to
    uppercase. *)

val next_word : context -> unit
(** [next_word ctx] moves the cursor to the end of the next word. *)

val prev_word : context -> unit
(** [prev_word ctx] moves the cursor to the beginning of the
    previous word. *)

val delete_next_word : context -> unit
(** [delete_next_word ctx] deletes the word after the cursor. *)

val delete_prev_word : context -> unit
(** [delete_prev_word ctx] deletes the word before the cursor. *)

val kill_next_word : context -> unit
(** [kill_next_word ctx] deletes the word after the cursor and save
    it to the clipboard. *)

val kill_prev_word : context -> unit
(** [kill_prev_word ctx] deletes the word before the cursor and save
    it to the clipboard. *)

val undo : context -> unit
(** [undo ctx] reverts the last performed action. *)

(** {6 Actions} *)

val actions : context Zed_actions.t
(** All the actions mentionned in the previous section *)

val parse_insert : Zed_utf8.t -> Zed_rope.t option
