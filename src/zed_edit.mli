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

val create : ?editable : (int -> bool) -> ?move : (int -> int -> int) -> unit -> t
  (** [create ?editable ?move ()] creates a new edition engine in the
      initial state. [editable] is used to determine whether the text
      at given position is editable or not, and [move] is used to
      alter deplacement of the cursor. It receive the current position
      of the cursor, a distance to move (which can be negative) and
      must returns the new position of the cursor. It defaults to [fun
      pos delta -> pos + delta]. *)

(** {6 State} *)

val text : t -> Zed_rope.t
  (** [text edit] returns the signal holding the current contents of
      the buffer. *)

val changes : t -> (int * int * int) event
  (** [changes edit] returns an event which occurs with values of the
      form [(start, added, removed)] when the contents of the engine
      changes. [start] is the start of modifications, [added] is the
      number of characters added and [removed] is the number of
      characters removed. *)

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

val insert : context -> Zed_rope.t -> unit
  (** [insert ctx rope] inserts the given rope at current position. *)

val next_char : context -> unit
  (** [next_char ctx] moves the cursor to the next character. *)

val prev_char : context -> unit
  (** [prev_char ctx] moves the cursor to the previous character. *)

(** {6 Action by names} *)

(** Type of action requiring no parameters. *)
type action =
  | Next_char
  | Prev_char

val exec : context -> action -> unit
  (** [exec ctx action] executes the given action. *)

val action_of_name : string -> action
  (** [action_of_name str] converts the given action name into an
      action. Action name are the same as function name but with '_'
      replaced by '-'. It raises [Not_found] if the name does not
      correspond to an action. *)

val name_of_action : action -> string
  (** [name_of_action act] returns the name of the given action. *)
