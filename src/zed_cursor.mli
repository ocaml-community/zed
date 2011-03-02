(*
 * zed_cursor.mli
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Zed, an editor engine.
 *)

(** Cursors *)

(** A cursor is a pointer in an edition buffer. When some text is
    inserted or removed, all cursors after the modification are
    automatically moved accordingly. *)

open React

type t
  (** Type of a cursor. *)

exception Out_of_bounds
  (** Exception raised when trying to move a cursor outside the bounds
      of the text it points to. *)

val create : int -> (int * int) event -> int -> t
  (** [create length changes position] creates a new cursor pointing
      to position [position]. [length] is the current length of the
      text the cursor points to. It raises {!Out_of_bounds} if
      [position] is greater than [length]. [changes] is an event which
      occurs with values of the form [(start, delta)] when the text
      changes. [start] is the start of the modification and [delta] is
      the number of characters added/removed. *)

val copy : t -> t
  (** [copy cursor] creates a copy of the given cursor. The new cursor
      initially points to the same location as [cursor]. *)

val position : t -> int signal
  (** [position cursor] returns the signal holding the current
      position of the given cursor. *)

val move : t -> int -> unit
  (** [move cursor delta] moves the given cursor by the given number
      of characters. It raises {!Out_of_bounds} if the result is
      outside the bounds of the text. *)
