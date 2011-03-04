(*
 * zed_rope.mli
 * ------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Zed, an editor engine.
 *)

(** Unicode ropes *)

open CamomileLibrary

type t
  (** Type of unicode ropes. *)

type rope = t
    (** Alias. *)

exception Out_of_bounds
  (** Exception raised when trying to access a character which is
      outside the bounds of a rope. *)

(** {6 Construction} *)

val empty : t
  (** The empty rope. *)

val make : int -> UChar.t -> t
  (** [make length char] creates a rope of length [length] containing
      only [char]. *)

val singleton : UChar.t -> t
  (** [singleton ch] creates a rope of length 1 containing only
      [ch]. *)

(** {6 Informations} *)

val length : t -> int
  (** Returns the length of the given rope. *)

val is_empty : t -> bool
  (** [is_empty rope] returns whether [str] is the empty rope or
      not. *)

val compare : t -> t -> int
  (** Compares two ropes (in code point order). *)

val equal : t -> t -> bool
  (** [equal r1 r2] retuns [true] iff [r1] is equal to [r2]. *)

(** {6 To/from strings} *)

val of_string : string -> t
  (** [of_string str] creates a rope from a string. The string must be
      UTF-8 encoded and is validated. Note that [str] must not be
      modified after this operation, if you intend to do so you must
      copy it before passing it to [of_string]. *)

val to_string : t -> string
  (** [to_string rope] flatten a rope into a string encoded in
      UTF-8. *)

(** {6 Random access} *)

val get : t -> int -> UChar.t
  (** [get str rope] returns the character at index [idx] in
      [rope]. *)

(** {6 Rope manipulation} *)

val append : t -> t -> t
  (** Concatenates the two given ropes. *)

val concat : t -> t list -> t
  (** [concat sep l] concatenates all strings of [l] separating them
      by [sep]. *)

val sub : t -> int -> int -> t
  (** [sub rope ofs len] Returns the sub-rope of [rope] starting at
      [ofs] and of length [len]. *)

val break : t -> int -> t * t
  (** [break rope pos] returns the sub-ropes before and after [pos] in
      [rope]. It is more efficient than creating two sub-ropes with
      {!sub}. *)

val before : t -> int -> t
  (** [before rope pos] returns the sub-rope before [pos] in [rope]. *)

val after : t -> int -> t
  (** [after rope pos] returns the sub-string after [pos] in [rope]. *)

val insert : t -> int -> t -> t
  (** [insert rope pos sub] inserts [sub] in [rope] at position
      [pos]. *)

val remove : t -> int -> int -> t
  (** [remove rope pos len] removes the [len] characters at position
      [pos] in [rope] *)

val replace : t -> int -> int -> t -> t
  (** [replace rope pos len repl] replaces the [len] characters at
      position [pos] in [rope] by [repl]. *)

val lchop : t -> t
  (** [lchop rope] returns [rope] without is first character. Returns
      {!empty} if [rope] is empty. *)

val rchop : t -> t
  (** [rchop rope] returns [rope] without is last character. Returns
      {!empty} if [rope] is empty. *)

(** {6 Iteration, folding and mapping} *)

val iter : (UChar.t -> unit) -> t-> unit
  (** [iter f rope] applies [f] on all characters of [rope] starting
      from the left. *)

val rev_iter : (UChar.t -> unit) -> t -> unit
  (** [rev_iter f rope] applies [f] an all characters of [rope]
      starting from the right. *)

val fold : (UChar.t -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f rope acc] applies [f] on all characters of [rope]
      starting from the left, accumulating a value. *)

val rev_fold : (UChar.t -> 'a -> 'a) -> t -> 'a -> 'a
  (** [rev_fold f rope acc] applies [f] on all characters of [rope]
      starting from the right, accumulating a value. *)

val map : (UChar.t -> UChar.t) -> t -> t
  (** [map f rope] maps all characters of [rope] with [f]. *)

val rev_map : (UChar.t -> UChar.t) -> t -> t
  (** [rev_map f str] maps all characters of [rope] with [f] in
      reverse order. *)

(** {6 Zippers} *)

module Zip : sig
  type t
    (** Type of zippers. A zipper allow to naviguate in a rope in a
        convenient and efficient manner. Note that a zipper points to
        a position between two characters, not to a character, so in a
        rope of length [len] there is [len + 1] positions. *)

  val make_f : rope -> int -> t
    (** [make_f rope pos] creates a new zipper pointing to positon
        [pos] of [rope]. *)

  val make_b : rope -> int -> t
    (** [make_f rope pos] creates a new zipper pointing to positon
        [length rope - pos] of [rope]. *)

  val offset : t -> int
    (** Returns the position of the zipper in the rope. *)

  val next : t -> UChar.t * t
    (** [next zipper] returns the code point at the right of the
        zipper and a zipper to the next position. It raises
        [Out_of_bounds] if the zipper points to the end of the
        rope. *)

  val prev : t -> UChar.t * t
    (** [prev zipper] returns the code point at the left of the
        zipper and a zipper to the previous position. It raises
        [Out_of_bounds] if the zipper points to the beginning of the
        rope. *)

  val move : int -> t -> t
    (** [move n zip] moves the zipper by [n] characters. If [n] is
        negative it is moved to the left and if it is positive it is
        moved to the right. It raises [Out_of_bounds] if the result
        is outside the bounds of the rope. *)

  val at_bos : t -> bool
    (** [at_bos zipper] returns [true] iff [zipper] points to the
        beginning of the rope. *)

  val at_eos : t -> bool
    (** [at_eos zipper] returns [true] iff [zipper] points to the
        end of the rope. *)

  val find_f : (UChar.t -> bool) -> t -> t
    (** [find_f f zip] search forward for a character to satisfy
        [f]. It returns a zipper pointing to the left of the first
        character to satisfy [f], or a zipper pointing to the end of
        the rope if no such character exists. *)

  val find_b : (UChar.t -> bool) -> t -> t
    (** [find_b f zip] search backward for a character to satisfy
        [f]. It returns a zipper pointing to the right of the first
        character to satisfy [f], or a zipper pointing to the
        beginning of the rope if no such character exists. *)

  val sub : t -> int -> rope
    (** [sub zipper len] returns the sub-rope of length [len] pointed
        by [zipper]. *)

  val slice : t -> t -> rope
    (** [slice zipper1 zipper2] returns the rope between [zipper1]
        and [zipper2]. If [zipper1 > zipper2] then this is the same as
        [slice zipper2 zipper1].

        The result is unspecified if the two zippers do not points to
        the same rope. *)
end

(** {6 Buffers} *)

module Buffer : sig

  (** This module is similar of the [Buffer] module of the standard
      library except that it works with rope. *)

  type t
    (** Type of rope buffers. *)

  val create : unit -> t
    (** Create a new empty buffer. *)

  val add : t -> UChar.t -> unit
    (** [add buffer x] add [x] at the end of [buffer]. *)

  val contents : t -> rope
    (** [contents buffer] returns the contents of [buffer] as a
        rope. *)

  val reset : t -> unit
    (** [reset buffer] resets [buffer] to its initial state. *)
end
