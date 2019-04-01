(*
 * zed_char.mli
 * ------------
 * Copyright : (c) 2019, ZAN DoYe <zandoye@gmail.com>
 * Licence   : BSD3
 *
 * This file is a part of Zed, an editor engine.
 *)

open CamomileLibrary
open Result

(** The type for glyphs. *)
type t = {
  core : UChar.t; (** the core character *)
  combined : UChar.t list; (** combining marks *)
  width : int;
  size : int; (** the number of characters(UChar.t) *)
}

type pos = Core | Combined of int
  (** A [pos]  describes a position in a [Zed_char.t] *)

val to_raw : t -> UChar.t list
  (** [to_raw t] returns a list of [UChar.t] which consists t *)

val to_array : t -> UChar.t array
  (** [to_array t] returns an array of [UChar.t] which consists t *)

type char_prop = Printable of int | Other | Null
  (** The type of a character. It's [Printable of width] or [Other](unprintable character) or [Null](code 0). *)

val zero : t
  (** The Character 0. *)

val prop_uChar : UChar.t -> char_prop
  (** [prop_uChar uChar] returns the char_prop of [uChar] *)

val prop : t -> char_prop
  (** [prop ch] returns the char_prop of [ch] *)

val size : t -> int
  (** [size ch] returns the size (number of characters) of [ch]. *)

val length : t -> int
  (** Aliase of size *)

val width : t -> int
  (** [width ch] returns the width of [ch]. *)

val calc_size : t -> int
  (** [calc_size ch] calculates and returns the width of [ch]. *)

val out_of_range : t -> int -> bool
  (** [out_of_range ch idx] returns whether [idx] is out of range of [ch]. *)

val get : t -> int -> UChar.t
  (** [get ch n] returns the [n]-th character of [ch]. *)

val get_opt : t -> int -> UChar.t option
  (** [get ch n] returns an optional value of the [n]-th character of [ch]. *)

val append : t -> UChar.t -> t
  (** [append ch cm] append the combining mark [cm] to ch and returns it. If [cm] is not a combining mark, then the original [ch] is returned. *)

val compare_core : t -> t -> int
  (** [compare_core ch1 ch2] compares the core components of ch1 and ch2*)

val compare_raw : t -> t -> int
  (** [compare_raw ch1 ch2] compares over the internal characters of ch1 and ch2 sequentially *)

val compare : t -> t -> int
  (** Alias of compare_raw *)

val mix_uChar : t -> UChar.t -> (t, t) result
  (** [mix_uChar ch uChar] tries to append [uChar] ch and returns [Ok result]. If [uChar] is not a combining mark, then an [Error (Zed_char.t consists of uChar) is returned. *)

val of_uChars : UChar.t list -> t option * UChar.t list
  (** [of_uChars uChars] transforms [uChars] to a tuple. The first value is an optional [Zed_char.t] and the second one is a list of remaining uChars. This function creates a Zed_char.t if the first uChar in uChars is a printable standalone glyph, and then, it appends all following combining marks to the Zed_char.t. After that, all remaining uChars returned as the second value in the tuple. Otherwise, if the first uChar is not printable or standalone glyph, the result will be (None, all uChars). *)

val zChars_of_uChars : UChar.t list -> t list * UChar.t list
  (** [zChars of_uChars uChars] transforms [uChars] to a tuple. The first value is a list of [Zed_char.t] and the second one is a list of remaining uChars. *)

val unsafe_of_char : char -> t
  (** [unsafe_of_char ch] returns a Zed_char.t whose core is [ch]  *)

val unsafe_of_uChar : UChar.t -> t
  (** [unsafe_of_uChar ch] returns a Zed_char.t whose core is [ch]  *)

val for_all : (UChar.t -> bool) -> t -> bool
  (** [for_all p zChar] checks if all elements of [zChar]
   satisfy the predicate [p]. *)

(** Converting between [UnicodeString.Type] and [Zed_char.t] *)
module US :
  functor (US : UnicodeString.Type) ->
    sig
      module Convert :
        sig
          val of_list : UChar.t list -> US.t
          val of_array : UChar.t array -> US.t
          val to_uChars : US.t -> UChar.t list
        end
      val of_t : t -> US.t
      val to_t : US.t -> t option * UChar.t list
      val to_t_exn : US.t -> t
    end

