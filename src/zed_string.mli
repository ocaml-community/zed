(*
 * zed_string.mli
 * -----------
 * Copyright : (c) 2019, ZAN DoYe <zandoye@gmail.com>
 * Licence   : BSD3
 *
 * This file is a part of Zed, an editor engine.
 *)

open CamomileLibrary
open Result

exception Out_of_bounds
  (** Exception raised when trying to access a character which is
      outside the bounds of a string. *)

type seg_width = { start : int; len : int; width : int; }
  (** Type of the width of a segment of a Zed_string.t *)

type all_width = { len : int; width : int; }
  (** Type of the width of a whole Zed_string.t *)

type width = (all_width, seg_width) result
  (** Type of the width of a Zed_string.t *)

type t
  (** Type of Zed_string.t *)

val unsafe_of_utf8 : string -> t
val of_utf8 : string -> t
val to_utf8 : t -> string

val explode : t -> Zed_char.t list
val implode : Zed_char.t list -> t

val aval_width : width -> int
  (** Returns the widest available width *)

val init : int -> (int -> Zed_char.t) -> t
val init_from_uChars : int -> (int -> UChar.t) -> t
val make : int -> Zed_char.t -> t

val copy : t -> t
val to_raw_list : t -> UChar.t list
val to_raw_array : t -> UChar.t array
type index = int
val get : t -> int -> Zed_char.t
val get_raw : t -> int -> UChar.t
val empty : unit -> t
val width : ?start:int -> ?num:int -> t -> width

val size : t -> int
  (** [size str] returns the number of UChar.t in [str]. *)

val length : t -> int
  (** [length str returns the number of Zed_char.t in [str] *)

val extract : t -> index -> Zed_char.t
val extract_next : t -> index -> (Zed_char.t * index)
val extract_prev : t -> index -> (Zed_char.t * index)

val of_uChars :
  UChar.t list -> t * UChar.t list
val for_all : (Zed_char.t -> bool) -> t -> bool
  (** [for_all p zStr] checks if all Zed_char.t in [zStr]
   satisfy the predicate [p]. *)


val iter : (Zed_char.t -> unit) -> t -> unit
val rev_iter : (Zed_char.t -> unit) -> t -> unit
val fold : (Zed_char.t -> 'a -> 'a) -> t -> 'a -> 'a
val rev_fold : (Zed_char.t -> 'a -> 'a) -> t -> 'a -> 'a
val map : (Zed_char.t -> Zed_char.t) -> t -> t
val rev_map : (Zed_char.t -> Zed_char.t) -> t -> t

val check_range : t -> int -> bool
val look : t -> index -> UChar.t
val nth : t -> int -> index
val next : t -> index -> index
val prev : t -> index -> index
val out_of_range : t -> index -> bool
val compare : t -> t -> int
val first : t -> index
val last : t -> index
val move : t -> index -> int -> index
val compare_index : t -> index -> index -> int
val sub : t -> int -> int -> t
val after : t -> int -> t
val unsafe_sub_equal : t -> int -> t -> int -> bool
val starts_with : prefix:t -> t -> bool
val append : t -> t -> t
val ends_with : t -> t -> bool

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
      val to_t : US.t -> t * UChar.t list
      val to_t_exn : US.t -> t
    end

module Buf :
  sig
    type buf
    val create : int -> buf
    val contents : buf -> t
    val clear : buf -> unit
    val reset : buf -> unit
    val length : buf -> int
    val add_zChar : buf -> Zed_char.t -> unit
    val add_uChar : buf -> UChar.t -> unit
    val add_string : buf -> t -> unit
    val add_buffer : buf -> buf -> unit
  end

module US_Core : sig
  type t
  type index = int
  val length : t -> int
  val size : t -> int

  val look : t -> index -> UChar.t
  val nth : t -> int -> index
  val next : t -> index -> index
  val prev : t -> index -> index
  val out_of_range : t -> index -> bool
  val first : t -> index
  val last : t -> index
  val move : t -> index -> int -> index
  val compare_index : t -> index -> index -> int

  val get : t -> int -> UChar.t
  val init : int -> (int -> UChar.t) -> t
  val iter : (UChar.t -> unit) -> t -> unit
  val compare : t -> t -> int

  val to_list : t -> UChar.t list
  val to_array : t -> UChar.t array

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
      end

  module Buf :
    sig
      type buf = Buf.buf
      val create : int -> buf
      val contents : buf -> t
      val clear : buf -> unit
      val reset : buf -> unit
      val add_zChar : buf -> Zed_char.t -> unit
      val add_uChar : buf -> UChar.t -> unit
      val add_string : buf -> t -> unit
      val add_buffer : buf -> buf -> unit
      val add_char : buf -> UChar.t -> unit
    end
end

module US_Raw : sig
  type t
  val get : t -> int -> UChar.t
  val init : int -> (int -> UChar.t) -> t
  val length : t -> int
  type index
  val check_range : t -> int -> bool

  val look : t -> index -> UChar.t
  val nth : t -> int -> index
  val next : t -> index -> index
  val prev : t -> index -> index
  val out_of_range : t -> index -> bool
  val first : t -> index
  val last : t -> index
  val move : t -> index -> int -> index
  val compare_index : t -> index -> index -> int
  val iter : (UChar.t -> unit) -> t -> unit
  val compare : t -> t -> int

  module US = US

  module Buf :
    sig
      type buf = Buf.buf
      val create : int -> buf
      val contents : buf -> t
      val clear : buf -> unit
      val reset : buf -> unit
      val add_zChar : buf -> Zed_char.t -> unit
      val add_uChar : buf -> UChar.t -> unit
      val add_string : buf -> t -> unit
      val add_buffer : buf -> buf -> unit
      val add_char : buf -> UChar.t -> unit
    end
end

