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

type seg_width = { start : int; len : int; width : int; }
  (** Type of the width of a segment of a Zed_string.t *)

type all_width = { len : int; width : int; }
  (** Type of the width of a whole Zed_string.t *)

type width = (all_width, seg_width) result
  (** Type of the width of a Zed_string.t *)

type t = { chars : Zed_char.t array; width : width; size : int; }
  (** Type of Zed_string.t *)

val aval_width : width -> int
  (** Returns the widest available width *)

val init : int -> (int -> Zed_char.t) -> t
val init_from_uChars : int -> (int -> UChar.t) -> t
val make : int -> Zed_char.t -> t

val calc_size : t -> int
val copy : t -> t
val to_raw_list : t -> UChar.t list
val to_raw_array : t -> UChar.t array
type index = int
val get : t -> int -> Zed_char.t
val get_raw : t -> int -> UChar.t
val empty : unit -> t
val calc_width : ?start:int -> ?num:int -> Zed_char.t array -> width
val width : ?start:int -> t -> width

val size : t -> int
  (** [size ch] returns the size (number of characters) of [ch]. *)

val length : t -> int
  (** Aliase of size *)

val calc_width_unsafe : Zed_char.t array -> int
val of_uChars :
  UChar.t list -> t * UChar.t list
val of_char_list : Zed_char.t list -> t
val of_char_array : Zed_char.t array -> t

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
    type buf = {
      mutable buffer : Zed_char.t array;
      mutable position : int;
      mutable length : int;
      initial_buffer : Zed_char.t array;
    }
    val resize : buf -> int -> unit
    val create : int -> buf
    val contents : buf -> t
    val contents_len : buf -> int
    val clear : buf -> unit
    val reset : buf -> unit
    val add_zChar : buf -> Zed_char.t -> unit
    val add_uChar : buf -> UChar.t -> unit
    val add_string : buf -> t -> unit
    val add_buffer : buf -> buf -> unit
    type t = buf
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
  val charsL_to_list : Zed_char.t list -> UChar.t list
  val charsL_to_array :
    Zed_char.t list -> UChar.t array
  val chars_to_list : Zed_char.t array -> UChar.t list
  val chars_to_array :
    Zed_char.t array -> UChar.t array

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
      val resize : buf -> int -> unit
      val create : int -> buf
      val contents : buf -> t
      val contents_len : buf -> int
      val clear : buf -> unit
      val reset : buf -> unit
      val add_zChar : buf -> Zed_char.t -> unit
      val add_uChar : buf -> UChar.t -> unit
      val add_string : buf -> t -> unit
      val add_buffer : buf -> buf -> unit
      val add_char : buf -> UChar.t -> unit
      type t = buf
    end
end

module US_Raw : sig
  type t
  val memory_get : unit -> t -> int -> UChar.t
  val get : t -> int -> UChar.t
  val init : int -> (int -> UChar.t) -> t
  val length : t -> int
  type index = int * int
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
      val resize : buf -> int -> unit
      val create : int -> buf
      val contents : buf -> t
      val contents_len : buf -> int
      val clear : buf -> unit
      val reset : buf -> unit
      val add_zChar : buf -> Zed_char.t -> unit
      val add_uChar : buf -> UChar.t -> unit
      val add_string : buf -> t -> unit
      val add_buffer : buf -> buf -> unit
      val add_char : buf -> UChar.t -> unit
      type t = buf
    end
end

