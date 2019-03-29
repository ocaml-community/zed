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

module Zed_string0 : sig
  module UCharArray :
    sig
      module UCharArray0 :
        sig
          type t = UChar.t array
          val length : t -> int
          val get : t -> int -> UChar.t
        end
      module Width0 :
        sig
          val width :
            ?cfg:CharInfo_width.Cfg.widthTable option ->
            UCharArray0.t -> (int, int) result
        end
      type t = UChar.t array
      val length : t -> int
      val get : t -> int -> UChar.t
      val width :
        ?cfg:CharInfo_width.Cfg.widthTable option ->
        UCharArray0.t -> (int, int) result
    end
  type seg_width = { start : int; len : int; width : int; }
  type all_width = { len : int; width : int; }
  type width = (all_width, seg_width) result
  type t = { chars : Zed_char.t array; width : width; size : int; }
  val aval_width : (all_width, seg_width) result -> int
  val calc_size' : Zed_char.t array -> int
  val calc_size : t -> int
  val copy : t -> t
  val to_raw_list : t -> UChar.t list
  val to_raw_array : t -> UChar.t array
  val charsL_to_raw_list : Zed_char.t list -> UChar.t list
  val charsL_to_raw_array :
    Zed_char.t list -> UChar.t array
  val chars_to_raw_list : Zed_char.t array -> UChar.t list
  val chars_to_raw_array :
    Zed_char.t array -> UChar.t array
  type index = int
  val get : t -> int -> Zed_char.t
  val get_raw : t -> int -> UChar.t
  val empty : unit -> t
  val yChars_of_uChars :
    UChar.t list ->
    Zed_char.t list * UChar.t list
  val calc_width :
    ?start:int ->
    ?num:int -> Zed_char.t array -> (all_width, seg_width) result
  val init : int -> (int -> Zed_char.t) -> t
  val init_from_uChars : int -> (int -> UChar.t) -> t
  val length : t -> int
  val width : t -> width
  val width_from : int -> t -> (all_width, seg_width) result
  val size : t -> int
  val add_width :
    (all_width, seg_width) result ->
    (all_width, seg_width) result ->
    (all_width, seg_width) result
  val calc_width_unsafe : Zed_char.t array -> int
  val of_uChars :
    UChar.t list -> t * UChar.t list
  val of_char_list : Zed_char.t list -> t
  val of_char_array : Zed_char.t array -> t
  module US0 :
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
  val check_range : t -> int -> bool
  val look : t -> int -> UChar.t
  val nth : t -> int -> int
  val next : t -> int -> int
  val prev : t -> int -> int
  val out_of_range : t -> int -> bool
  val iter : (Zed_char.t -> unit) -> t -> unit
  val rev_iter : (Zed_char.t -> unit) -> t -> unit
  val fold : (Zed_char.t -> 'a -> 'a) -> t -> 'a -> 'a
  val rev_fold : (Zed_char.t -> 'a -> 'a) -> t -> 'a -> 'a
  val map : (Zed_char.t -> Zed_char.t) -> t -> t
  val rev_map : (Zed_char.t -> Zed_char.t) -> t -> t
  val compare : t -> t -> int
  val first : t -> int
  val last : t -> int
  val move : t -> int -> int -> int
  val compare_index : t -> 'a -> 'a -> int
  val sub : t -> int -> int -> t
  val after : t -> int -> t
  val unsafe_sub_equal : t -> int -> t -> int -> bool
  val starts_with : prefix:t -> t -> bool
  val make : int -> Zed_char.t -> t
  val append : t -> t -> t
  module Buf0 :
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
      val add_yChar : buf -> Zed_char.t -> unit
      val add_uChar : buf -> UChar.t -> unit
      val add_string : buf -> t -> unit
      val add_buffer : buf -> buf -> unit
      type t = buf
    end
end

module US_Core : sig
  module UCharArray = Zed_string0.UCharArray
  type seg_width =
    Zed_string0.seg_width = {
    start : int;
    len : int;
    width : int;
  }
  type all_width = Zed_string0.all_width = { len : int; width : int; }
  type width = (all_width, seg_width) result
  type t =
    Zed_string0.t = {
    chars : Zed_char.t array;
    width : width;
    size : int;
  }
  val aval_width : (all_width, seg_width) result -> int
  val calc_size' : Zed_char.t array -> int
  val calc_size : t -> int
  val copy : t -> t
  type index = int
  val get_raw : t -> int -> UChar.t
  val empty : unit -> t
  val yChars_of_uChars :
    UChar.t list ->
    Zed_char.t list * UChar.t list
  val calc_width :
    ?start:int ->
    ?num:int -> Zed_char.t array -> (all_width, seg_width) result
  val init_from_uChars : int -> (int -> UChar.t) -> t
  val length : t -> int
  val width : t -> width
  val width_from : int -> t -> (all_width, seg_width) result
  val size : t -> int
  val add_width :
    (all_width, seg_width) result ->
    (all_width, seg_width) result ->
    (all_width, seg_width) result
  val calc_width_unsafe : Zed_char.t array -> int
  val of_uChars :
    UChar.t list -> t * UChar.t list
  val of_char_list : Zed_char.t list -> t
  val of_char_array : Zed_char.t array -> t
  module US0 = Zed_string0.US0
  val check_range : t -> int -> bool
  val look : t -> int -> UChar.t
  val nth : t -> int -> int
  val next : t -> int -> int
  val prev : t -> int -> int
  val out_of_range : t -> int -> bool
  val rev_iter : (Zed_char.t -> unit) -> t -> unit
  val fold : (Zed_char.t -> 'a -> 'a) -> t -> 'a -> 'a
  val rev_fold : (Zed_char.t -> 'a -> 'a) -> t -> 'a -> 'a
  val map : (Zed_char.t -> Zed_char.t) -> t -> t
  val rev_map : (Zed_char.t -> Zed_char.t) -> t -> t
  val first : t -> int
  val last : t -> int
  val move : t -> int -> int -> int
  val compare_index : t -> 'a -> 'a -> int
  val sub : t -> int -> int -> t
  val after : t -> int -> t
  val unsafe_sub_equal : t -> int -> t -> int -> bool
  val starts_with : prefix:t -> t -> bool
  val make : int -> Zed_char.t -> t
  val append : t -> t -> t
  module Buf0 = Zed_string0.Buf0
  val get : t -> int -> UChar.t
  val init : int -> (int -> UChar.t) -> t
  val iter : (UChar.t -> unit) -> t -> unit
  val compare : t -> t -> int
  val to_raw_list : t -> UChar.t list
  val to_raw_array : t -> UChar.t array
  val charsL_to_raw_list : Zed_char.t list -> UChar.t list
  val charsL_to_raw_array :
    Zed_char.t list -> UChar.t array
  val chars_to_raw_list : Zed_char.t array -> UChar.t list
  val chars_to_raw_array :
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
      type buf =
        Zed_string0.Buf0.buf = {
        mutable buffer : Zed_char.t array;
        mutable position : int;
        mutable length : int;
        initial_buffer : Zed_char.t array;
      }
      type t = buf
      val resize : buf -> int -> unit
      val create : int -> buf
      val contents : buf -> Zed_string0.t
      val contents_len : buf -> int
      val clear : buf -> unit
      val reset : buf -> unit
      val add_yChar : buf -> Zed_char.t -> unit
      val add_uChar : buf -> UChar.t -> unit
      val add_string : buf -> Zed_string0.t -> unit
      val add_buffer : buf -> buf -> unit
      val add_char : buf -> UChar.t -> unit
    end
end

module US_Raw : sig
  type width = Zed_string0.width
  type t = Zed_string0.t
  val memory_get : unit -> Zed_string0.t -> int -> UChar.t
  val get : Zed_string0.t -> int -> UChar.t
  val init : int -> (int -> UChar.t) -> Zed_string0.t
  val length : Zed_string0.t -> int
  type index = int * int
  val check_range : Zed_string0.t -> int -> bool
  val out_of_range : Zed_string0.t -> int * int -> bool
  val look : Zed_string0.t -> int * int -> UChar.t
  val nth : Zed_string0.t -> int -> int * int
  val next : Zed_string0.t -> int * int -> int * int
  val prev : Zed_string0.t -> int * int -> int * int
  val first : 'a -> int * int
  val last : Zed_string0.t -> int * int
  val move : Zed_string0.t -> int * int -> int -> int * int
  val compare_index : 'a -> 'b * 'c -> 'b * 'c -> int
  val iter : (UChar.t -> unit) -> Zed_string0.t -> unit
  val compare : Zed_string0.t -> Zed_string0.t -> int
  module US = Zed_string0.US0
  module Buf :
    sig
      type buf =
        Zed_string0.Buf0.buf = {
        mutable buffer : Zed_char.t array;
        mutable position : int;
        mutable length : int;
        initial_buffer : Zed_char.t array;
      }
      type t = buf
      val resize : buf -> int -> unit
      val create : int -> buf
      val contents : buf -> Zed_string0.t
      val contents_len : buf -> int
      val clear : buf -> unit
      val reset : buf -> unit
      val add_yChar : buf -> Zed_char.t -> unit
      val add_uChar : buf -> UChar.t -> unit
      val add_string : buf -> Zed_string0.t -> unit
      val add_buffer : buf -> buf -> unit
      val add_char : buf -> UChar.t -> unit
    end
end

module UCharArray = Zed_string0.UCharArray

type seg_width =
  Zed_string0.seg_width = {
  start : int;
  len : int;
  width : int;
}

type all_width = Zed_string0.all_width = { len : int; width : int; }

type width = (all_width, seg_width) result

type t =
  Zed_string0.t = {
  chars : Zed_char.t array;
  width : width;
  size : int;
}

val aval_width : (all_width, seg_width) result -> int
val calc_size' : Zed_char.t array -> int
val calc_size : t -> int
val copy : t -> t
val to_raw_list : t -> UChar.t list
val to_raw_array : t -> UChar.t array
val charsL_to_raw_list : Zed_char.t list -> UChar.t list
val charsL_to_raw_array : Zed_char.t list -> UChar.t array
val chars_to_raw_list : Zed_char.t array -> UChar.t list
val chars_to_raw_array : Zed_char.t array -> UChar.t array

type index = int

val get : t -> int -> Zed_char.t
val get_raw : t -> int -> UChar.t
val empty : unit -> t
val yChars_of_uChars :
  UChar.t list ->
  Zed_char.t list * UChar.t list
val calc_width :
  ?start:int ->
  ?num:int -> Zed_char.t array -> (all_width, seg_width) result
val init : int -> (int -> Zed_char.t) -> t
val init_from_uChars : int -> (int -> UChar.t) -> t
val length : t -> int
val width : t -> width
val width_from : int -> t -> (all_width, seg_width) result
val size : t -> int
val add_width :
  (all_width, seg_width) result ->
  (all_width, seg_width) result ->
  (all_width, seg_width) result
val calc_width_unsafe : Zed_char.t array -> int
val of_uChars :
  UChar.t list -> t * UChar.t list
val of_char_list : Zed_char.t list -> t
val of_char_array : Zed_char.t array -> t

module US0 = Zed_string0.US0

val check_range : t -> int -> bool
val look : t -> int -> UChar.t
val nth : t -> int -> int
val next : t -> int -> int
val prev : t -> int -> int
val out_of_range : t -> int -> bool
val iter : (Zed_char.t -> unit) -> t -> unit
val rev_iter : (Zed_char.t -> unit) -> t -> unit
val fold : (Zed_char.t -> 'a -> 'a) -> t -> 'a -> 'a
val rev_fold : (Zed_char.t -> 'a -> 'a) -> t -> 'a -> 'a
val map : (Zed_char.t -> Zed_char.t) -> t -> t
val rev_map : (Zed_char.t -> Zed_char.t) -> t -> t
val compare : t -> t -> int
val first : t -> int
val last : t -> int
val move : t -> int -> int -> int
val compare_index : t -> 'a -> 'a -> int
val sub : t -> int -> int -> t
val after : t -> int -> t
val unsafe_sub_equal : t -> int -> t -> int -> bool
val starts_with : prefix:t -> t -> bool
val make : int -> Zed_char.t -> t
val append : t -> t -> t

module Buf0 = Zed_string0.Buf0

module US = US0

module Buf = Buf0

