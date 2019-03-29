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

type t = {
  core : UChar.t;
  combined : UChar.t list;
  width : int;
  size : int;
}

type pos = Core | Combined of int
val to_raw : t -> UChar.t list
val to_array : t -> UChar.t array
type char_prop = Printable of int | Other | Null
val zero : t
val prop_uChar : UChar.t -> char_prop
val prop : t -> char_prop
val length : t -> int
val width : t -> int
val size : t -> int
val calc_size' : 'a list -> int
val calc_size : t -> int
val out_of_range : t -> int -> bool
val get : t -> int -> UChar.t
val get_opt : t -> int -> UChar.t option
val append : t -> UChar.t -> t
val compare_core : t -> t -> int
val compare_raw : t -> t -> int
val mix_uChar : t -> UChar.t -> (t, t) result
val first_core :
  UChar.t list ->
  (char_prop * UChar.t) option *
  UChar.t list
val subsequent :
  UChar.t list ->
  UChar.t list * UChar.t list
val of_uChars :
  UChar.t list -> t option * UChar.t list
val unsafe_of_char : char -> t
val unsafe_of_uChar : UChar.t -> t

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

