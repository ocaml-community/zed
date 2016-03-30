(*
 * zed_bigarray.mli
 * ----------------
 * Copyright : (c) 2016, Jeremie Dimino <jdimino@janestreet.com>
 * Licence   : BSD3
 *
 * This file is a part of zed.
 *)

open Bigarray

val encode
  :  (char, int8_unsigned_elt, c_layout) Array1.t
  -> Uchar.t
  -> pos:int
  -> int
(** Encode a code-point in UTF-8 into a bigarray. Return the position just after the
    written character. *)
