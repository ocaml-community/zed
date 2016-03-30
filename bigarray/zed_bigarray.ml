(*
 * zed_bigarray.ml
 * ---------------
 * Copyright : (c) 2016, Jeremie Dimino <jdimino@janestreet.com>
 * Licence   : BSD3
 *
 * This file is a part of zed.
 *)

open Bigarray

let encode (buf : (char, int8_unsigned_elt, c_layout) Array1.t) char ~pos =
  let code = Uchar.to_int char in
  if code < 0x80 then begin
    buf.{pos} <- Char.unsafe_chr code;
    pos + 1;
  end else if code <= 0x800 then begin
    buf.{pos    } <- Char.unsafe_chr ((code lsr  6)    lor 0xc0);
    buf.{pos + 1} <- Char.unsafe_chr ((code land 0x3f) lor 0x80);
    pos + 2
  end else if code <= 0x10000 then begin
    buf.{pos    } <- Char.unsafe_chr ((code lsr  12)           lor 0xe0);
    buf.{pos + 1} <- Char.unsafe_chr (((code lsr 6) land 0x3f) lor 0x80);
    buf.{pos + 2} <- Char.unsafe_chr ((code land 0x3f)         lor 0x80);
    pos + 3
  end else if code <= 0x10ffff then begin
    buf.{pos    } <- Char.unsafe_chr ((code lsr  18)            lor 0xf0);
    buf.{pos + 1} <- Char.unsafe_chr (((code lsr 12) land 0x3f) lor 0x80);
    buf.{pos + 2} <- Char.unsafe_chr (((code lsr 6) land 0x3f)  lor 0x80);
    buf.{pos + 3} <- Char.unsafe_chr ((code land 0x3f)          lor 0x80);
    pos + 4
  end else
    assert false

