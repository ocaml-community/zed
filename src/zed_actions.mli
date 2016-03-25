(*
 * zed_actions.mli
 * ---------------
 * Copyright : (c) 2016, Jeremie Dimino <jdimino@janestreet.com>
 * Licence   : BSD3
 *
 * This file is a part of zed.
 *)

(** Editing actions *)

(** This module provides a simple way to define editing actions that can be configured ty
    the user. It's not supposed to be comprehensive, but it's enough to write inputrc like
    files.
*)

type 'context t

module Arg : sig
  type 'a t = ..

  type 'a t +=
    | Rope : Zed_rope.t t
end

module Args : sig
  type 'a t (* with 4.03:
               = [] : unit t | ( :: ) : 'a Arg.t * 'b t -> ('a -> 'b) t *)

  val nil    : unit t
  val ( +> ) : 'a Arg.t -> 'b t -> ('a -> 'b) t
end

module Action : sig
  type 'context t

  val make
    :  string
    -> doc:string
    -> 'a Args.t
    -> ('context -> 'a)
    -> 'context t
end

val empty : 'context t

val add : 'context t -> 'context Action.t list -> 'context t

val wrap : 'old_context t -> f:('new_context -> 'old_context) -> 'new_context t
