(*
 * zed_actions.ml
 * --------------
 * Copyright : (c) 2016, Jeremie Dimino <jdimino@janestreet.com>
 * Licence   : BSD3
 *
 * This file is a part of zed.
 *)

module SMap = Map.Make(String)

module Arg = struct
  type 'a t = ..

  type 'a t +=
    | Rope : Zed_rope.t t
end

module Args = struct
  type 'a t =
    | Nil  : unit t
    | Cons : 'a Arg.t * 'b t -> ('a -> 'b) t

  let nil = Nil
  let ( +> ) a b = Cons (a, b)
end

module Action = struct
  type ('context, 'a) unpacked =
    { name : string
    ; doc  : string
    ; args : 'a Args.t
    ; impl : 'context -> 'a
    }

  type 'context t = A : ('context, _) unpacked -> 'context t

  let make name ~doc args impl =
    A { name
      ; doc
      ; args
      ; impl
      }

  let name (A x) = x.name
end

type 'context t = 'context Action.t SMap.t

let empty = SMap.empty

let add t l =
  List.fold_left
    (fun acc action -> SMap.add (Action.name action) action acc)
    t l

let wrap t ~f =
  SMap.map
    (fun (Action.A a) -> Action.A { a with impl = fun context -> a.impl (f context) })
    t
