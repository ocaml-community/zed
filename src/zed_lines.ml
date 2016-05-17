(*
 * zed_lines.ml
 * ------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Zed, an editor engine.
 *)

exception Out_of_bounds

(* +-----------------------------------------------------------------+
   | Representation                                                  |
   +-----------------------------------------------------------------+ *)

(* Sets are represented by ropes. *)

type t =
  | String of int
      (* [String len] is a string of length [len] without newline
         character. *)
  | Return
      (* A newline character. *)
  | Concat of t * t * int * int * int
      (* [Concat(t1, t2, len, count, depth)] *)

(* +-----------------------------------------------------------------+
   | Basic functions                                                 |
   +-----------------------------------------------------------------+ *)

let length = function
  | String len -> len
  | Return -> 1
  | Concat(_, _, len, _, _) -> len

let count = function
  | String _ -> 0
  | Return -> 1
  | Concat(_, _, _, count, _) -> count

let depth = function
  | String _ | Return -> 0
  | Concat(_, _, _, _, d) -> d

let empty = String 0

(* +-----------------------------------------------------------------+
   | Offset/line resolution                                          |
   +-----------------------------------------------------------------+ *)

let rec line_index_rec set ofs acc =
  match set with
    | String _ ->
        acc
    | Return ->
        if ofs = 0 then
          acc
        else
          acc + 1
    | Concat(s1, s2, _, _, _) ->
        let len1 = length s1 in
        if ofs < len1 then
          line_index_rec s1 ofs acc
        else
          line_index_rec s2 (ofs - len1) (acc + count s1)

let line_index set ofs =
  if ofs < 0 || ofs > length set then
    raise Out_of_bounds
  else
    line_index_rec set ofs 0

let rec line_start_rec set idx acc =
  match set with
    | String _ ->
        acc
    | Return ->
        if idx = 0 then
          acc
        else
          acc + 1
    | Concat(s1, s2, _, _, _) ->
        let count1 = count s1 in
        if idx <= count1 then
          line_start_rec s1 idx acc
        else
          line_start_rec s2 (idx - count1) (acc + length s1)

let line_start set idx =
  if idx < 0 || idx > count set then
    raise Out_of_bounds
  else
    line_start_rec set idx 0

let line_stop set idx =
  if idx = count set
  then length set
  else line_start set (idx + 1) - 1

let line_length set idx =
  line_stop set idx - line_start set idx


(* +-----------------------------------------------------------------+
   | Operations on sets                                              |
   +-----------------------------------------------------------------+ *)

let concat set1 set2 =
  Concat(set1, set2,
         length set1 + length set2,
         count set1 + count set2,
         1 + max (depth set1) (depth set2))

let append set1 set2 =
  match set1, set2 with
    | String 0, _ -> set2
    | _, String 0 -> set1
    | String len1, String len2 ->
        String(len1 + len2)
    | String len1, Concat(String len2, set, len, count, h) ->
        Concat(String(len1 + len2), set, len + len1, count, h)
    | Concat(set, String len1, len, count, h), String len2 ->
        Concat(set, String(len1 + len2), len + len2, count, h)
    | _ ->
        let d1 = depth set1 and d2 = depth set2 in
        if d1 > d2 + 2 then begin
          match set1 with
            | String _ | Return ->
                assert false
            | Concat(set1_1, set1_2, _, _, _) ->
                if depth set1_1 >= depth set1_2 then
                  concat set1_1 (concat set1_2 set2)
                else begin
                  match set1_2 with
                    | String _ | Return ->
                        assert false
                    | Concat(set1_2_1, set1_2_2, _, _, _) ->
                        concat (concat set1_1 set1_2_1) (concat set1_2_2 set2)
                end
        end else if d2 > d1 + 2 then begin
          match set2 with
            | String _ | Return ->
                assert false
            | Concat(set2_1, set2_2, _, _, _) ->
                if depth set2_2 >= depth set2_1 then
                  concat (concat set1 set2_1) set2_2
                else begin
                  match set2_1 with
                    | String _ | Return ->
                        assert false
                    | Concat(set2_1_1, set2_1_2, _, _, _) ->
                        concat (concat set1 set2_1_1) (concat set2_1_2 set2_2)
                end
        end else
          concat set1 set2

let rec unsafe_sub set idx len =
  match set with
    | String _ ->
        String len
    | Return ->
        if len = 1 then
          Return
        else
          String 0
    | Concat(set_l, set_r, len', _, _) ->
        let len_l = length set_l in
        if len = len' then
          set
        else if idx >= len_l then
          unsafe_sub set_r (idx - len_l) len
        else if idx + len <= len_l then
          unsafe_sub set_l idx len
        else
          append
            (unsafe_sub set_l idx (len_l - idx))
            (unsafe_sub set_r 0 (len - len_l + idx))

let sub set idx len =
  if idx < 0 || len < 0 || idx + len > length set then
    raise Out_of_bounds
  else
    unsafe_sub set idx len

let break set ofs =
  let len = length set in
  if ofs < 0 || ofs > len then
    raise Out_of_bounds
  else
    (unsafe_sub set 0 ofs, unsafe_sub set ofs (len - ofs))

let insert set ofs set' =
  let set1, set2 = break set ofs in
  append set1 (append set' set2)

let remove set ofs len =
  append (sub set 0 ofs) (sub set (ofs + len) (length set - ofs - len))

let replace set ofs len repl =
  append (sub set 0 ofs) (append repl (sub set (ofs + len) (length set - ofs - len)))

(* +-----------------------------------------------------------------+
   | Sets from ropes                                                 |
   +-----------------------------------------------------------------+ *)

let of_rope rope =
  let rec loop zip len acc =
    match Zed_rope.Zip.next zip with
    | No_more -> append acc (String len)
    | Yield (ch, zip) ->
      if Uchar.to_int ch = 10 then
        loop0 zip (append (append acc (String len)) Return)
      else
        loop zip (len + 1) acc
  and loop0 zip acc =
    match Zed_rope.Zip.next zip with
    | No_more -> acc
    | Yield (ch, zip) ->
      if Uchar.to_int ch = 10 then
        loop0 zip (append acc Return)
      else
        loop zip 1 acc
  in
  loop0 (Zed_rope.Zip.make_f rope 0) empty
