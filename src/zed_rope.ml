(*
 * zed_rope.ml
 * -----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Zed, an editor engine.
 *)

(* Maximum length of a leaf *)
let max_leaf_size = 256

exception Out_of_bounds

(* +-----------------------------------------------------------------+
   | Ropes representation                                            |
   +-----------------------------------------------------------------+ *)

type t =
  | Leaf of Zed_utf8.t * int
      (* [Leaf(str, len)] *)
  | Node of int * int * t * int * t
      (* [Node(depth, length_left, left, length_right, right)] *)

type rope = t

type code_point = int

let empty = Leaf("", 0)

(* +-----------------------------------------------------------------+
   | Basic operations                                                |
   +-----------------------------------------------------------------+ *)

let rec length = function
  | Leaf(_, len) -> len
  | Node(_, len_l, _, len_r, _) -> len_l + len_r

let rec depth = function
  | Leaf _ -> 0
  | Node(d, _, _, _, _) -> d

let is_empty = function
  | Leaf(_, 0) -> true
  | _ -> false

(* +-----------------------------------------------------------------+
   | Balancing                                                       |
   +-----------------------------------------------------------------+ *)

let rec make_fibo acc a b =
  let c = a + b in
  if c < b then
    (* overflow *)
    acc
  else
    make_fibo (c :: acc) b c

let fibo =
  let l = make_fibo [1; 1; 0] 1 1 in
  let n = List.length l in
  let fibo = Array.make n 0 in
  let rec loop i = function
    | [] ->
        fibo
    | x :: l ->
        fibo.(i) <- x;
        loop (i - 1) l
  in
  loop (n - 1) l

let max_depth = Array.length fibo

let unsafe_concat rope1 rope2 =
  match rope1, rope2 with
    | Leaf(_, 0), _ -> rope2
    | _, Leaf(_, 0) -> rope1
    | _ -> Node(1 + max (depth rope1) (depth rope2),
                  length rope1, rope1,
                  length rope2, rope2)

let rec insert_to_forest forest acc idx =
  let acc = unsafe_concat forest.(idx) acc in
  if length acc < fibo.(idx + 1) then
    forest.(idx) <- acc
  else begin
    forest.(idx) <- empty;
    insert_to_forest forest acc (idx + 1)
  end

let rec concat_forest_until forest acc idx rope =
  if length rope < fibo.(idx + 1) then
    insert_to_forest forest (unsafe_concat acc rope) idx
  else begin
    let acc = unsafe_concat forest.(idx) acc in
    forest.(idx) <- empty;
    concat_forest_until forest acc (idx + 1) rope
  end

let rec balance_rec forest rope =
  match rope with
    | Leaf _ ->
        concat_forest_until forest empty 2 rope
    | Node(depth, len_l, rope_l, len_r, rope_r) ->
        balance_rec forest rope_l;
        balance_rec forest rope_r

let rec concat_forest forest acc idx =
  if idx = max_depth then
    acc
  else
    concat_forest forest (unsafe_concat forest.(idx) acc) (idx + 1)

let balance rope =
  match length rope with
    | 0 | 1 ->
        rope
    | len when len >= fibo.(depth rope + 2) ->
        rope
    | len ->
        let forest = Array.make max_depth empty in
        balance_rec forest rope;
        concat_forest forest empty 2

(* +-----------------------------------------------------------------+
   | Leaf operations                                               |
   +-----------------------------------------------------------------+ *)

let append rope1 rope2 =
  match rope1, rope2 with
    | Leaf(_, 0), _ ->
        rope2
    | _, Leaf(_, 0) ->
        rope1
    | Leaf(text1, len1), Leaf(text2, len2) when len1 + len2 <= max_leaf_size ->
        Leaf(text1 ^ text2, len1 + len2)
    | Node(d, len_l, rope_l, _, Leaf(text1, len1)), Leaf(text2, len2) when len1 + len2 <= max_leaf_size ->
        Node(d,
             len_l,
             rope_l,
             len1 + len2,
             Leaf(text1 ^ text2, len1 + len2))
    | Leaf(text1, len1), Node(d, _, Leaf(text2, len2), len_r, rope_r) when len1 + len2 <= max_leaf_size ->
        Node(d,
             len1 + len2,
             Leaf(text1 ^ text2, len1 + len2),
             len_r,
             rope_r)
    | _ ->
        balance (Node(1 + max (depth rope1) (depth rope2),
                        length rope1, rope1,
                        length rope2, rope2))

let concat sep l =
  let rec loop acc = function
    | [] -> acc
    | x :: l -> loop (append (append acc sep) x) l
  in
  match l with
    | [] -> empty
    | x :: l -> loop x l

let rec unsafe_get idx rope =
  match rope with
    | Leaf(text, _) ->
        Zed_utf8.get text idx
    | Node(_, len_l, rope_l, len_r, rope_r) ->
        if idx < len_l then
          unsafe_get idx rope_l
        else
          unsafe_get (idx - len_l) rope_r

let get rope idx =
  if idx < 0 || idx >= length rope then
    raise Out_of_bounds
  else
    unsafe_get idx rope

let rec unsafe_sub rope idx len =
  match rope with
    | Leaf(text, _) ->
        Leaf(Zed_utf8.sub text idx len, len)
    | Node(_, len_l, rope_l, len_r, rope_r) ->
        if len = len_l + len_r then
          rope
        else if idx >= len_l then
          unsafe_sub rope_r (idx - len_l) len
        else if idx + len <= len_l then
          unsafe_sub rope_l idx len
        else
          append
            (unsafe_sub rope_l idx (len_l - idx))
            (unsafe_sub rope_r 0 (len - len_l + idx))

let sub rope idx len =
  if idx < 0 || len < 0 || idx + len > length rope then
    raise Out_of_bounds
  else
    unsafe_sub rope idx len

let make length char =
  if length < max_leaf_size then
    Leaf(Zed_utf8.make length char, length)
  else begin
    let text = Zed_utf8.make max_leaf_size char in
    let chunk = Leaf(text, max_leaf_size) in
    let rec loop acc n =
      if n = 0 then
        acc
      else if n < max_leaf_size then
        append acc (Leaf(Zed_utf8.sub text 0 n, n))
      else
        loop (append acc chunk) (n - max_leaf_size)
    in
    loop empty length
  end

let singleton ch =
  Leaf(Zed_utf8.singleton ch, 1)

let break rope pos =
  let len = length rope in
  if pos < 0 || pos > len then raise Out_of_bounds;
  (unsafe_sub rope 0 pos, unsafe_sub rope pos (len - pos))

let before rope pos =
  sub rope 0 pos

let after rope pos =
  sub rope pos (length rope - pos)

let insert rope pos sub =
  let before, after = break rope pos in
  append before (append sub after)

let remove rope pos len =
  append (sub rope 0 pos) (sub rope (pos + len) (length rope - pos - len))

let replace rope pos len repl =
  append (sub rope 0 pos) (append repl (sub rope (pos + len) (length rope - pos - len)))

let lchop = function
  | Leaf(_, 0) -> empty
  | rope -> sub rope 1 (length rope - 1)

let rchop = function
  | Leaf(_, 0) -> empty
  | rope -> sub rope 0 (length rope - 1)

(* +-----------------------------------------------------------------+
   | Iterating, folding and mapping                                  |
   +-----------------------------------------------------------------+ *)

let rec iter f = function
  | Leaf(text, _) ->
      Zed_utf8.iter f text
  | Node(_, _, rope_l, _, rope_r) ->
      iter f rope_l;
      iter f rope_r

let rec rev_iter f = function
  | Leaf(text, _) ->
      Zed_utf8.rev_iter f text
  | Node(_, _, rope_l, _, rope_r) ->
      rev_iter f rope_r;
      rev_iter f rope_l

let rec fold f rope acc =
  match rope with
    | Leaf(text, _) ->
        Zed_utf8.fold f text acc
    | Node(_, _, rope_l, _, rope_r) ->
        fold f rope_r (fold f rope_l acc)

let rec rev_fold f rope acc =
  match rope with
    | Leaf(text, _) ->
        Zed_utf8.rev_fold f text acc
    | Node(_, _, rope_l, _, rope_r) ->
        rev_fold f rope_l (rev_fold f rope_r acc)

let rec map f = function
  | Leaf(txt, len) ->
      Leaf(Zed_utf8.map f txt, len)
  | Node(depth, length_l, rope_l, length_r, rope_r) ->
      Node(depth, length_l, map f rope_l, length_r, map f rope_r)

let rec rev_map f = function
  | Leaf(txt, len) ->
      Leaf(Zed_utf8.rev_map f txt, len)
  | Node(depth, length_l, rope_l, length_r, rope_r) ->
      Node(depth, length_r, map f rope_r, length_l, map f rope_l)

(* +-----------------------------------------------------------------+
   | Zippers                                                         |
   +-----------------------------------------------------------------+ *)

module Zip = struct

  type rope_zipper = {
    str : string;
    (* The string of the current leaf. *)
    ofs : int;
    (* The offset of the current leaf in the whole rope. *)
    leaf : t;
    (* The current leaf. *)
    rest_b : t list;
    rest_f : t list;
  }

  type t = {
    idx : int;
    (* The index in byte of the zipper in the current leaf. *)
    pos : int;
    (* The index in character of the zipper in the current leaf. *)
    zip : rope_zipper;
  }

  let rec move_utf8_f str idx len =
    if len = 0 then
      idx
    else
      move_utf8_f str (Zed_utf8.unsafe_next str idx) (len - 1)

  let rec make_f_rec ofs rope pos rest_b rest_f =
    match rope with
      | Leaf(str, _) ->
          { idx = move_utf8_f str 0 pos;
            pos = pos;
            zip = { str; ofs = ofs - pos; leaf = rope; rest_b; rest_f } }
      | Node(_, _, r1, _, r2) ->
          let len1 = length r1 in
          if pos < len1 then
            make_f_rec ofs r1 pos rest_b (r2 :: rest_f)
          else
            make_f_rec ofs r2 (pos - len1) (r1 :: rest_b) rest_f

  let make_f rope pos =
    if pos < 0 || pos > length rope then raise Out_of_bounds;
    make_f_rec pos rope pos [] []

  let rec move_utf8_b str idx len =
    if len = 0 then
      idx
    else
      move_utf8_b str (Zed_utf8.unsafe_prev str idx) (len - 1)

  let rec make_b_rec ofs rope pos rest_b rest_f =
    match rope with
      | Leaf(str, len) ->
          { idx = move_utf8_b str (String.length str) (len - pos);
            pos = pos;
            zip = { str; ofs = ofs - pos; leaf = rope; rest_b; rest_f } }
      | Node(_, _, r1, _, r2) ->
          let len1 = length r1 in
          if pos < len1 then
            make_b_rec ofs r1 pos rest_b (r2 :: rest_f)
          else
            make_b_rec ofs r2 (pos - len1) (r1 :: rest_b) rest_f

  let make_b rope pos =
    let len = length rope in
    if pos < 0 || pos > length rope then raise Out_of_bounds;
    let pos = len - pos in
    make_b_rec pos rope pos [] []

  let offset zip =
    zip.zip.ofs + zip.pos

  let rec next_leaf ofs rope rest_b rest_f =
    match rope with
      | Leaf(str, _) ->
          let chr, idx = Zed_utf8.unsafe_extract_next str 0 in
          (chr,
           { idx = idx;
             pos = 1;
             zip = { str; ofs; leaf = rope; rest_b; rest_f } })
      | Node(_, _, r1, _, r2) ->
          next_leaf ofs r1 rest_b (r2 :: rest_f)

  let next zip =
    if zip.idx = String.length zip.zip.str then
      match zip.zip.rest_f with
        | [] ->
            raise Out_of_bounds
        | rope :: rest ->
            next_leaf (zip.zip.ofs + length zip.zip.leaf) rope (zip.zip.leaf :: zip.zip.rest_b) rest
    else
      let chr, idx = Zed_utf8.unsafe_extract_next zip.zip.str zip.idx in
      (chr, { zip with idx; pos = zip.pos + 1 })

  let rec prev_leaf ofs rope rest_b rest_f =
    match rope with
      | Leaf(str, len) ->
          let chr, idx = Zed_utf8.unsafe_extract_prev str (String.length str) in
          (chr,
           { idx = idx;
             pos = len - 1;
             zip = { str; ofs = ofs - len; leaf = rope; rest_b; rest_f } })
      | Node(_, _, r1, _, r2) ->
          prev_leaf ofs r2 (r1 :: rest_b) rest_f

  let prev zip =
    if zip.idx = 0 then
      match zip.zip.rest_b with
        | [] ->
            raise Out_of_bounds
        | rope :: rest ->
            prev_leaf zip.zip.ofs rope rest (zip.zip.leaf :: zip.zip.rest_f)
    else
      let chr, idx = Zed_utf8.unsafe_extract_prev zip.zip.str zip.idx in
      (chr, { zip with idx; pos = zip.pos - 1 })

  let rec move_f n ofs rope rest_b rest_f =
    match rope with
      | Leaf(str, len) ->
          if n <= len then
            { idx = move_utf8_f str 0 n;
              pos = n;
              zip = { str; ofs; leaf = rope; rest_b; rest_f } }
          else begin
            match rest_f with
              | [] ->
                  raise Out_of_bounds
              | rope' :: rest_f ->
                  move_f (n - len) (ofs + len) rope' (rope :: rest_b) rest_f
          end
      | Node(_, _, r1, _, r2) ->
          move_f n ofs r1 rest_b (r2 :: rest_f)

  let rec move_b n ofs rope rest_b rest_f =
    match rope with
      | Leaf(str, len) ->
          if n <= len then
            { idx = move_utf8_b str (String.length str) n;
              pos = len - n;
              zip = { str; ofs; leaf = rope; rest_b; rest_f } }
          else begin
            match rest_b with
              | [] ->
                  raise Out_of_bounds
              | rope' :: rest_b ->
                  move_b (n - len) (ofs - len) rope' rest_b (rope :: rest_f)
          end
      | Node(_, _, r1, _, r2) ->
          move_b n ofs r2 (r1 :: rest_b) rest_f

  let move n zip =
    if n > 0 then
      let len = length zip.zip.leaf in
      if zip.pos + n <= len then
        { zip with idx = move_utf8_f zip.zip.str zip.idx n; pos = zip.pos + n }
      else
        match zip.zip.rest_f with
          | [] ->
              raise Out_of_bounds
          | rope :: rest_f ->
              move_f
                (n - (len - zip.pos))
                (zip.zip.ofs + len)
                rope
                (zip.zip.leaf :: zip.zip.rest_b)
                rest_f
    else
      if zip.pos + n >= 0 then
        { zip with idx = move_utf8_b zip.zip.str zip.idx (-n); pos = zip.pos + n }
      else
        match zip.zip.rest_b with
          | [] ->
              raise Out_of_bounds
          | rope :: rest_b ->
              move_b
                (n - zip.pos)
                zip.zip.ofs
                rope
                rest_b
                (zip.zip.leaf :: zip.zip.rest_f)

  let at_bos zip = zip.zip.rest_b = [] && zip.idx = 0
  let at_eos zip = zip.zip.rest_f = [] && zip.idx = String.length zip.zip.str

  let rec sub_rec acc ropes len =
    match ropes with
      | [] ->
          if len > 0 then
            raise Out_of_bounds
          else
            acc
      | rope :: rest ->
          let len' = length rope in
          if len <= len' then
            append acc (sub rope 0 len)
          else
            sub_rec (append acc rope) rest (len - len')

  let unsafe_sub str ofs len =
    let res = String.create len in
    String.unsafe_blit str ofs res 0 len;
    res

  let sub zip len =
    if len < 0 then
      raise Out_of_bounds
    else
      let len' = length zip.zip.leaf - zip.pos in
      if len <= len' then
        Leaf(unsafe_sub zip.zip.str zip.idx (move_utf8_f zip.zip.str zip.idx len), len)
      else
        sub_rec (Leaf(unsafe_sub zip.zip.str zip.idx (String.length zip.zip.str - zip.idx), len')) zip.zip.rest_f (len - len')

  let slice zip1 zip2 =
    let ofs1 = offset zip1 and ofs2 = offset zip2 in
    if ofs1 <= ofs2 then
      sub zip1 (ofs2 - ofs1)
    else
      sub zip2 (ofs1 - ofs2)

  let rec find_f f zip =
    if at_eos zip then
      zip
    else
      let ch, zip' = next zip in
      if f ch then
        zip
      else
        find_f f zip'

  let rec find_b f zip =
    if at_bos zip then
      zip
    else
      let ch, zip' = prev zip in
      if f ch then
        zip
      else
        find_b f zip'
end

(* +-----------------------------------------------------------------+
   | Comparison                                                      |
   +-----------------------------------------------------------------+ *)

let rec cmp_loop str1 ofs1 str2 ofs2 rest1 rest2 =
  if ofs1 = String.length str1 then
    match rest1 with
      | [] ->
          if ofs2 = String.length str2 && rest2 = [] then
            0
          else
            -1
      | rope1 :: rest1 ->
          cmp_search1 rope1 str2 ofs2 rest1 rest2
  else if ofs2 = String.length str2 then
    match rest2 with
      | [] ->
          1
      | rope2 :: rest2 ->
          cmp_search2 rope2 str1 ofs1 rest1 rest2
  else
    let chr1, ofs1 = Zed_utf8.unsafe_extract_next str1 ofs1 and chr2, ofs2 = Zed_utf8.unsafe_extract_next str2 ofs2 in
    let d = chr1 - chr2 in
    if d = 0 then
      cmp_loop str1 ofs1 str2 ofs2 rest1 rest2
    else
      d

and cmp_search1 rope1 str2 ofs2 rest1 rest2 =
  match rope1 with
    | Leaf(str1, _) ->
        cmp_loop str1 0 str2 ofs2 rest1 rest2
    | Node(_, _, rope1_l, _, rope1_r) ->
        cmp_search1 rope1_l str2 ofs2 (rope1_r :: rest1) rest2

and cmp_search2 rope2 str1 ofs1 rest1 rest2 =
  match rope2 with
    | Leaf(str2, _) ->
        cmp_loop str1 ofs1 str2 0 rest1 rest2
    | Node(_, _, rope2_l, _, rope2_r) ->
        cmp_search2 rope2_l str1 ofs1 rest1 (rope2_r :: rest2)

let rec cmp_init rope1 rope2 rest1 =
  match rope1 with
    | Leaf(str1, _) ->
        cmp_search2 rope2 str1 0 rest1 []
    | Node(_, _, rope1_l, _, rope1_r) ->
        cmp_init rope1_l rope2 (rope1_r :: rest1)

let compare r1 r2 = cmp_init r1 r2 []

let equal r1 r2 = length r1 = length r2 && compare r1 r2 = 0

(* +-----------------------------------------------------------------+
   | Buffers                                                         |
   +-----------------------------------------------------------------+ *)

module String_buffer = Buffer

module Buffer = struct
  type t = {
    mutable acc : rope;
    mutable buf : String_buffer.t;
    mutable idx : int;
  }

  let create () = {
    acc = empty;
    buf = String_buffer.create 1024;
    idx = 0;
  }

  let add buffer x =
    if buffer.idx = max_leaf_size then begin
      buffer.acc <- append buffer.acc (Leaf(String_buffer.contents buffer.buf, max_leaf_size));
      String_buffer.reset buffer.buf;
      String_buffer.add_string buffer.buf (Zed_utf8.singleton x);
      buffer.idx <- 1
    end else begin
      String_buffer.add_string buffer.buf (Zed_utf8.singleton x);
      buffer.idx <- buffer.idx + 1
    end

  let contents buffer =
    if buffer.idx = 0 then
      buffer.acc
    else
      append buffer.acc (Leaf(String_buffer.contents buffer.buf, buffer.idx))

  let reset buffer =
    String_buffer.reset buffer.buf;
    buffer.acc <- empty;
    buffer.idx <- 0
end

(* +-----------------------------------------------------------------+
   | To/from strings                                                 |
   +-----------------------------------------------------------------+ *)

let of_string str =
  let len = Zed_utf8.validate str in
  Leaf(str, len)

let to_string rope =
  let rec collect acc = function
    | Leaf(text, _) ->
        text :: acc
    | Node(_, _, rope_l, _, rope_r) ->
        collect (collect acc rope_r) rope_l
  in
  Zed_utf8.concat "" (collect [] rope)
