(*
 * zed_rope.ml
 * -----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Copyright : (c) 2019, ZAN DoYe <zandoye@gmail.com>
 * Licence   : BSD3
 *
 * This file is a part of Zed, an editor engine.
 *)

(* Maximum length of a leaf *)
let max_leaf_size= 256

exception Out_of_bounds

(* +-----------------------------------------------------------------+
   | Ropes representation                                            |
   +-----------------------------------------------------------------+ *)

type t=
  (* the size is the number of UChar.t in the rope *)
  | Leaf of Zed_string.t * (int * int)
    (* [Leaf(str, (len, size))] *)
  | Node of int * (int * int) * t * (int * int) * t
    (* [Node(depth, (length_left, size_left), left, (length_right, size_right), right)] *)

type rope= t

let empty ()= Leaf (Zed_string.empty (), (0, 0))

(* +-----------------------------------------------------------------+
   | Basic operations                                                |
   +-----------------------------------------------------------------+ *)

let length= function
  | Leaf(_, (len, _)) -> len
  | Node(_, (len_l,_), _, (len_r,_), _) -> len_l + len_r

let size= function
  | Leaf(_, (_,size)) -> size
  | Node(_, (_,size_l), _, (_,size_r), _) -> size_l + size_r

let depth= function
  | Leaf _ -> 0
  | Node(d, _, _, _, _) -> d

let is_empty= function
  | Leaf(_, (0, 0)) -> true
  | _ -> false

(* +-----------------------------------------------------------------+
   | Balancing                                                       |
   +-----------------------------------------------------------------+ *)

let rec make_fibo acc a b=
  let c= a + b in
  if c < b then
    (* overflow *)
    acc
  else
    make_fibo (c :: acc) b c

let fibo=
  let l= make_fibo [1; 1; 0] 1 1 in
  let n= List.length l in
  let fibo= Array.make n 0 in
  let rec loop i= function
    | [] ->
        fibo
    | x :: l ->
        fibo.(i) <- x;
        loop (i - 1) l
  in
  loop (n - 1) l

let max_depth= Array.length fibo

let unsafe_concat rope1 rope2=
  match rope1, rope2 with
    | Leaf(_, (0,_)), _ -> rope2
    | _, Leaf(_, (0,_)) -> rope1
    | _ -> Node(
      1 + max (depth rope1) (depth rope2),
      (length rope1, size rope1), rope1,
      (length rope2, size rope2), rope2)

let rec insert_to_forest forest acc idx=
  let acc= unsafe_concat forest.(idx) acc in
  if length acc < fibo.(idx + 1) then
    forest.(idx) <- acc
  else begin
    forest.(idx) <- empty ();
    insert_to_forest forest acc (idx + 1)
  end

let rec concat_forest_until forest acc idx rope=
  if length rope < fibo.(idx + 1) then
    insert_to_forest forest (unsafe_concat acc rope) idx
  else begin
    let acc= unsafe_concat forest.(idx) acc in
    forest.(idx) <- empty ();
    concat_forest_until forest acc (idx + 1) rope
  end

let rec balance_rec forest rope=
  match rope with
    | Leaf _ ->
        concat_forest_until forest (empty ()) 2 rope
    | Node(_depth, _len_l, rope_l, _len_r, rope_r) ->
        balance_rec forest rope_l;
        balance_rec forest rope_r

let rec concat_forest forest acc idx=
  if idx = max_depth then
    acc
  else
    concat_forest forest (unsafe_concat forest.(idx) acc) (idx + 1)

let balance rope=
  match length rope with
    | 0 | 1 ->
        rope
    | len when len >= fibo.(depth rope + 2) ->
        rope
    | _len ->
        let forest= Array.make max_depth (empty ()) in
        balance_rec forest rope;
        concat_forest forest (empty ()) 2


(* +-----------------------------------------------------------------+
   | Leaf operations                                               |
   +-----------------------------------------------------------------+ *)

let append rope1 rope2 =
  match rope1, rope2 with
    | Leaf(_, (0,_)), _ ->
        rope2
    | _, Leaf(_, (0,_)) ->
        rope1
    | Leaf(text1, (len1, size1)), Leaf(text2, (len2, size2))
      when len1 + len2 <= max_leaf_size ->
        Leaf(Zed_string.append text1 text2, (len1+len2, size1+size2))
    | Node(d, len_l, rope_l, _, Leaf(text1, (len1,size1))), Leaf(text2, (len2,size2))
      when len1 + len2 <= max_leaf_size ->
      let ls= len1+len2, size1+size2 in
        Node(
          d,
          len_l,
          rope_l,
          ls,
          Leaf(Zed_string.append text1 text2, ls))
    | Leaf(text1, (len1,size1)), Node(d, _, Leaf(text2, (len2,size2)), len_r, rope_r)
      when len1 + len2 <= max_leaf_size ->
      let ls= len1+len2, size1+size2 in
        Node(
          d,
          ls,
          Leaf(Zed_string.append text1 text2, ls),
          len_r,
          rope_r)
    | _ ->
      balance (Node(
        1 + max (depth rope1) (depth rope2),
        (length rope1, size rope1), rope1,
        (length rope2, size rope2), rope2))

let concat sep l =
  let rec loop acc = function
    | [] -> acc
    | x :: l -> loop (append (append acc sep) x) l
  in
  match l with
    | [] -> empty ()
    | x :: l -> loop x l

let rec unsafe_get idx rope =
  match rope with
    | Leaf(text, _) ->
        Zed_string.get text idx
    | Node(_, (len_l,_), rope_l, _len_r, rope_r) ->
        if idx < len_l then
          unsafe_get idx rope_l
        else
          unsafe_get (idx - len_l) rope_r

let get rope idx =
  if idx < 0 || idx >= length rope then
    raise Out_of_bounds
  else
    unsafe_get idx rope

let rec unsafe_get_raw idx rope =
  match rope with
    | Leaf(text, _) ->
        Zed_string.get_raw text idx
    | Node(_, (_,size_l), rope_l, _len_r, rope_r) ->
        if idx < size_l then
          unsafe_get_raw idx rope_l
        else
          unsafe_get_raw (idx - size_l) rope_r

let get_raw rope idx =
  if idx < 0 || idx >= size rope then
    raise Out_of_bounds
  else
    unsafe_get_raw idx rope

let rec unsafe_sub rope idx len =
  match rope with
    | Leaf(text, _) ->
      let str= Zed_string.sub text idx len in
      let size= Zed_string.size str in
        Leaf(str, (len,size))
    | Node(_, (len_l,_), rope_l, (len_r,_), rope_r) ->
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
    Leaf(Zed_string.make length char, (length, length))
  else begin
    let text = Zed_string.make max_leaf_size char in
    let chunk = Leaf(text, (max_leaf_size, max_leaf_size)) in
    let rec loop acc n =
      if n = 0 then
        acc
      else if n < max_leaf_size then
        let str= Zed_string.sub text 0 n in
        let size= Zed_string.size str in
        append acc (Leaf(str, (n, size)))
      else
        loop (append acc chunk) (n - max_leaf_size)
    in
    loop (empty ()) length
  end

let singleton ch =
  Leaf(Zed_string.make 1 ch, (1, 1))

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

let insert_uChar rope pos ch =
  let open CamomileLibraryDefault.Camomile in
  if UChar.code ch = 0 then
    rope
  else
    match CharInfo_width.width ch with
    | 0-> let glyph= get rope (pos-1) in
      let glyph= Zed_char.append glyph ch in
      replace rope (pos-1) 1 (Leaf (Zed_string.of_char_list [glyph], (1, 1)))
    | _-> let sub= (Leaf (Zed_string.of_char_list [Zed_char.unsafe_of_uChar ch], (1, 1))) in
      insert rope pos sub

let lchop = function
  | Leaf(_, (0,_)) -> empty ()
  | rope -> sub rope 1 (length rope - 1)

let rchop = function
  | Leaf(_, (0,_)) -> empty ()
  | rope -> sub rope 0 (length rope - 1)

(* +-----------------------------------------------------------------+
   | Iterating, folding and mapping                                  |
   +-----------------------------------------------------------------+ *)

let rec iter f = function
  | Leaf(text, _) ->
      Zed_string.iter f text
  | Node(_, _, rope_l, _, rope_r) ->
      iter f rope_l;
      iter f rope_r

let rec rev_iter f = function
  | Leaf(text, _) ->
      Zed_string.rev_iter f text
  | Node(_, _, rope_l, _, rope_r) ->
      rev_iter f rope_r;
      rev_iter f rope_l

let rec fold f rope acc =
  match rope with
    | Leaf(text, _) ->
        Zed_string.fold f text acc
    | Node(_, _, rope_l, _, rope_r) ->
        fold f rope_r (fold f rope_l acc)

let rec rev_fold f rope acc =
  match rope with
    | Leaf(text, _) ->
        Zed_string.rev_fold f text acc
    | Node(_, _, rope_l, _, rope_r) ->
        rev_fold f rope_l (rev_fold f rope_r acc)

let rec map f = function
  | Leaf(txt, len) ->
      Leaf(Zed_string.map f txt, len)
  | Node(depth, length_l, rope_l, length_r, rope_r) ->
      let rope_l' = map f rope_l in
      let rope_r' = map f rope_r in
      Node(depth, length_l, rope_l', length_r, rope_r')

let rec rev_map f = function
  | Leaf(txt, len) ->
      Leaf(Zed_string.rev_map f txt, len)
  | Node(depth, length_l, rope_l, length_r, rope_r) ->
      let rope_l' = rev_map f rope_l in
      let rope_r' = rev_map f rope_r in
      Node(depth, length_r, rope_r', length_l, rope_l')

let rec iter_leaf f = function
  | Leaf(text, _) ->
      f text
  | Node(_, _, rope_l, _, rope_r) ->
      iter_leaf f rope_l;
      iter_leaf f rope_r

let rec rev_iter_leaf f = function
  | Leaf(text, _) ->
      f text
  | Node(_, _, rope_l, _, rope_r) ->
      rev_iter_leaf f rope_r;
      rev_iter_leaf f rope_l

let rec fold_leaf f rope acc =
  match rope with
    | Leaf(text, _) ->
        f text acc
    | Node(_, _, rope_l, _, rope_r) ->
        fold_leaf f rope_r (fold_leaf f rope_l acc)

let rec rev_fold_leaf f rope acc =
  match rope with
    | Leaf(text, _) ->
        f text acc
    | Node(_, _, rope_l, _, rope_r) ->
        rev_fold_leaf f rope_l (rev_fold_leaf f rope_r acc)

(* +-----------------------------------------------------------------+
   | Comparison                                                      |
   +-----------------------------------------------------------------+ *)

let rec cmp_loop str1 ofs1 str2 ofs2 rest1 rest2 =
  if ofs1 = Zed_string.length str1 then
    match rest1 with
      | [] ->
          if ofs2 = Zed_string.length str2 && rest2 = [] then
            0
          else
            -1
      | rope1 :: rest1 ->
          cmp_search1 rope1 str2 ofs2 rest1 rest2
  else if ofs2 = Zed_string.length str2 then
    match rest2 with
      | [] ->
          1
      | rope2 :: rest2 ->
          cmp_search2 rope2 str1 ofs1 rest1 rest2
  else
    let chr1, ofs1 = Zed_string.get str1 ofs1, ofs1+1
    and chr2, ofs2 = Zed_string.get str2 ofs2, ofs2+1 in
    let d = Zed_char.compare_raw chr1 chr2 in
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
   | Zippers                                                         |
   +-----------------------------------------------------------------+ *)

module Zip = struct
  type rope_zipper = {
    str : Zed_string.t;
    (* The string of the current leaf. *)
    ofs : int;
    (* The offset of the current leaf in the whole rope. *)
    leaf : t;
    (* The current leaf. *)
    rest_b : t list;
    rest_f : t list;
  }

  type t = {
    pos : int;
    (* The index in character of the zipper in the current leaf. *)
    zip : rope_zipper;
  }

  let rec make_f_rec ofs rope pos rest_b rest_f =
    match rope with
    | Leaf(str, _) ->
      { pos = pos;
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

  let rec make_b_rec ofs rope pos rest_b rest_f =
    match rope with
    | Leaf(str, _) ->
      { pos = pos;
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
      let chr= Zed_string.get str 0 in
      (chr,
       { pos = 1;
         zip = { str; ofs; leaf = rope; rest_b; rest_f } })
    | Node(_, _, r1, _, r2) ->
      next_leaf ofs r1 rest_b (r2 :: rest_f)

  let next zip =
    if zip.pos = Zed_string.length zip.zip.str then
      match zip.zip.rest_f with
      | [] ->
        raise Out_of_bounds
      | rope :: rest ->
        next_leaf (zip.zip.ofs + length zip.zip.leaf) rope (zip.zip.leaf :: zip.zip.rest_b) rest
    else
      let chr= Zed_string.get zip.zip.str zip.pos in
      (chr, { zip with pos = zip.pos + 1 })

  let rec prev_leaf ofs rope rest_b rest_f =
    match rope with
    | Leaf(str, (len,_size)) ->
      let chr=
        let len= Zed_string.length str - 1 in
        Zed_string.get str len
      in
      (chr,
       { pos = len - 1;
         zip = { str; ofs = ofs - len; leaf = rope; rest_b; rest_f } })
    | Node(_, _, r1, _, r2) ->
      prev_leaf ofs r2 (r1 :: rest_b) rest_f

  let prev zip =
    if zip.pos = 0 then
      match zip.zip.rest_b with
      | [] ->
        raise Out_of_bounds
      | rope :: rest ->
        prev_leaf zip.zip.ofs rope rest (zip.zip.leaf :: zip.zip.rest_f)
    else
      let chr= Zed_string.get zip.zip.str (zip.pos-1) in
      (chr, { zip with pos = zip.pos - 1 })

  let rec move_f n ofs rope rest_b rest_f =
    match rope with
    | Leaf(str, (len,_size)) ->
      if n <= len then
        { pos = n;
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
    | Leaf(str, (len,_size)) ->
      if n <= len then
        { pos = len - n;
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
        { zip with pos = zip.pos + n }
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
      { zip with pos = zip.pos + n }
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

  let at_bos zip= zip.zip.rest_b = [] && zip.pos = 0
  let at_eos zip= zip.zip.rest_f = [] && zip.pos = Zed_string.length zip.zip.str

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

  let sub zip len =
    if len < 0 then
      raise Out_of_bounds
    else
      let len' = length zip.zip.leaf - zip.pos in
      if len <= len' then
        let str= Zed_string.sub zip.zip.str zip.pos len in
        let size= Zed_string.size str in
        Leaf(str, (len,size))
      else
        let str= Zed_string.sub zip.zip.str zip.pos (Zed_string.length zip.zip.str - zip.pos) in
        let size= Zed_string.size str in
        sub_rec (Leaf(str, (len',size))) zip.zip.rest_f (len - len')

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

module Zip_raw = struct
  type rope_zipper = {
    str : Zed_string.t;
    (* The string of the current leaf. *)
    ofs : int;
    (* The offset of the current leaf in the whole rope. *)
    leaf : t;
    (* The current leaf. *)
    rest_b : t list;
    rest_f : t list;
  }

  type t = {
    pos : int;
    (* The index in character of the zipper in the current leaf. *)
    zip : rope_zipper;
  }

  let rec make_rec ofs rope pos rest_b rest_f =
    match rope with
    | Leaf(str, _) ->
      { pos = pos;
        zip = { str; ofs = ofs - pos; leaf = rope; rest_b; rest_f } }
    | Node(_, _, r1, _, r2) ->
      let size1= size r1 in
      if pos < size1 then
        make_rec ofs r1 pos rest_b (r2 :: rest_f)
      else
        make_rec ofs r2 (pos - size1) (r1 :: rest_b) rest_f

  let make_f rope pos =
    if pos < 0 || pos > size rope then raise Out_of_bounds;
    make_rec pos rope pos [] []

  let make_b rope pos =
    let size = size rope in
    if pos < 0 || pos > size then raise Out_of_bounds;
    let pos = size - pos in
    make_rec pos rope pos [] []

  let offset zip =
    zip.zip.ofs + zip.pos

  let rec next_leaf ofs rope rest_b rest_f =
    match rope with
    | Leaf(str, _) ->
      let chr= Zed_string.get_raw str 0 in
      (chr,
       { pos = 1;
         zip = { str; ofs; leaf = rope; rest_b; rest_f } })
    | Node(_, _, r1, _, r2) ->
      next_leaf ofs r1 rest_b (r2 :: rest_f)

  let next zip =
    if zip.pos = Zed_string.size zip.zip.str then
      match zip.zip.rest_f with
      | [] ->
        raise Out_of_bounds
      | rope :: rest ->
        next_leaf (zip.zip.ofs + size zip.zip.leaf) rope (zip.zip.leaf :: zip.zip.rest_b) rest
    else
      let chr= Zed_string.get_raw zip.zip.str zip.pos in
      (chr, { zip with pos = zip.pos + 1 })

  let rec prev_leaf ofs rope rest_b rest_f =
    match rope with
    | Leaf(str, (_len, size)) ->
      let chr=
        let len= Zed_string.size str - 1 in
        Zed_string.get_raw str len
      in
      (chr,
       { pos = size - 1;
         zip = { str; ofs = ofs - size; leaf = rope; rest_b; rest_f } })
    | Node(_, _, r1, _, r2) ->
      prev_leaf ofs r2 (r1 :: rest_b) rest_f

  let prev zip =
    if zip.pos = 0 then
      match zip.zip.rest_b with
      | [] ->
        raise Out_of_bounds
      | rope :: rest ->
        prev_leaf zip.zip.ofs rope rest (zip.zip.leaf :: zip.zip.rest_f)
    else
      let chr= Zed_string.get_raw zip.zip.str (zip.pos-1) in
      (chr, { zip with pos = zip.pos - 1 })

  let rec move_f n ofs rope rest_b rest_f =
    match rope with
    | Leaf(str, (_,size)) ->
      if n <= size then
        { pos = n;
          zip = { str; ofs; leaf = rope; rest_b; rest_f } }
      else begin
        match rest_f with
        | [] ->
          raise Out_of_bounds
        | rope' :: rest_f ->
          move_f (n - size) (ofs + size) rope' (rope :: rest_b) rest_f
      end
    | Node(_, _, r1, _, r2) ->
      move_f n ofs r1 rest_b (r2 :: rest_f)

  let rec move_b n ofs rope rest_b rest_f =
    match rope with
    | Leaf(str, (_,size)) ->
      if n <= size then
        { pos = size - n;
          zip = { str; ofs; leaf = rope; rest_b; rest_f } }
      else begin
        match rest_b with
        | [] ->
          raise Out_of_bounds
        | rope' :: rest_b ->
          move_b (n - size) (ofs - size) rope' rest_b (rope :: rest_f)
      end
    | Node(_, _, r1, _, r2) ->
      move_b n ofs r2 (r1 :: rest_b) rest_f

  let move n zip =
    if n > 0 then
      let size = size zip.zip.leaf in
      if zip.pos + n <= size then
        { zip with pos = zip.pos + n }
      else
        match zip.zip.rest_f with
        | [] ->
          raise Out_of_bounds
        | rope :: rest_f ->
          move_f
            (n - (size - zip.pos))
            (zip.zip.ofs + size)
            rope
            (zip.zip.leaf :: zip.zip.rest_b)
            rest_f
    else
    if zip.pos + n >= 0 then
      { zip with pos = zip.pos + n }
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

  let at_bos zip= zip.zip.rest_b = [] && zip.pos = 0
  let at_eos zip= zip.zip.rest_f = [] && zip.pos = Zed_string.length zip.zip.str

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
   | Buffers                                                         |
   +-----------------------------------------------------------------+ *)

module String_buffer = Buffer

module Buffer = struct
  type t = {
    mutable acc : rope;
    mutable buf : Zed_string.Buf.t;
    mutable idx : int;
  }

  let create () = {
    acc = empty ();
    buf = Zed_string.Buf.create 1024;
    idx = 0;
  }

  let add buffer x =
    if buffer.idx = max_leaf_size then begin
      let str= Zed_string.Buf.contents buffer.buf in
      let size= Zed_string.size str in
      buffer.acc <- append buffer.acc (Leaf(str, (max_leaf_size,size)));
      Zed_string.Buf.reset buffer.buf;
      Zed_string.Buf.add_zChar buffer.buf x;
      buffer.idx <- Zed_string.Buf.contents_len buffer.buf
    end else begin
      Zed_string.Buf.add_zChar buffer.buf x;
      buffer.idx <- Zed_string.Buf.contents_len buffer.buf
    end

  let add_uChar buffer x =
    if buffer.idx = max_leaf_size then begin
      let str= Zed_string.Buf.contents buffer.buf in
      let size= Zed_string.size str in
      buffer.acc <- append buffer.acc (Leaf(str, (max_leaf_size,size)));
      Zed_string.Buf.reset buffer.buf;
      Zed_string.Buf.add_uChar buffer.buf x;
      buffer.idx <- Zed_string.Buf.contents_len buffer.buf
    end else begin
      Zed_string.Buf.add_uChar buffer.buf x;
      buffer.idx <- Zed_string.Buf.contents_len buffer.buf
    end

  let add_rope buf rope= iter (add buf) rope
  let add_string buf str= Zed_string.iter (add buf) str

  let contents buffer =
    if buffer.idx = 0 then
      buffer.acc
    else
      let str= Zed_string.Buf.contents buffer.buf in
      let size= Zed_string.size str in
      append
        buffer.acc
        (Leaf (str, (buffer.idx, size)))

  let reset buffer =
    Zed_string.Buf.reset buffer.buf;
    buffer.acc <- empty ();
    buffer.idx <- 0
end

(* +-----------------------------------------------------------------+
   | Init                                                            |
   +-----------------------------------------------------------------+ *)

let init n f =
  let buf = Buffer.create () in
  for i = 0 to n - 1 do
    Buffer.add buf (f i)
  done;
  Buffer.contents buf

let init_from_uChars len f=
  match len with
  | 0-> empty ()
  | len when len > 0 ->
    let rec create n=
      if n > 0 then
        f (len - n) :: create (n-1)
      else []
    in
    let uChars= create len in
    let zChars, _= Zed_char.zChars_of_uChars uChars in
    let buf = Buffer.create () in
    List.iter (Buffer.add buf) zChars;
    Buffer.contents buf
  | _-> raise (Invalid_argument "Zed_rope.init_from_uChars")

let of_string s=
  let buf= Buffer.create () in
  Buffer.add_string buf s;
  Buffer.contents buf

let rec to_string t=
  match t with
  | Leaf (s,_)-> s
  | Node (_,_,l,_,r)-> Zed_string.append (to_string l) (to_string r)

module Text = struct
  type t = rope

  let get = get
  let init = init
  let length = length

  type index = Zip.t
  let look _ zip = fst (Zip.next zip)
  let nth rope idx = Zip.make_f rope idx
  let next _ zip = Zip.move 1 zip
  let prev _ zip = Zip.move (-1) zip
  let out_of_range _ zip = Zip.at_eos zip

  let iter = iter
  let compare = compare

  let first rope = Zip.make_f rope 0
  let last rope = Zip.make_b rope 1
  let move _ zip delta = Zip.move delta zip
  let compare_index _ zip1 zip2 = Zip.offset zip1 - Zip.offset zip2

  module Buf = struct
    type buf = Buffer.t
    let create _ = Buffer.create ()
    let contents = Buffer.contents
    let clear = Buffer.reset
    let reset = Buffer.reset
    let add_char = Buffer.add_uChar
    let add_string= Buffer.add_rope
    let add_buffer buf buf' = add_string buf (Buffer.contents buf')
  end
end

module Text_core = struct
  include Text
  let get t i= (get t i).core
  let init = init_from_uChars
  let look _ zip = (fst (Zip.next zip)).core
  let iter f= iter (fun c-> f c.core)
end

module Text_raw = struct
  type t = rope
  type index = Zip_raw.t

  let get= get_raw
  let init = init_from_uChars
  let length = length

  let look _ zip = fst (Zip_raw.next zip)
  let iter f= iter (fun c-> f c.core)

  let nth rope idx = Zip_raw.make_f rope idx
  let next _ zip = Zip_raw.move 1 zip
  let prev _ zip = Zip_raw.move (-1) zip
  let out_of_range _ zip = Zip_raw.at_eos zip

  let iter = iter
  let compare = compare

  let first rope = Zip_raw.make_f rope 0
  let last rope = Zip_raw.make_b rope 1
  let move _ zip delta = Zip_raw.move delta zip
  let compare_index _ zip1 zip2 = Zip_raw.offset zip1 - Zip_raw.offset zip2

  module Buf = struct
    type buf = Buffer.t
    let create _ = Buffer.create ()
    let contents = Buffer.contents
    let clear = Buffer.reset
    let reset = Buffer.reset
    let add_char = Buffer.add_uChar
    let add_string= Buffer.add_rope
    let add_buffer buf buf' = add_string buf (Buffer.contents buf')
  end

end

