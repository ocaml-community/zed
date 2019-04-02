(*
 * zed_string.ml
 * -----------
 * Copyright : (c) 2019, ZAN DoYe <zandoye@gmail.com>
 * Licence   : BSD3
 *
 * This file is a part of Zed, an editor engine.
 *)


open CamomileLibraryDefault.Camomile
open Result

module Zed_string0 = struct
  module UCharArray = struct
    module UCharArray0 = struct
      type t= UChar.t array
      let length: t-> int= Array.length
      let get: t -> int -> UChar.t= Array.get
    end
    module Width0 = CharInfo_width.String(UCharArray0)

    include UCharArray0
  end


  type seg_width= {
    start: int;
    len: int;
    width: int;
  }

  type all_width= {
    len: int;
    width: int;
  }

  type width= (all_width, seg_width) result

  type t= {
    chars: Zed_char.t array;
    width: width;
    size: int;
  }

  let aval_width= function
    | Ok {len=_;width}-> width
    | Error {start=_;len=_;width}-> width

  let calc_size'= Array.fold_left (fun acc c-> Zed_char.size c + acc) 0
  let calc_size t= calc_size' t.chars

  let copy t= {t with chars= Array.copy t.chars }

  let to_raw_list t= t.chars
    |> Array.to_list
    |> List.map Zed_char.to_raw
    |> List.concat

  let to_raw_array t= t.chars
    |> Array.to_list
    |> List.map Zed_char.to_array
    |> Array.concat

  let chars_to_raw_list t= t
    |> Array.to_list
    |> List.map Zed_char.to_raw
    |> List.concat

  let chars_to_raw_array t= t
    |> Array.to_list
    |> List.map Zed_char.to_array
    |> Array.concat

  type index= int

  let get t i= t.chars.(i)
  let get_raw t i=
    let rec get pos offset=
      let zChar= t.chars.(pos) in
      if i < offset + Zed_char.size zChar then
        Zed_char.get zChar (i - offset)
      else
        get (pos+1) (offset + Zed_char.size zChar)
    in
    get 0 0


  let empty ()= { chars= [||]; width= Ok {len= 0; width= 0}; size= 0 }

  let calc_width ?(start=0) ?num (chars: Zed_char.t array)=
    let length= Array.length chars in
    let rec calc w i=
      if i < length then
        if Zed_char.width chars.(i) > 0 then
          calc (w + Zed_char.width chars.(i)) (i+1)
        else
          Error { start; len= i - start; width= w }
      else Ok {len= length; width= w }
    in
    let calc_num num w i=
      let num= min num length in
      let rec calc n w i=
        if i < length && n > 0 then
          if Zed_char.width chars.(i) > 0 then
            calc (n-1) (w + Zed_char.width chars.(i)) (i+1)
          else
            Error { start; len= i - start; width= w }
        else Ok {len= num; width= w }
      in
      calc num w i
    in
    match num with
    | Some num-> calc_num num 0 start
    | None-> calc 0 start

  let init len (f: int -> Zed_char.t)=
    let rec create n=
      if n > 0 then
        f (len - n) :: create (n-1)
      else []
    in
    let chars= create len |> Array.of_list in
    let width= calc_width chars
    and size= calc_size' chars in
    { chars; width; size }

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
      let chars= Array.of_list zChars in
      let width= calc_width chars
      and size= calc_size' chars in
      {  chars; width; size }
    | _-> raise (Invalid_argument "Zed_string0.init_from_uChars")


  let chars t= t.chars

  let length t= Array.length t.chars

  let width ?start t=
    match start with
    | None-> t.width
    | _-> calc_width ?start t.chars

  let size t= t.size

    let add_width w1 w2=
      match w1, w2 with
      | Ok { len= l1; width= w1 }, Ok { len= l2; width= w2 }->
        Ok { len= l1 + l2; width= w1 + w2 }
      | Ok { len= l1; width= w1 }, Error { start; len= l2; width= w2}->
        Error { start= l1 + start; len= l1 + l2; width= w1 + w2 }
      | Error _ as w, _-> w

  let calc_width_unsafe chars= Array.fold_left
    (+) 0 (Array.map Zed_char.width chars)

  let of_uChars uChars=
    let zChars, uChars= Zed_char.zChars_of_uChars uChars in
    let chars= Array.of_list zChars in
    let width= calc_width chars
    and size= calc_size' chars in
    { chars; width; size }, uChars

  let of_char_list cl=
    let chars= Array.of_list cl in
    let width= calc_width chars
    and size= calc_size' chars in
    { chars; width; size }

  let of_char_array ca=
    let chars= ca in
    let width= calc_width chars
    and size= calc_size' chars in
    { chars; width; size }

  let for_all p str= Array.for_all p str.chars

  let check_range t n= n >= 0 && n <= length t

  let look t i= Zed_char.core t.chars.(i)

  let nth t n= if check_range t n
    then n
    else raise (Invalid_argument "Zed_string.nth")

  let next t n=
    let n= n + 1 in
    if check_range t n
    then n
    else raise (Invalid_argument "Zed_string.next")

  let prev t n=
    let n= n - 1 in
    if check_range t n
    then n
    else raise (Invalid_argument "Zed_string.prev")

  let out_of_range t n= n < 0 || n >= length t

  let iter f t= Array.iter f t.chars

  let rev_iter f t=
    for i= Array.length t.chars - 1 downto 0 do
      f t.chars.(i)
    done

  let fold f t acc=
    let len= Array.length t.chars in
    let rec fold acc i=
      if i < len then
        fold (f t.chars.(i) acc) (i+1)
      else
        acc
    in
    fold acc 0

  let rev_fold f t acc=
    let rec fold acc i=
      if i >= 0 then
        fold (f t.chars.(i) acc) (i-1)
      else
        acc
    in
    fold acc (Array.length t.chars - 1)

  let map f t=
    let chars= Array.map f t.chars in
    let width= calc_width chars
    and size= calc_size' chars in
    { chars; width; size }

  let rev_map f t=
    let chars= Array.map f (Zed_utils.array_rev t.chars) in
    let width= calc_width chars
    and size= calc_size' chars in
    { chars; width; size }

  let compare t1 t2= Zed_utils.array_compare
    ~compare:Zed_char.compare_raw
    t1.chars t2.chars

  let first (_:t)= 0
  let last t= max (length t - 1) 0

  let move t i n=
    let len= length t in
    let pos= i + n in
    if i >= 0 && i < len
    then pos
    else raise (Invalid_argument "Zed_string.move: index out of bounds")

  let compare_index (_:t) i j= Pervasives.compare i j

  let sub s ofs len =
    if ofs < 0 || len < 0 || ofs > length s - len then
      invalid_arg "Zed_string.sub"
    else
      let chars= Array.sub s.chars ofs len in
      let width= calc_width chars
      and size= calc_size' chars in
      { chars; width; size }

  let after s i=
    let len= length s in
    if i < len then
      sub s i (len-i)
    else
      empty ()

  let rec unsafe_sub_equal str ofs sub ofs_sub=
    if ofs_sub = length sub then
      true
    else
      (get str ofs = get sub ofs_sub)
      && unsafe_sub_equal str (ofs + 1) sub (ofs_sub + 1)

  let starts_with ~prefix str=
    if length prefix > length str then
      false
    else
      unsafe_sub_equal str 0 prefix 0

  let make len c=
    let chars= Array.make len c in
    let width= calc_width chars
    and size= calc_size' chars in
    { chars; width; size }

  let append s1 s2=
    let chars= Array.append s1.chars s2.chars
    and width= add_width s1.width s2.width
    and size= s1.size + s2.size in
    { chars; width; size }

  let ends_with str ends=
    let len_s= length str
    and len_e= length ends in
    if len_s >= len_e then
      let start= len_s - len_e in
      compare (sub str start len_e) ends = 0
    else false

  module US0(US:UnicodeString.Type) = struct
    module Convert = Zed_utils.Convert(US)
    let of_t t= t.chars |> chars_to_raw_list |> Convert.of_list
    let to_t us=
      let len= US.length us in
      let rec create i=
        if i < len
        then US.get us i :: create (i+1)
        else []
      in
      let uChars= create 0 in
      of_uChars uChars
    let to_t_exn us= let t,_= to_t us in t
  end

  module Buf0 = struct
    type buf= {
      mutable buffer: Zed_char.t array;
      mutable position: int;
      mutable length: int;
      initial_buffer: Zed_char.t array;
    }
    type t= buf

    let resize buf more =
      let len = buf.length in
      let new_len = ref len in
      if more <> 0 then begin
        if more > 0 then
          while buf.position + more > !new_len do
            new_len := 2 * !new_len
          done
        else
          while buf.position + more < !new_len / 2 && !new_len >= 2 do
            new_len := !new_len / 2
          done;
        if !new_len > Sys.max_array_length then begin
          if buf.position + more <= Sys.max_array_length
          then new_len := Sys.max_array_length
          else failwith "Buf.resize: cannot grow buffer"
        end;
        let new_buffer = Array.make !new_len Zed_char.zero in
        Array.blit buf.buffer 0 new_buffer 0 buf.position;
        buf.buffer <- new_buffer;
        buf.length <- !new_len
      end

    let create n=
      let n= if n < 1 then 1 else n in
      let n=
        if n > Sys.max_array_length
        then Sys.max_array_length
        else n in
      let buffer= Array.make n Zed_char.zero in
      let initial_buffer= buffer in
      { buffer; position= 0; length= n; initial_buffer }

    let contents b=
      let chars= Array.sub b.buffer 0 b.position in
      let width= calc_width chars
      and size= calc_size' chars in
      { chars; width; size }

    let contents_len b= b.position

    let clear b= b.position <- 0

    let reset b=
      b.position <- 0;
      b.buffer <- b.initial_buffer;
      b.length <- Array.length b.buffer

    let add_zChar b zChar=
      if b.position >= b.length then resize b 1;
      b.buffer.(b.position) <- zChar;
      b.position <- b.position + 1

    let add_uChar b uChar=
      if b.position > 0 then
        let zChar= b.buffer.(b.position-1) in
        match Zed_char.mix_uChar zChar uChar with
        | Ok zChar-> b.buffer.(b.position-1) <- zChar
        | Error new_char->
          if b.position >= b.length then resize b 1;
          b.buffer.(b.position) <- new_char;
          b.position <- b.position + 1
      else
        match Zed_char.of_uChars [uChar] with
        | Some new_char, _->
          if b.position >= b.length then resize b 1;
          b.buffer.(b.position) <- new_char;
          b.position <- b.position + 1
        | None, _-> ()

    let add_string b s=
      let length= Array.length s.chars in
      let new_pos= b.position + length in
      if new_pos > b.length then resize b length;
      Array.blit s.chars 0 b.buffer b.position length;
      b.position <- b.position + length

    let add_buffer b1 b2=
      let more= b1.position + b2.position - b1.length in
      if more > 0 then resize b1 more;
      Array.blit b2.buffer 0 b1.buffer b1.position b2.position;
      b1.position <- b1.position + b2.position
  end
end

module US_Core = struct
  include Zed_string0

  let get t i= Zed_char.core t.chars.(i)
  let init= init_from_uChars
  let iter f t= Array.iter
    (fun zChar-> f (Zed_char.core zChar))
    t.chars
  let compare t1 t2= Zed_utils.array_compare
    ~compare:Zed_char.compare_core
    t1.chars t2.chars

  let to_list t= t.chars
    |> Array.to_list
    |> List.map Zed_char.core

  let to_array t= t.chars
    |> Array.map Zed_char.core

  let charsL_to_list t= t
    |> List.map Zed_char.core

  let charsL_to_array t= t
    |> List.map Zed_char.core
    |> Array.of_list

  let chars_to_list t= t
    |> Array.to_list
    |> List.map Zed_char.core

  let chars_to_array t= t
    |> Array.map Zed_char.core


  module US(US:UnicodeString.Type) = struct
    module Convert = Zed_utils.Convert(US)
    let of_t t= t.chars
      |> Array.map Zed_char.core
      |> Convert.of_array
  end
  module Buf = struct
    include Buf0
    let add_char= add_uChar
  end
end

module US_Raw = struct
  type t= Zed_string0.t
  let memory_get ()=
    let open Zed_string0 in
    let last= ref (empty ())
    and cache= ref [||] in
    let get t i=
      if t != !last then
        begin
          last:= t;
          cache:= chars_to_raw_array t.chars
        end;
      !cache.(i)
    in
    get

  let get= Zed_string0.get_raw (* memory_get () *)
  let init= Zed_string0.init_from_uChars
  let length t= Array.fold_left (+) 0 (Array.map Zed_char.length t.Zed_string0.chars)

  type index= int * int

  let check_range t n= n >= 0 && n < t.Zed_string0.size
  let out_of_range t (i,o)=
    if i < 0 || i >= Array.length t.Zed_string0.chars then
      true
    else
      Zed_char.out_of_range t.chars.(i) o

  let look t (i,o)= Zed_char.get t.Zed_string0.chars.(i) o

  let nth t o=
    let rec nth i n=
      let size= Zed_char.size t.Zed_string0.chars.(i) in
      if n < size then
        (i, o)
      else
        nth (i + 1) (o - size)
    in
    nth 0 o

  let next t (i, o)=
    let size= Zed_char.size t.Zed_string0.chars.(i) in
    if o+1 < size then
      (i, o+1)
    else
      (i+1, 0)

  let prev t (i, o)=
    if o-1 > 0 then
      (i, o-1)
    else
      let i= i - 1 in
      let size= Zed_char.size t.Zed_string0.chars.(i) in
      (i, size - 1)

  let first _= (0, 0)
  let last t=
    let i= Array.length t.Zed_string0.chars - 1 in
    let o=
      if i >= 0
      then Zed_char.size t.chars.(i) - 1
      else 0
    in
    (i, o)

  let move t (i, o) n=
    let rec move (i, o) n=
      let size= Zed_char.size t.Zed_string0.chars.(i) in
      if o + n >= size then
        move (i+1, 0) (n - (size-o))
      else if o + n < 0 then
        let size= Zed_char.size t.Zed_string0.chars.(i-1) in
        move (i-1, size-1) (o + n)
      else
        (i, o+n)
    in
    move (i, o) n

  let compare_index _ (i1,o1) (i2,o2)=
    let r= compare  i1 i2 in
    if r <> 0
    then r
    else compare o1 o2

  let iter f t= Array.iter
    (Zed_char.iter f)
    t.Zed_string0.chars


  let compare t1 t2=
    let open Zed_string0 in
    Zed_utils.list_compare
      ~compare:UChar.compare
      (chars_to_raw_list t1.chars) (chars_to_raw_list t2.chars)

  module US = Zed_string0.US0
  module Buf = struct
    include Zed_string0.Buf0
    let add_char= add_uChar
  end
end

include Zed_string0
module US = US0
module Buf = Buf0

