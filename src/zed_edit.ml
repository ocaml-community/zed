(*
 * zed_edit.ml
 * -----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Zed, an editor engine.
*)

open React

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

module Clipboard = struct
  type t =
    { get : unit -> Zed_rope.t
    ; set : Zed_rope.t -> unit
    }

  let create () =
    let r = ref Zed_rope.empty in
    { get = (fun () -> !r)
    ; set = (fun x -> r := x)
    }
end

type 'a t = {
  mutable data : 'a option;
  (* Custom data attached to the engine. *)

  mutable text : Zed_rope.t;
  (* The contents of the engine. *)

  mutable lines : Zed_lines.t;
  (* The set of line position of [text]. *)

  changes : (int * int * int) event;
  send_changes : (int * int * int) -> unit;
  (* Changes of the contents. *)

  erase_mode : bool signal;
  set_erase_mode : bool -> unit;
  (* The current erase mode. *)

  editable : int -> int -> bool;
  (* The editable function of the engine. *)

  clipboard : Clipboard.t;
  (* The clipboard for this engine. *)

  mutable mark : Zed_cursor.t;
  (* The cursor that points to the mark. *)

  selection : bool signal;
  set_selection : bool -> unit;
  (* The current selection state. *)

  match_word : Zed_rope.t -> int -> int option;
  (* The function for matching words. *)

  undo : (Zed_rope.t * Zed_lines.t * int * int * int * int) array;
  (* The undo buffer. It is an array of element of the form [(text,
     lines, pos, new_pos, added, removed)]. *)

  undo_size : int;
  (* Size of the undo buffer. *)

  mutable undo_start : int;
  (* Position of the first used cell in the undo buffer. *)

  mutable undo_index : int;
  (* Position of the next available cell in the undo buffer. *)

  mutable undo_count : int;
  (* Number of used cell in the undo buffer. *)
}

(* +-----------------------------------------------------------------+
   | Creation                                                        |
   +-----------------------------------------------------------------+ *)

let dummy_cursor = Zed_cursor.create 0 E.never (fun () -> Zed_lines.empty) 0 0

let default_match_word =
  let rec loop_start segmenter zip =
    match Zed_rope.Zip.next zip with
    | No_more         -> None
    | Yield (ch, zip) ->
      match Uuseg.add segmenter (`Uchar (Uchar.to_int ch)) with
      | `Await          -> loop_start segmenter zip
      | `Uchar _ | `End -> None
      | `Boundary       -> loop_word segmenter zip ~pos:0 `Await
  and loop_word segmenter zip v ~pos =
    match Uuseg.add segmenter v with
    | `Boundary | `End -> Some pos
    | `Uchar _         -> loop_word segmenter zip `Await ~pos:(pos + 1)
    | `Await           ->
      match Zed_rope.Zip.next zip with
      | No_more         -> Some pos
      | Yield (ch, zip) -> loop_word segmenter zip (`Uchar (Uchar.to_int ch)) ~pos
  in
  fun rope idx ->
    let zip = Zed_rope.Zip.make_f rope idx in
    loop_start (Uuseg.create `Word) zip
;;

let create ?(editable=fun pos len -> true) ?clipboard ?(undo_size = 1000) () =
  let changes, send_changes = E.create () in
  let erase_mode, set_erase_mode = S.create false in
  let selection, set_selection = S.create false in
  let clipboard =
    match clipboard with
    | Some clipboard ->
      clipboard
    | None ->
      Clipboard.create ()
  in
  let rec edit =
    { data  = None
    ; text  = Zed_rope.empty
    ; lines = Zed_lines.empty
    ; changes
    ; send_changes
    ; erase_mode
    ; set_erase_mode
    ; editable
    ; clipboard
    ; mark = dummy_cursor
    ; selection
    ; set_selection
    (* TODO: make this user-configurable again *)
    ; match_word = default_match_word
    ; undo = Array.make undo_size (Zed_rope.empty, Zed_lines.empty, 0, 0, 0, 0)
    ; undo_size
    ; undo_start = 0
    ; undo_index = 0
    ; undo_count = 0
    }
  in
  edit.mark <- Zed_cursor.create 0 changes (fun () -> edit.lines) 0 0;
  edit

(* +-----------------------------------------------------------------+
   | State                                                           |
   +-----------------------------------------------------------------+ *)

let get_data engine =
  match engine.data with
  | Some data -> data
  | None -> raise Not_found
let set_data engine data = engine.data <- Some data
let clear_data engine = engine.data <- None
let text engine = engine.text
let lines engine = engine.lines
let changes engine = engine.changes
let erase_mode engine = engine.erase_mode
let get_erase_mode engine = S.value engine.erase_mode
let set_erase_mode engine state = engine.set_erase_mode state
let mark engine = engine.mark
let selection engine = engine.selection
let get_selection engine = S.value engine.selection
let set_selection engine state = engine.set_selection state

let get_line e i =
  let txt = text e in
  let lines = lines e in
  let start = Zed_lines.line_start lines i in
  let stop = Zed_lines.line_stop lines i in
  Zed_rope.sub txt start (stop - start)

let update engine cursors =
  E.select (
    E.stamp engine.changes ()
    :: E.stamp (S.changes engine.selection) ()
    :: E.stamp (S.changes (Zed_cursor.position engine.mark)) ()
    :: List.map (fun cursor -> E.stamp (S.changes (Zed_cursor.position cursor)) ()) cursors
  )

(* +-----------------------------------------------------------------+
   | Cursors                                                         |
   +-----------------------------------------------------------------+ *)

let new_cursor engine =
  Zed_cursor.create (Zed_rope.length engine.text) engine.changes (fun () -> engine.lines) 0 0

(* +-----------------------------------------------------------------+
   | Actions                                                         |
   +-----------------------------------------------------------------+ *)

exception Cannot_edit

type 'a context = {
  edit : 'a t;
  cursor : Zed_cursor.t;
  check : bool;
}

let context ?(check=true) edit cursor =
  { edit; cursor; check }

let edit ctx = ctx.edit
let cursor ctx = ctx.cursor
let check ctx = ctx.check

let with_check check ctx = { ctx with check }

let goto ctx ?set_wanted_column new_position =
  Zed_cursor.goto ctx.cursor ?set_wanted_column new_position

let move ctx ?set_wanted_column delta =
  Zed_cursor.move ctx.cursor ?set_wanted_column delta

let next_line_n ctx n =
  let index = Zed_cursor.get_line ctx.cursor in
  if index + n > Zed_lines.count ctx.edit.lines then
    goto ctx ~set_wanted_column:false (Zed_rope.length ctx.edit.text)
  else begin
    let start = Zed_lines.line_start ctx.edit.lines (index + n) in
    let stop =
      if index + n = Zed_lines.count ctx.edit.lines then
        Zed_rope.length ctx.edit.text
      else
        Zed_lines.line_start ctx.edit.lines (index + n + 1) - 1
    in
    goto ctx ~set_wanted_column:false (start + min (Zed_cursor.get_wanted_column ctx.cursor) (stop - start))
  end

let prev_line_n ctx n =
  let index = Zed_cursor.get_line ctx.cursor in
  if index - n < 0 then begin
    goto ctx ~set_wanted_column:false 0
  end else begin
    let start = Zed_lines.line_start ctx.edit.lines (index - n) in
    let stop = Zed_lines.line_start ctx.edit.lines (index - (n - 1)) - 1 in
    goto ctx ~set_wanted_column:false (start + min (Zed_cursor.get_wanted_column ctx.cursor) (stop - start))
  end

let move_line ctx delta =
  match delta with
  | _ when delta < 0 ->
    prev_line_n ctx (-delta)
  | _ when delta > 0 ->
    next_line_n ctx delta
  | _ ->
    ()

let position ctx =
  Zed_cursor.get_position ctx.cursor

let line ctx =
  Zed_cursor.get_line ctx.cursor

let column ctx =
  Zed_cursor.get_column ctx.cursor

let at_bol ctx =
  Zed_cursor.get_column ctx.cursor = 0

let at_eol ctx =
  let position = Zed_cursor.get_position ctx.cursor in
  let index = Zed_cursor.get_line ctx.cursor in
  if index = Zed_lines.count ctx.edit.lines then
    position = Zed_rope.length ctx.edit.text
  else
    position = Zed_lines.line_start ctx.edit.lines (index + 1) - 1

let at_bot ctx =
  Zed_cursor.get_position ctx.cursor = 0

let at_eot ctx =
  Zed_cursor.get_position ctx.cursor = Zed_rope.length ctx.edit.text

let modify { edit } text lines position new_position added removed =
  if edit.undo_size > 0 then begin
    edit.undo.(edit.undo_index) <- (text, lines, position, new_position, added, removed);
    edit.undo_index <- (edit.undo_index + 1) mod edit.undo_size;
    if edit.undo_count = edit.undo_size then
      edit.undo_start <- (edit.undo_start + 1) mod edit.undo_size
    else
      edit.undo_count <- edit.undo_count + 1
  end;
  edit.send_changes (position, added, removed)

let insert ctx rope =
  let position = Zed_cursor.get_position ctx.cursor in
  if not ctx.check || ctx.edit.editable position 0 then begin
    let len = Zed_rope.length rope in
    let text = ctx.edit.text and lines = ctx.edit.lines in
    if S.value ctx.edit.erase_mode then begin
      let text_len = Zed_rope.length ctx.edit.text in
      if position + len > text_len then begin
        ctx.edit.text <- Zed_rope.replace text position (text_len - position) rope;
        ctx.edit.lines <- Zed_lines.replace ctx.edit.lines position (text_len - position) (Zed_lines.of_rope rope);
        modify ctx text lines position position len (text_len - position)
      end else begin
        ctx.edit.text <- Zed_rope.replace text position len rope;
        ctx.edit.lines <- Zed_lines.replace ctx.edit.lines position len (Zed_lines.of_rope rope);
        modify ctx text lines position position len len;
      end;
      move ctx len
    end else begin
      ctx.edit.text <- Zed_rope.insert ctx.edit.text position rope;
      ctx.edit.lines <- Zed_lines.insert ctx.edit.lines position (Zed_lines.of_rope rope);
      modify ctx text lines position position len 0;
      move ctx len
    end
  end else
    raise Cannot_edit

let insert_no_erase ctx rope =
  let position = Zed_cursor.get_position ctx.cursor in
  if not ctx.check || ctx.edit.editable position 0 then begin
    let len = Zed_rope.length rope and text = ctx.edit.text and lines = ctx.edit.lines in
    ctx.edit.text <- Zed_rope.insert text position rope;
    ctx.edit.lines <- Zed_lines.insert ctx.edit.lines position (Zed_lines.of_rope rope);
    modify ctx text lines position position len 0;
    move ctx len
  end else
    raise Cannot_edit

let remove_next ctx len =
  let position = Zed_cursor.get_position ctx.cursor in
  let text_len = Zed_rope.length ctx.edit.text in
  let len = if position + len > text_len then text_len - position else len in
  if not ctx.check || ctx.edit.editable position len then begin
    let text = ctx.edit.text and lines = ctx.edit.lines in
    ctx.edit.text <- Zed_rope.remove text position len;
    ctx.edit.lines <- Zed_lines.remove ctx.edit.lines position len;
    modify ctx text lines position position 0 len;
  end else
    raise Cannot_edit

let remove_prev ctx len =
  let position = Zed_cursor.get_position ctx.cursor in
  let len = min position len in
  if not ctx.check || ctx.edit.editable (position - len) len then begin
    let text = ctx.edit.text and lines = ctx.edit.lines in
    ctx.edit.text <- Zed_rope.remove text (position - len) len;
    ctx.edit.lines <- Zed_lines.remove ctx.edit.lines (position - len) len;
    modify ctx text lines (position - len) position 0 len;
  end else
    raise Cannot_edit

let remove = remove_next

let replace ctx len rope =
  let position = Zed_cursor.get_position ctx.cursor in
  let text_len = Zed_rope.length ctx.edit.text in
  let len = if position + len > text_len then text_len - position else len in
  if not ctx.check || ctx.edit.editable position len then begin
    let rope_len = Zed_rope.length rope and text = ctx.edit.text and lines = ctx.edit.lines in
    ctx.edit.text <- Zed_rope.replace text position len rope;
    ctx.edit.lines <- Zed_lines.replace ctx.edit.lines position len (Zed_lines.of_rope rope);
    modify ctx text lines position position rope_len len;
    move ctx rope_len
  end else
    raise Cannot_edit

let newline_rope =
  Zed_rope.singleton (Uchar.of_char '\n')

let newline ctx =
  insert ctx newline_rope

let next_char ctx =
  if not (at_eot ctx) then move ctx 1

let prev_char ctx =
  if not (at_bot ctx) then move ctx (-1)

let next_line ctx =
  let index = Zed_cursor.get_line ctx.cursor in
  if index = Zed_lines.count ctx.edit.lines then
    goto ctx ~set_wanted_column:false (Zed_rope.length ctx.edit.text)
  else begin
    let start = Zed_lines.line_start ctx.edit.lines (index + 1) in
    let stop =
      if index + 1 = Zed_lines.count ctx.edit.lines then
        Zed_rope.length ctx.edit.text
      else
        Zed_lines.line_start ctx.edit.lines (index + 2) - 1
    in
    goto ctx ~set_wanted_column:false (start + min (Zed_cursor.get_wanted_column ctx.cursor) (stop - start))
  end

let prev_line ctx =
  let index = Zed_cursor.get_line ctx.cursor in
  if index = 0 then begin
    goto ctx ~set_wanted_column:false 0
  end else begin
    let start = Zed_lines.line_start ctx.edit.lines (index - 1) in
    let stop = Zed_lines.line_start ctx.edit.lines index - 1 in
    goto ctx ~set_wanted_column:false (start + min (Zed_cursor.get_wanted_column ctx.cursor) (stop - start))
  end

let goto_bol ctx =
  goto ctx (Zed_lines.line_start ctx.edit.lines (Zed_cursor.get_line ctx.cursor))

let goto_eol ctx =
  let index = Zed_cursor.get_line ctx.cursor in
  if index = Zed_lines.count ctx.edit.lines then
    goto ctx (Zed_rope.length ctx.edit.text)
  else
    goto ctx (Zed_lines.line_start ctx.edit.lines (index + 1) - 1)

let goto_bot ctx =
  goto ctx 0

let goto_eot ctx =
  goto ctx (Zed_rope.length ctx.edit.text)

let delete_next_char ctx =
  if not (at_eot ctx) then begin
    ctx.edit.set_selection false;
    remove_next ctx 1
  end

let delete_prev_char ctx =
  if not (at_bot ctx) then begin
    ctx.edit.set_selection false;
    remove_prev ctx 1
  end

let delete_next_line ctx =
  ctx.edit.set_selection false;
  let position = Zed_cursor.get_position ctx.cursor in
  let index = Zed_cursor.get_line ctx.cursor in
  if index = Zed_lines.count ctx.edit.lines then
    remove_next ctx (Zed_rope.length ctx.edit.text - position)
  else
    remove_next ctx (Zed_lines.line_start ctx.edit.lines (index + 1) - position)

let delete_prev_line ctx =
  ctx.edit.set_selection false;
  let position = Zed_cursor.get_position ctx.cursor in
  let start = Zed_lines.line_start ctx.edit.lines (Zed_cursor.get_line ctx.cursor) in
  remove_prev ctx (position - start)

let kill_next_line ctx =
  let position = Zed_cursor.get_position ctx.cursor in
  let index = Zed_cursor.get_line ctx.cursor in
  if index = Zed_lines.count ctx.edit.lines then begin
    ctx.edit.clipboard.set (Zed_rope.after ctx.edit.text position);
    ctx.edit.set_selection false;
    remove ctx (Zed_rope.length ctx.edit.text - position)
  end else begin
    let len = Zed_lines.line_start ctx.edit.lines (index + 1) - position in
    ctx.edit.clipboard.set (Zed_rope.sub ctx.edit.text position len);
    ctx.edit.set_selection false;
    remove ctx len
  end

let kill_prev_line ctx =
  let position = Zed_cursor.get_position ctx.cursor in
  let start = Zed_lines.line_start ctx.edit.lines (Zed_cursor.get_line ctx.cursor) in
  ctx.edit.clipboard.set (Zed_rope.sub ctx.edit.text start (position - start));
  ctx.edit.set_selection false;
  remove_prev ctx (position - start)

let switch_erase_mode ctx =
  ctx.edit.set_erase_mode (not (S.value ctx.edit.erase_mode))

let set_mark ctx =
  Zed_cursor.goto ctx.edit.mark (Zed_cursor.get_position ctx.cursor);
  ctx.edit.set_selection true

let goto_mark ctx =
  goto ctx (Zed_cursor.get_position ctx.edit.mark)

let copy ctx =
  if S.value ctx.edit.selection then begin
    let a = Zed_cursor.get_position ctx.cursor and b = Zed_cursor.get_position ctx.edit.mark in
    let a = min a b and b = max a b in
    ctx.edit.clipboard.set (Zed_rope.sub ctx.edit.text a (b - a));
    ctx.edit.set_selection false
  end

let kill ctx =
  if S.value ctx.edit.selection then begin
    let a = Zed_cursor.get_position ctx.cursor and b = Zed_cursor.get_position ctx.edit.mark in
    let a = min a b and b = max a b in
    ctx.edit.clipboard.set (Zed_rope.sub ctx.edit.text a (b - a));
    ctx.edit.set_selection false;
    goto ctx a;
    let a = Zed_cursor.get_position ctx.cursor in
    if a <= b then remove ctx (b - a)
  end

let yank ctx =
  ctx.edit.set_selection false;
  insert ctx (ctx.edit.clipboard.get ())

let search_word_forward ctx =
  let len = Zed_rope.length ctx.edit.text in
  let rec loop idx =
    if idx = len then
      None
    else
      match ctx.edit.match_word ctx.edit.text idx with
      | Some idx' ->
        Some(idx, idx')
      | None ->
        loop (idx + 1)
  in
  loop (Zed_cursor.get_position ctx.cursor)

let search_word_backward ctx =
  let rec loop idx =
    if idx = -1 then
      None
    else
      match ctx.edit.match_word ctx.edit.text idx with
      | Some idx' ->
        loop2 (idx - 1) (idx, idx')
      | None ->
        loop (idx - 1)
  and loop2 idx result =
    if idx = -1 then
      Some result
    else
      match ctx.edit.match_word ctx.edit.text idx with
      | Some idx' ->
        loop2 (idx - 1) (idx, idx')
      | None ->
        Some result
  in
  loop (Zed_cursor.get_position ctx.cursor - 1)

let case_map s ~f =
  let buf = Buffer.create (String.length s) in
  Zed_utf8.iter (fun c ->
    match f (Uchar.to_int c) with
    | `Self -> Zed_utf8.add buf c
    | `Uchars us -> List.iter (fun c -> Zed_utf8.add buf (Uchar.of_int c)) us)
    s;
  Buffer.contents buf

let replace_word ctx ~f =
  match search_word_forward ctx with
  | Some (idx1, idx2) ->
    goto ctx idx1;
    if Zed_cursor.get_position ctx.cursor = idx1 && idx1 < idx2 then begin
      let str = Zed_rope.sub ctx.edit.text idx1 (idx2 - idx1) in
      replace
        ctx
        (Zed_rope.length str)
        (Zed_rope.of_string (f (Zed_rope.to_string str)))
    end
  | None ->
    ()

let capitalize_word ctx =
  replace_word ctx ~f:(fun str ->
    let first, rest = Zed_utf8.break str 1 in
    case_map first ~f:Uucp.Case.Map.to_upper ^
    case_map rest  ~f:Uucp.Case.Map.to_lower)

let lowercase_word ctx =
  replace_word ctx ~f:(fun str -> case_map str ~f:Uucp.Case.Map.to_lower)

let uppercase_word ctx =
  replace_word ctx ~f:(fun str -> case_map str ~f:Uucp.Case.Map.to_upper)

let next_word ctx =
  match search_word_forward ctx with
  | Some(idx1, idx2) ->
    goto ctx idx2
  | None ->
    ()

let prev_word ctx =
  match search_word_backward ctx with
  | Some(idx1, idx2) ->
    goto ctx idx1
  | None ->
    ()

let delete_next_word ctx =
  let position = Zed_cursor.get_position ctx.cursor in
  match search_word_forward ctx with
  | Some(idx1, idx2) ->
    remove ctx (idx2 - position)
  | None ->
    ()

let delete_prev_word ctx =
  let position = Zed_cursor.get_position ctx.cursor in
  match search_word_backward ctx with
  | Some(idx1, idx2) ->
    remove_prev ctx (position - idx1)
  | None ->
    ()

let kill_next_word ctx =
  let position = Zed_cursor.get_position ctx.cursor in
  match search_word_forward ctx with
  | Some(idx1, idx2) ->
    ctx.edit.clipboard.set (Zed_rope.sub ctx.edit.text position (idx2 - position));
    ctx.edit.set_selection false;
    remove ctx (idx2 - position)
  | None ->
    ()

let kill_prev_word ctx =
  let position = Zed_cursor.get_position ctx.cursor in
  match search_word_backward ctx with
  | Some(idx1, idx2) ->
    ctx.edit.clipboard.set (Zed_rope.sub ctx.edit.text idx1 (position - idx1));
    ctx.edit.set_selection false;
    remove_prev ctx (position - idx1)
  | None ->
    ()

let undo { check; edit; cursor } =
  if edit.undo_count > 0 then begin
    let index =
      if edit.undo_index = 0 then
        edit.undo_size - 1
      else
        edit.undo_index - 1
    in
    let text, lines, pos, new_pos, added, removed = edit.undo.(index) in
    if not check || edit.editable pos added then begin
      edit.undo_count <- edit.undo_count - 1;
      edit.undo_index <- index;
      edit.text <- text;
      edit.lines <- lines;
      edit.send_changes (pos, removed, added);
      Zed_cursor.goto cursor new_pos
    end else
      raise Cannot_edit
  end

(* +-----------------------------------------------------------------+
   | Actions                                                         |
   +-----------------------------------------------------------------+ *)

type a_context = C : _ context -> a_context
type 'a for_all_contexts = { f : 'c. 'c context -> 'a }

let actions =
  let open Zed_actions in
  let open Args in
  let mk name ~doc args impl =
    Action.make name ~doc args (fun (C context) -> impl.f context)
  in
  add empty
    [ mk "insert" (Zed_actions.Arg.Rope +> nil) { f = insert }
        ~doc:"insert the given character"
    ; mk "newline" nil { f = newline }
        ~doc:"insert a newline character"
    ; mk "next-char" nil { f = next_char }
        ~doc:"move the cursor to the next character."
    ; mk "prev-char" nil { f = prev_char }
        ~doc:"move the cursor to the previous character."
    ; mk "next-line" nil { f = next_line }
        ~doc:"move the cursor to the next line."
    ; mk "prev-line" nil { f = prev_line }
        ~doc:"move the cursor to the previous line."
    ; mk "goto-bol" nil { f = goto_bol }
        ~doc:"move the cursor to the beginning of the current line."
    ; mk "goto-eol" nil { f = goto_eol }
        ~doc:"move the cursor to the end of the current line."
    ; mk "goto-bot" nil { f = goto_bot }
        ~doc:"move the cursor to the beginning of the text."
    ; mk "goto-eot" nil { f = goto_eot }
        ~doc:"move the cursor to the end of the text."
    ; mk "delete-next-char" nil { f = delete_next_char }
        ~doc:"delete the character after the cursor."
    ; mk "delete-prev-char" nil { f = delete_prev_char }
        ~doc:"delete the character before the cursor."
    ; mk "delete-next-line" nil { f = delete_next_line }
        ~doc:"delete everything until the end of the current line."
    ; mk "delete-prev-line" nil { f = delete_prev_line }
        ~doc:"delete everything until the beginning of the current line."
    ; mk "kill-next-line" nil { f = kill_next_line }
        ~doc:"cut everything until the end of the current line."
    ; mk "kill-prev-line" nil { f = kill_prev_line }
        ~doc:"cut everything until the beginning of the current line."
    ; mk "switch-erase-mode" nil { f = switch_erase_mode }
        ~doc:"switch the current erasing mode."
    ; mk "set-mark" nil { f = set_mark }
        ~doc:"set the mark to the current position."
    ; mk "goto-mark" nil { f = goto_mark }
        ~doc:"move the cursor to the mark."
    ; mk "copy" nil { f = copy }
        ~doc:"copy the current region to the clipboard."
    ; mk "kill" nil { f = kill }
        ~doc:"cut the current region to the clipboard."
    ; mk "yank" nil { f = yank }
        ~doc:"paste the contents of the clipboard at current position."
    ; mk "capitalize-word" nil { f = capitalize_word }
        ~doc:"capitalize the first word after the cursor."
    ; mk "lowercase-word" nil { f = lowercase_word }
        ~doc:"convert the first word after the cursor to lowercase."
    ; mk "uppercase-word" nil { f = uppercase_word }
        ~doc:"convert the first word after the cursor to uppercase."
    ; mk "next-word" nil { f = next_word }
        ~doc:"move the cursor to the end of the next word."
    ; mk "prev-word" nil { f = prev_word }
        ~doc:"move the cursor to the beginning of the previous word."
    ; mk "delete-next-word" nil { f = delete_next_word }
        ~doc:"delete up until the next non-word character."
    ; mk "delete-prev-word" nil { f = delete_prev_word }
        ~doc:"delete the word behind the cursor."
    ; mk "kill-next-word" nil { f = kill_next_word }
        ~doc:"cut up until the next non-word character."
    ; mk "kill-prev-word" nil { f = kill_prev_word }
        ~doc:"cut the word behind the cursor."
    ; mk "undo" nil { f = undo }
        ~doc:"revert the last action."
    ]

let parse_insert x =
  if Zed_utf8.starts_with x "insert(" && Zed_utf8.ends_with x ")" then begin
    let str = String.sub x 7 (String.length x - 8) in
    if String.length str = 1 && Char.code str.[0] < 128 then
      Some (Zed_rope.singleton (Uchar.of_char str.[0]))
    else if String.length str > 2 && str.[0] = 'U' && str.[1] = '+' then
      let acc = ref 0 in
      for i = 2 to String.length str - 1 do
        let ch = str.[i] in
        acc := !acc * 16 + (match ch with
          | '0' .. '9' -> Char.code ch - Char.code '0'
          | 'a' .. 'f' -> Char.code ch - Char.code 'a' + 10
          | 'A' .. 'F' -> Char.code ch - Char.code 'A' + 10
          | _ -> raise Not_found)
      done;
      match Uchar.of_int !acc with
      | c -> Some (Zed_rope.singleton c)
      | exception _ -> None
    else
      None
  end else
    None
