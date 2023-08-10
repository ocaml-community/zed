3.2.3 (2023-08-10)
------------------
* `Zed_edit`: fix the `Next_word` and `Prev_word` actions

3.2.2 (2023-06-20)
------------------

* `Zed_utf8.next_error`: raise `Zed_utf8.Out_of_bounds` in case of invalid offset (@Lucccyo, #52)
* `kill_next_word` should not raise `Out_of_bound` (@Lucccyo, #55)
* `of_utf8`: add `Uchar.is_valid` to check the input (@Lucccyo, #51)

3.2.1 (2022-11-10)
------------------

* Check if decoded values fit in Uchar (Etienne Millon, #50)

3.2.0 (2022-06-30)
------------------

* Replace Camomile with uu* (Nicolás Ojeda Bär, ZAN DoYe, Thibaut Mattio, #46)

3.1.0 (2020-05-30)
------------------

* `Zed_edit`
  * `Set_pos` action
  * `Insert_str` action

3.0.1 (2020-04-28)
------------------

* `Zed_edit`: fix `copy_sequence`

3.0.0 (2020-04-25)
------------------

* `Zed_edit`:
  * new actions
    * `Join_line`
    * `Goto of int`
    * `Delete_next_chars of int`
    * `Delete_prev_chars of int`
    * `Kill_next_chars of int`
    * `Kill_prev_chars of int`
  * function `copy_sequence`

2.0.7 (2020-04-08)
------------------

* fix Zed\_edit.undo (#36)

2.0.6 (2020-02-27)
------------------

* compabile with `Result` (>= 1.5) (@mjambon, #31)


2.0.5 (2020-01-29)
------------------

* Zed\_rope.Zip: fix a bug in function `make_b`

2.0.4 (2019-12-31)
------------------

* add wanted\_column support for wide width character
* Zed\_lines: `get_idx_by_width set row column_width` return the offset of the character at `[row, column_width]`

2.0.3 (2019-08-09)
------------------

* Zed\_string
  * `exception Invalid of string * string` raised when an invalid Zed\_char sequence is encounted
  * `next_ofs : t -> int -> int` returns the offset of the next zchar in `t`
  * `prev_ofs : t -> int -> int` returns the offset of the prev zchar in `t`

2.0.2 (2019-06-21)
------------------

* Zed\_utf8: fix an ofs-stepping bug in function `unsafe_extract_prev`

2.0.1 (2019-06-04)
------------------

* Zed\_char: add an `indv_combining` option to the transforming
  functions(`of_uChars, zChars_of_uChars, of_utf8`) to determine whether
  to extract individual combining marks from the parameter (#18)
* Zed\_char: clarify some documentation comments (#18)

2.0 (2019-05-17)
----------------

### Additions

* module Zed\_char
* module Zed\_string
* Zed\_cursor
  * `column_display: Zed_cursor.t -> int React.signal`
  * `get_column: Zed_cursor.t -> int`
  * `coordinates_display: Zed_cursor.t -> (int * int) React.signal`
  * `get_coordinates: Zed_cursor.t -> int * int`
* Zed\_edit
  * `regexp_word_core: Zed_re.Core.t`
  * `regexp_word_raw: Zed_re.raw.t`
  * `match_by_regexp_core`
  * `match_by_regexp_raw`

### Breaking

* Zed\_rope
  * Zed\_rope.empty is a function now
  * Other functions in this module take `Zed_char.t` or `Zed_string.t` as arguemnts instead of `UChar.t` or `Zed_utf8.t`
  * module Zipper is divided into two modules, Zip and Zip\_raw, to navigate over a rope by Zed\_char.t or UChar.t, respectively
  * module Text is divided into three modules, Text, Text\_core, Text\_raw, to manager Zed\_rope by Zed\_char.t, the core UChar.t of a `Zed_char.t` and raw `UChar.t`, respectively
* Zed\_re is therefore divided into two modules: Core and Raw
* Zed\_cursor: the type `changes` is defined as a structure and has two more fields: `added_width` and `removed_width`

### General

* README: Add Travis badge (Kevin Ji, #11)
* Add travis config (Anurag Soni, #10)
* Switch to dune (Anurag Soni, #9)

1.6 (2017-11-05)
----------------

* safe-string compatibility (#8)

1.5 (2017-04-26)
----------------

* Switch to jbuilder (Rudi Gringberg, #4)
* Make `{delete_,kill_,}{next,prev}_word` consistent near the
  start/end of the buffer (Fabian (github use copy), #5)

1.4 (2015-01-07)
----------------

* added `Zed_edit.get_line`
* added `Zed_line.line_{length,stop}`
* fix a bug in cursor updates
* fix some invalid use of react

1.3 (2014-04-21)
----------------

* `Zed_rope` fixes:
  - `rev_map`: fix recursion
  - enforce evaluation order in `map` & `rev_map`

1.2 (2012-07-30)
----------------

* add escaping functions
* add `Zed_utf8.next_error`

1.1 (2011-08-06)
----------------

* add the `{delete,kill}-{prev,next}-word` actions and functions
* add `Zed_edit.Insert(ch)`
* add `Zed_edit.replace`
* raise an exception when editing a read-only part of a text
* disable the move function
* add support for undo
* add `Zed_input` to ease writing key binders
* add `Zed_macro` to ease writing macro system
* fix `Zed_rope.Zip.sub`
* add `Zed_edit.new_clipboard`
* add `Zed_utf8.add`
