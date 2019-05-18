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
  * module Text is divided into there modules, Text, Text\_core, Text\_raw, to manager Zed\_rope by Zed\_char.t, the core UChar.t of a `Zed_char.t` and raw `UChar.t`, respectively
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
