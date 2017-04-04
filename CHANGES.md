1.5
---

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
