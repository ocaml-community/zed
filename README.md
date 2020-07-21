Zed
===

[![Build Status](https://travis-ci.org/ocaml-community/zed.svg?branch=master)](https://travis-ci.org/ocaml-community/zed)

Zed is an abstract engine for text edition. It can be used to write
text editors, edition widgets, readlines, ... You just have to
_connect_ an engine to your inputs and rendering functions to get an
editor.

Zed provides:

* edition state management,
* multiple cursor support,
* key-binding helpers,
* general purpose unicode rope manipulation functions.

[API Documentation](http://ocaml-community.github.io/zed/)

Installation
------------

To build and install zed, use opam:

    $ opam install zed

Modules
-------

* `Zed_edit`: the main module, it defines edition engines.
* `Zed_cursor`: manages cursors. Cursors are automatically updated
  when the text is modified.
* `Zed_lines`: maintains the offsets of beginning of lines.
* `Zed_input`: helpers for implementing key bindings.
* `Zed_macro`: helpers for writing macro systems.
* `Zed_utf8`: general purpose UTF-8 strings manipulation.
* `Zed_rope`: general purpose unicode ropes manipulation.
* `Zed_char`: general purpose unicode characters manipulation.
* `Zed_string`: general purpose unicode strings manipulation.
