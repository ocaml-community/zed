Zed
===

Zed is an abstract engine for text edition. It can be used to write
text editors, edition widgets, readlines, ... You just have to
_connect_ an engine to your inputs and rendering functions to get an
editor.

Zed provides:

* edition state management,
* multiple cursor support,
* key-binding helpers,
* general purpose unicode rope manipulation functions.

Dependencies
------------

* [OCaml](http://caml.inria.fr/ocaml/) (>= 3.12)
* [findlib](http://projects.camlcity.org/projects/findlib.html)
* [Camomile](http://github.com/yoriyuki/Camomile) (>= 0.8)
* [react](http://erratique.ch/software/react)

For building the development version, you also need to install
[oasis](http://oasis.forge.ocamlcore.org/) (>= 0.3.0).

Installation
------------

To build and install zed:

    $ ./configure
    $ make
    $ make install

### Documentation _(optional)_

To build the documentation:

    $ make doc

It will then be installed by `make install`.

### Tests _(optionnal)_

To build and execute tests:

    $ ./configure --enable-tests
    $ make test

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
