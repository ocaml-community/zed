(lang dune 3.0)

(name zed)

(generate_opam_files true)

(source
 (github ocaml-community/zed))

(authors "Jérémie Dimino")

(maintainers "opam-devel@lists.ocaml.org")

(license BSD-3-Clause)

(package
 (name zed)
 (synopsis "Abstract engine for text edition in OCaml")
 (description
  "Zed is an abstract engine for text edition. It can be used to write text
editors, edition widgets, readlines, ... Zed uses Camomile to fully support the
Unicode specification, and implements an UTF-8 encoded string type with
validation, and a rope datastructure to achieve efficient operations on large
Unicode buffers. Zed also features a regular expression search on ropes. To
support efficient text edition capabilities, Zed provides macro recording and
cursor management facilities.")
 (depends
  (ocaml
   (>= 4.02.3))
  react
  result
  uchar
  uutf
  (uucp
   (>= 2.0.0))
  uuseg
  (alcotest :with-test)))
