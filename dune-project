(lang dune 2.8)

(generate_opam_files true)

(authors "Joel Bjornson <joel.bjornson@gmail.com>")

(license ISC)

(maintainers "Joel Bjornson <joel.bjornson@gmail.com>")

(source
 (github jobjo/popper))

(bug_reports "https://github.com/jobjo/popper/issues")

(homepage "https://github.com/jobjo/popper/issues")

(documentation "https://github.com/jobjo/popper/issues")

(package
 (name popper)
 (synopsis "Testing in the spirit of Karl Popper")
 (description "An OCaml library for unit and property-based testing.")
 (depends
  pringo
  (ocaml
   (>= 4.08.1))
  dune))

(package
 (name ppx_deriving_popper)
 (synopsis "A ppx for deriving samplers and comparators")
 (description
  "This ppx derives popper sampler and comparator functions for custom data types")
 (depends
  ppx_deriving
  (ocaml
   (>= 4.08.1))
  dune))
