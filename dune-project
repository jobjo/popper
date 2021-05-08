(lang dune 2.8)

(generate_opam_files true)

(name popper)

(version 0.1.0)

(source
 (github jobjo/popper))

(authors "Joel Bjornson <joel.bjornson@gmail.com>")

(license ISC)

(maintainers "Joel Bjornson <joel.bjornson@gmail.com>")

(bug_reports "https://github.com/jobjo/popper/issues")

(homepage "https://github.com/jobjo/popper/issues")

(documentation "https://github.com/jobjo/popper/issues")

(package
 (name popper)
 (synopsis "Popperty-based testing")
 (description
  "Popper (after Karl) is an OCaml library for writing regular unit tests as well as property-based tests. The design is inspired by the Python library Hypothesis.")
 (depends
  pringo
  (ocaml
   (>= 4.09.1))
  dune))

(package
 (name ppx_deriving_popper)
 (synopsis
  "A ppx for deriving sample and comparator functions to be used with Popper")
 (description
  "This ppx derives popper sampler and comparator functions for custom data types.")
 (depends
  ppx_deriving
  popper
  (ocaml
   (>= 4.09.1))
  dune))
