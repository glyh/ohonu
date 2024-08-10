(lang dune 3.15)

(name ohonu)

(using menhir 2.0)

(generate_opam_files true)

(source
 (github glyh/ohonu))

(authors "Yihang Liu")

(maintainers "Yihang Liu")

(license GPL-3.0-or-later)

(documentation https://github.com/glyh/ohonu)

(package
 (name ohonu)
 (synopsis "Minimal Implementation of Honu Macros")
 (description "Minimal Implementation of Honu Macros")
 (depends
   ocaml
   dune
   menhir
   (ppx_inline_test (>= v0.16.1))
   (nice_parser (= 1.0.0))
   (linenoise (>= 1.5.1)))
          
 (tags
  (topics macro hygienic)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
