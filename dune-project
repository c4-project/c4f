(lang dune 2.0)
(name act)

; Doesn't work well with the latest jane street, so disabling for now.
; (implicit_transitive_deps false)
(generate_opam_files true)
(formatting (enabled_for dune ocaml))

(using menhir 2.0)

(license MIT)
(maintainers "Matt Windsor <m.windsor@imperial.ac.uk>")
(authors "Matt Windsor <m.windsor@imperial.ac.uk>")
(source (github MattWindsor91/act))
(documentation "https://automagic-compiler-tormentor.readthedocs.io")

(package
  (name act)
  (synopsis "Automagic compiler tormentor")
  (description "'act' is a toolkit for testing memory-model behaviour of compilers.")
  (depends
    (ocaml (and (>= 4.09) (< 4.11)))
    (dune (and (>= 2.0) (< 3)))
    (expect_test_helpers_core (and (>= v0.14) (< v0.15)))
    ppx_deriving
    ppx_jane
    (ppx_yojson_conv (and (>= v0.14) (< v0.15)))
    (core (and (>= v0.14) (< v0.15)))
    (fmt (and (>= 0.8.7) (< 0.9)))
    (fpath (and (>= 0.7.2) (< 0.8)))
    (menhir (>= 20200211))
    (sedlex (and (>= 2.1) (< 3)))
    (shell (and (>= v0.14) (< v0.15)))
    (textutils_kernel (and (>= v0.14) (< v0.15)))
    (travesty (and (>= 0.6.2) (< 0.7)))
    (yojson (and (>= 1.7) (< 2)))
    ))

; Local Variables:
; mode: lisp
; End:
