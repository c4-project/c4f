(lang dune 2.0)
(name c4f)

(implicit_transitive_deps false)
(generate_opam_files true)
(formatting (enabled_for dune ocaml))

(using menhir 2.0)

(license MIT)
(maintainers "Matt Windsor <m.windsor@imperial.ac.uk>")
(authors "Matt Windsor <m.windsor@imperial.ac.uk>")
(source (github MattWindsor91/c4f))
(documentation "https://automagic-compiler-tormentor.readthedocs.io")

(package
  (name c4f)
  (synopsis "Metamorphic fuzzer for C litmus tests")
  (description "'c4f' (the 'C Compiler Concurrency Checker' fuzzer) is a toolkit for randomly expanding C litmus tests in a semantics-refining way.")
  (depends
    (ocaml (and (>= 4.09) (< 4.11)))
    (accessor (and (>= v0.14) (< v0.15)))
    (accessor_base (and (>= v0.14) (< v0.15)))
    (ppx_accessor (and (>= v0.14) (< v0.15)))
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
