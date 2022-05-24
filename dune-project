(lang dune 3.2)
(name c4f)

(implicit_transitive_deps false)
(generate_opam_files true)

(using menhir 2.0)

(license MIT)
(maintainers "Matt Windsor <m.windsor@imperial.ac.uk>")
(authors "Matt Windsor <m.windsor@imperial.ac.uk>")
(source (github c4-project/c4f))
(documentation "https://automagic-compiler-tormentor.readthedocs.io")

(package
  (name c4f)
  (synopsis "Metamorphic fuzzer for C litmus tests")
  (description "'c4f' (the 'C Compiler Concurrency Checker' fuzzer) is a toolkit for randomly expanding C litmus tests in a semantics-refining way.")
  (depends
    (ocaml (and (>= 4.14) (< 4.15)))
    (accessor (and (>= v0.15) (< v0.16)))
    (accessor_base (and (>= v0.15) (< v0.16)))
    (ppx_accessor (and (>= v0.15) (< v0.16)))
    (expect_test_helpers_core (and (>= v0.15) (< v0.16)))
    ppx_deriving
    ppx_jane
    (ppx_yojson_conv (and (>= v0.15) (< v0.16)))
    (core (and (>= v0.15) (< v0.16)))
    (core_unix (and (>= v0.15) (< v0.16)))
    (fmt (and (>= 0.9) (< 0.10)))
    (fpath (and (>= 0.7.3) (< 0.8)))
    (menhir (>= 20220210))
    (sedlex (and (>= 3) (< 4)))
    (shell (and (>= v0.15) (< v0.16)))
    (textutils_kernel (and (>= v0.15) (< v0.16)))
    (travesty (and (>= 0.7.2) (< 0.8)))
    (yojson (and (>= 1.7) (< 2)))
    ))

; Local Variables:
; mode: lisp
; End:
