(lang dune 3.6)
(name c4f)

(implicit_transitive_deps false)
(generate_opam_files true)

(using menhir 2.0)

(license MIT)
(maintainers "Matt Windsor <mattwindsor91@gmail.com>")
(authors "Matt Windsor <mattwindsor91@gmail.com>")
(source (github c4-project/c4f))
(documentation "https://automagic-compiler-tormentor.readthedocs.io")

(package
  (name c4f)
  (synopsis "Metamorphic fuzzer for C litmus tests")
  (description "'c4f' (the 'C Compiler Concurrency Checker' fuzzer) is a toolkit for randomly expanding C litmus tests in a semantics-refining way.")
  (depends
    (ocaml (and (>= 5.1) (< 5.2)))
    (accessor (and (>= v0.16) (< v0.17)))
    (accessor_base (and (>= v0.16) (< v0.17)))
    (ppx_accessor (and (>= v0.16) (< v0.17)))
    (expect_test_helpers_core (and (>= v0.16) (< v0.17)))
    ppx_deriving
    ppx_jane
    (ppx_yojson_conv (and (>= v0.16) (< v0.17)))
    (ppx_yojson_conv_lib (and (>= v0.16) (< v0.17)))
    (base (and (>= v0.16) (< v0.17)))
    (core (and (>= v0.16) (< v0.17)))
    (core_unix (and (>= v0.16) (< v0.17)))
    (fmt (and (>= 0.9) (< 0.10)))
    (fpath (and (>= 0.7.3) (< 0.8)))
    (menhir (>= 20230608))
    (sedlex (and (>= 3) (< 4)))
    (shell (and (>= v0.16) (< v0.17)))
    (textutils_kernel (and (>= v0.16) (< v0.17)))
    (travesty (and (>= 0.8) (< 0.9)))
    (yojson (and (>= 2.1) (< 3)))
    ))

; Local Variables:
; mode: lisp
; End:
