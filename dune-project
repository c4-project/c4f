(lang dune 1.10)
(name act)

(implicit_transitive_deps false)
(generate_opam_files true)

(using menhir 2.0)
(using fmt 1.1)

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
    (ocaml (>= 4.07))
    (dune (and :build (> 1.10)))
    ppx_deriving
    ppx_import
    ppx_jane
    (core (and (>= v0.12) (< v0.13)))
    (fmt (and (>= 0.8.5) (< 0.9)))
    (fpath (and (>= 0.7.2) (< 0.8)))
    (menhir (>= 20181113))
    (sedlex (and (>= 2.0) (< 2.1)))
    (shell (and (>= v0.12) (< v0.13)))
    (textutils_kernel (and (>= v0.12) (< v0.13)))
    (travesty (and (>= 0.5.1) (< 0.6)))
    (yojson (and (>= 1.7) (< 1.8)))
  ))

