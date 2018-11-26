(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

(** [Standard_args] contains argument specifications common to all act
   sub-commands. *)

open Core
open Lib

type t =
  { verbose     : bool
  ; no_warnings : bool
  ; spec_file   : string
  }
;;

let is_verbose t = t.verbose
let are_warnings_enabled t = not t.no_warnings
let spec_file t = t.spec_file

let default_spec_file = "compiler.spec"

let get =
  let open Command.Let_syntax in
  [%map_open
    let verbose =
      flag "verbose"
        no_arg
        ~doc: "print more information about the compilers"
    and no_warnings =
      flag "no-warnings"
        no_arg
        ~doc: "if given, suppresses all warnings"
    and spec_file =
      flag_optional_with_default_doc
        "spec"
        string [%sexp_of: string]
        ~default:default_spec_file
        ~doc:"PATH the compiler spec file to use"
    in
    { verbose
    ; no_warnings
    ; spec_file
    }
  ]
;;

module Other = struct
  open Command.Param

  let compiler_id_type = Arg_type.create Id.of_string

  let compiler_id_or_arch =
    choose_one
      [ map ~f:(Option.map ~f:(fun x -> `Id x))
          (flag "compiler"
             (optional compiler_id_type)
             ~doc: "COMPILER_ID ID of the compiler to target")
      ; map ~f:(Option.map ~f:(fun x -> `Arch x))
          (flag "arch"
             (optional (sexp_conv [%of_sexp: string list]))
             ~doc: "EMITS_CLAUSE the architecture to target")
      ]
      ~if_nothing_chosen:`Raise
  ;;

  let file_type =
    choose_one
      [ (map ~f:(Fn.flip Option.some_if `C)
           (flag "c"
              no_arg
              ~doc: "if given, assume input is C (and compile it)"))
      ; (map ~f:(Fn.flip Option.some_if `Assembly)
           (flag "asm"
              no_arg
              ~doc: "if given, assume input is assembly"))
      ]
      ~if_nothing_chosen:(`Default_to `Infer)
  ;;

  let compiler_predicate =
    flag "filter-compilers"
      (optional (sexp_conv [%of_sexp: Compiler.Property.t Blang.t]))
      ~doc:"PREDICATE filter compilers using this predicate"
  ;;

  let machine_predicate =
    flag "filter-machines"
      (optional (sexp_conv [%of_sexp: Machine.Property.t Blang.t]))
      ~doc:"PREDICATE filter machines using this predicate"
  ;;
end
