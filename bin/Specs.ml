(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
   LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
   WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Core
open Lib
open Utils

let pp_spec_verbose f (c, s) =
  MyFormat.pp_kv f (CompilerSpec.Id.to_string c) CompilerSpec.pp s
;;

let pp_spec_terse f (c, s) =
  Format.pp_open_hbox f ();
  let facts =
    List.concat
      [ [CompilerSpec.Id.to_string c]
      ; if s.CompilerSpec.enabled then [] else ["(DISABLED)"]
      ; if Option.is_none s.ssh then [] else ["(REMOTE)"]
      ]
  in
  Format.pp_print_list ~pp_sep:Format.pp_print_space String.pp f facts;
  Format.pp_close_box f ()
;;

let pp_specs
    (verbose : bool) (f : Format.formatter)
    (specs : CompilerSpec.set) : unit =
  Format.pp_open_vbox f 0;
  Format.pp_print_list
    ~pp_sep:Format.pp_print_cut
    (if verbose then pp_spec_verbose else pp_spec_terse)
    f
    specs;
  Format.pp_close_box f ();
  Format.pp_print_newline f ()
;;

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"outputs information about the current compiler specs"
    [%map_open
      let verbose =
        flag "verbose"
          no_arg
          ~doc: "print more information about the compilers"
      and spec_file =
        flag "spec"
          (optional_with_default
             (Filename.concat Filename.current_dir_name "compiler.spec")
             string)
          ~doc: "PATH the compiler spec file to use"
      in
      fun () ->
        Or_error.Let_syntax.(
          let%bind specs = CompilerSpec.load_specs ~path:spec_file in
          pp_specs verbose Format.std_formatter specs;
          return ()
        )
        |> Common.print_error
    ]
;;
