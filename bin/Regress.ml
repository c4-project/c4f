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

let regress_run_litmusifier (l : (module Litmusifier.S))
    path mode passes file =
  let open Or_error.Let_syntax in
  let module L = (val l) in

  printf "## %s\n\n```\n" file;
  Out_channel.flush stdout;

  let o =
    { OutputCtx.vf = MyFormat.null_formatter ()
    ; wf = MyFormat.null_formatter ()
    ; ef = Format.std_formatter
    }
  in

  let f _ inp _ outp =
    L.run
      { o
      ; iname = file
      ; inp
      ; outp
      ; mode
      ; passes
      ; symbols = [] (* TODO(@MattWindsor91): exercise this? *)
      }
  in

  let%bind _ =
    Io.with_input_and_output ~f (`File (path ^/ file)) `Stdout
  in

  Out_channel.flush stdout;
  printf "```\n";

  return ()
;;

let regress_run_litmusifier_many modename mode passes test_path =
  let open Or_error.Let_syntax in

  printf "# %s tests\n\n" modename;

  let emits = ["x86"; "att"] in
  let path = MyFilename.concat_list ([test_path; "asm"] @ emits) in
  let%bind l = LangSupport.get_litmusifier ~emits in
  let%bind testfiles = Io.Dir.get_files ~ext:"s" path in
  let results = List.map
      ~f:(regress_run_litmusifier l path mode passes) testfiles
  in
  let%bind _ = Or_error.combine_errors_unit results in
  printf "\nRan %d test(s).\n\n" (List.length testfiles);
  return ()
;;

let regress_explain =
  regress_run_litmusifier_many "Explainer" `Explain
    (Sanitiser_pass.explain)
;;

let regress_litmusify =
  regress_run_litmusifier_many "Litmusifier" `Litmusify
    (Sanitiser_pass.all_set ())
;;

let regress test_path =
  let open Or_error.Let_syntax in
  let%bind _ = regress_explain test_path in
  let%bind _ = regress_litmusify test_path in
  Out_channel.newline stdout;
  return ()
;;

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"runs regression tests"
    [%map_open
      let test_path =
        anon ("TEST_PATH" %: string)
      in
       fun () ->
         regress test_path
         |> Common.print_error
    ]
;;
