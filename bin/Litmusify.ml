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

open Core
open Lib
open Utils

let make_herd cfg =
  let open Or_error.Let_syntax in
  let%bind herd_cfg =
    Result.of_option (Config.M.herd cfg)
      ~error:(Error.of_string
                "No Herd stanza in configuration"
             )
  in
  Herd.create ~config:herd_cfg
;;

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"converts an assembly file to a litmus test"
    [%map_open
      let standard_args = Standard_args.get
      and herd =
        flag "herd"
          no_arg
          ~doc: "if true, pipe results through herd"
      and compiler_id =
        anon ("COMPILER_ID" %: string)
      and outfile =
        flag "output"
          (optional string)
          ~doc: "FILE the litmus output file (default: stdout)"
      and infile =
        anon (maybe ("FILE" %: string))
      in
      fun () ->
        Common.lift_command standard_args
          ~local_only:false
          ~test_compilers:false
          ~f:(fun o cfg ->
              let id = Spec.Id.of_string compiler_id in
              Result.Let_syntax.(
                let%bind spec = Compiler.Full_spec.Set.get (Config.M.compilers cfg) id in
                let cspec = Compiler.Full_spec.With_id.create ~id ~spec in
                Io.(
                  let source = In_source.of_option infile in
                  let sink = Out_sink.of_option outfile in
                  if herd
                  then
                    let%bind herd = make_herd cfg in
                    let tmpname = Filename.temp_file "act" "litmus" in
                    let%bind _ = Common.litmusify o source (`File tmpname) [] cspec in
                    let arch = Herd.Assembly (Compiler.Full_spec.emits spec) in
                    Herd.run herd arch ~path:tmpname ~sink
                  else Or_error.ignore (Common.litmusify o source sink [] cspec)
                )
              )
            )
    ]
