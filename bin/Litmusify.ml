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

let run_herd prog litname _ oc =
  Run.Local.run ~oc ~prog [litname]

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"converts an assembly file to a litmus test"
    [%map_open
     let spec_file =
       flag "spec"
            (optional_with_default
               (Filename.concat Filename.current_dir_name "compiler.spec")
               string)
            ~doc: "PATH the compiler spec file to use"
     and verbose =
       flag "verbose"
         no_arg
         ~doc: "verbose mode"
     and no_warnings =
       flag "no-warnings"
         no_arg
         ~doc: "silence all warnings"
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
       let warnings = not no_warnings in
       let cid = Compiler.Id.of_string compiler_id in
       let o = OutputCtx.make ~verbose ~warnings in
       let passes = Sanitiser_pass.all_set () in
       Result.Let_syntax.(
         let%bind cfg = LangSupport.load_cfg spec_file in
         let%bind spec = Compiler.CSpec.Set.get (Config.M.compilers cfg) cid in
         if herd
         then
           let cmd = Config.M.herd_or_default cfg in
           let tmpname = Filename.temp_file "act" "litmus" in
           let%bind () =
             Common.do_litmusify `Litmusify passes o ~infile ~outfile:(Some tmpname) spec in
           Io.Out_sink.with_output ~f:(run_herd cmd tmpname)
             (Io.Out_sink.of_option outfile)
         else
           Common.do_litmusify `Litmusify passes o ~infile ~outfile spec
       )
       |> Common.print_error
    ]
