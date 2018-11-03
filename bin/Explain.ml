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

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"explains act's understanding of an assembly file"
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
     and sanitise =
       flag "sanitise"
         no_arg
         ~doc: "if true, do basic sanitisation on the assembly first"
     and compiler_id =
       anon ("COMPILER_ID" %: string)
     and outfile =
       flag "output"
            (optional string)
            ~doc: "FILE the explanation output file (default: stdout)"
     and infile =
       anon (maybe ("FILE" %: string))
     in
     fun () ->
       let warnings = not no_warnings in
       let cid = Compiler.Id.of_string compiler_id in
       let o = OutputCtx.make ~verbose ~warnings in
       let passes = Sanitiser_pass.(
           if sanitise then explain else Set.empty
         )
       in
       Or_error.Let_syntax.(
         let%bind cfg = LangSupport.load_cfg spec_file in
         let%bind spec = Compiler.CSpec.Set.get (Config.M.compilers cfg) cid in
         let emits = Compiler.CSpec.emits spec in
         let%bind runner = LangSupport.get_runner ~emits in
         let module Runner = (val runner) in
         Io.(
           let input =
             { Asm_job.inp = In_source.of_option infile
             ; outp = Out_sink.of_option outfile
             ; passes
             ; symbols = []
             }
           in
           let%map out = Runner.explain input in
           Asm_job.warn out o.wf
         )
       )
       |> Common.print_error
   ]
;;
