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

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Explains act's understanding of an assembly file"
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
       let cid = CompilerSpec.Id.of_string compiler_id in
       let o = OutputCtx.make ~verbose ~warnings in
       let passes = Sanitiser.(
           if sanitise then Pass.explain else Pass.Set.empty
         )
       in
       Or_error.Let_syntax.(
         let%bind specs = CompilerSpec.load_specs ~path:spec_file in
         Common.do_litmusify `Explain passes o ~infile ~outfile cid specs
       )
       |> Common.print_error
   ]
;;
