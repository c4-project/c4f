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

(** Main entry point into act *)

open Core
open Lib
open Utils

let pp_specs (f : Format.formatter) (specs : CompilerSpec.set) : unit =
  Format.pp_open_vbox f 0;
  Format.pp_print_string f "Compiler specs --";
  Format.pp_print_break f 0 4;
  Format.pp_open_vbox f 0;
  List.iter ~f:(fun (c, s) ->
      MyFormat.pp_kv f (CompilerSpec.Id.to_string c) CompilerSpec.pp s) specs;
  Format.pp_close_box f ();
  Format.pp_print_cut f ();
  Format.pp_close_box f ();
  Format.pp_print_flush f ()

let prerr =
  function
  | Ok _ -> ()
  | Error err ->
    Format.eprintf "@[act encountered a top-level error:@.@[%a@]@]@." Error.pp err

let get_spec specs compiler_id =
  List.Assoc.find specs ~equal:(CompilerSpec.Id.equal) compiler_id
  |> Result.of_option
    ~error:(Error.create "invalid compiler ID" compiler_id [%sexp_of: CompilerSpec.Id.t])

let do_litmusify mode o ~infile ~outfile (cid : CompilerSpec.Id.t) specs =
  let open Result.Let_syntax in
  let%bind spec = get_spec specs cid in
  Io.(
    let f src inp _ outp =
      let iname = MyFormat.format_to_string (In_source.pp) src in
      Litmusifier.run
        { o
        ; cid
        ; spec
        ; iname
        ; inp
        ; outp
        ; mode
        }
    in
    with_input_and_output
      (In_source.of_option infile)
      (Out_sink.of_option outfile)
      ~f
  )

let explain =
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
       Or_error.Let_syntax.(
         let%bind specs = Compiler.load_and_test_specs ~path:spec_file in
         pp_specs o.vf specs;
         do_litmusify `Explain o ~infile ~outfile cid specs
       )
       |> prerr
    ]

let run_herd prog litname _ oc =
  Run.Local.run ~oc ~prog [litname]

let litmusify =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Converts an assembly file to a litmus test"
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
     and sendto =
       flag "sendto"
         (optional string)
         ~doc: "CMDNAME pass generated litmus through this Herd-like command"
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
       let cid = CompilerSpec.Id.of_string compiler_id in
       let o = OutputCtx.make ~verbose ~warnings in
       Result.Let_syntax.(
         let%bind specs = Compiler.load_and_test_specs ~path:spec_file in
         pp_specs o.vf specs;
         match sendto with
         | None -> do_litmusify `Litmusify o ~infile ~outfile cid specs
         | Some cmd ->
           let tmpname = Filename.temp_file "act" "litmus" in
           let cid = CompilerSpec.Id.of_string compiler_id in
           let%bind _ = do_litmusify `Litmusify o ~infile ~outfile:(Some tmpname) cid specs in
           Io.Out_sink.with_output ~f:(run_herd cmd tmpname)
             (Io.Out_sink.of_option outfile)
       )
       |> prerr
    ]

let memalloy =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Runs automatic testing over a memalloy output directory"
    [%map_open
      let spec_file =
        flag "spec"
          (optional_with_default
             (Filename.concat Filename.current_dir_name "compiler.spec")
             string)
          ~doc: "PATH the compiler spec file to use"
      and out_root =
        flag "output"
          (optional_with_default
             Filename.current_dir_name
             string)
          ~doc: "PATH the path under which output directories will be created"
      and verbose =
        flag "verbose"
          no_arg
          ~doc: "verbose mode"
      and no_warnings =
        flag "no-warnings"
          no_arg
          ~doc: "silence all warnings"
      and in_root =
        anon ("RESULTS_PATH" %: string)
      in
      fun () ->
        let warnings = not no_warnings in
        let o = OutputCtx.make ~verbose ~warnings in
        Result.Let_syntax.(
          let%bind specs = Compiler.load_and_test_specs ~path:spec_file in
          pp_specs o.vf specs;
          Memalloy.run o specs ~in_root ~out_root
        ) |> prerr
]

let command =
  Command.group
    ~summary:"Automagic Compiler Tormentor"
    [ "explain"  , explain
    ; "litmusify", litmusify
    ; "memalloy" , memalloy
    ]

let () = Command.run command
