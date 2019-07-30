(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel

let run (args : Toplevel.Args.Standard.t Toplevel.Args.With_files.t) (_o : Act_common.Output.t) (_cfg : Act_config.Act.t) : unit Or_error.t =
  Or_error.Let_syntax.(
    let%bind i = Toplevel.Args.With_files.infile_source args in
    let%bind vast = Act_c_mini.Frontend.load_from_isrc i in
    let vars = Act_c_mini.Litmus.vars vast in
    let%bind o = Toplevel.Args.With_files.outfile_sink args in
    Act_backend.Diff.Location_map.(
      vars |> reflexive |> output ~onto:o
    )
  )

let readme () : string =
  Act_utils.My_string.format_for_readme
    {|
Generates a reflexive location map --- a map from each thread-ID-qualified C
identifier to itself --- for a given C litmus test.

This seemingly pointless exercise is useful in situations where one needs to
diff the state output of a C test against some modification of itself: for
example, to test ACT's fuzzer.
|}

let command : Command.t =
  Command.basic ~summary:"outputs a reflexive location map"
    ~readme
    Command.Let_syntax.(
      let%map_open standard_args =
        ignore anon ;
        Toplevel.Args.(With_files.get Standard.get)
      in
      fun () ->
        Toplevel.Common.lift_command
          (Toplevel.Args.With_files.rest standard_args)
          ~with_compiler_tests:false
          ~f:(run standard_args))
