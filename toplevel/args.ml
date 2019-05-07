(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Core
open Act_common
open Utils
include Args_intf

module Colour_table = String_table.Make (struct
  type t = Fmt.style_renderer option

  let table =
    [(Some `None, "never"); (Some `Ansi_tty, "always"); (None, "auto")]
end)

let colour_map : Fmt.style_renderer option String.Map.t =
  String.Map.of_alist_exn (List.Assoc.inverse Colour_table.table)

let colour_type : Fmt.style_renderer option Command.Arg_type.t =
  Command.Arg_type.of_map colour_map

let colour_sexp (sr : Fmt.style_renderer option) : Sexp.t =
  sr |> Colour_table.to_string |> Option.value ~default:"?" |> Sexp.Atom

module Standard : S_standard = struct
  type t =
    { verbose: bool
    ; no_warnings: bool
    ; colour: Fmt.style_renderer option
    ; config_file: string }
  [@@deriving fields]

  let is_verbose t = t.verbose

  let are_warnings_enabled t = not t.no_warnings

  let default_config_file = "act.conf"

  let get =
    Command.Let_syntax.(
      let%map_open verbose =
        flag "verbose" no_arg
          ~doc:"print more information about the compilers"
      and no_warnings =
        flag "no-warnings" no_arg ~doc:"if given, suppresses all warnings"
      and config_file =
        flag_optional_with_default_doc "config" string [%sexp_of: string]
          ~default:default_config_file ~doc:"PATH the act.conf file to use"
      and colour =
        flag_optional_with_default_doc "colour" colour_type colour_sexp
          ~default:None ~doc:"MODE force a particular colouring mode"
      in
      {verbose; no_warnings; config_file; colour})
end

module Standard_with_files = struct
  type nonrec t =
    {rest: Standard.t; infile: string option; outfile: string option}

  let as_standard_args t = t.rest

  let is_verbose t = Standard.is_verbose t.rest

  let are_warnings_enabled t = Standard.are_warnings_enabled t.rest

  let config_file t = Standard.config_file t.rest

  let colour t = Standard.colour t.rest

  let get =
    Command.Let_syntax.(
      let%map_open infile = anon (maybe ("FILE" %: Filename.arg_type))
      and outfile =
        flag "output"
          (optional Filename.arg_type)
          ~doc:"FILE the output file (default: stdout)"
      and rest = Standard.get in
      {rest; infile; outfile})

  let infile_raw (args : t) : string option = args.infile

  let infile_fpath (args : t) : Fpath.t option Or_error.t =
    args |> infile_raw |> Io.fpath_of_string_option

  let infile_source (args : t) : Io.In_source.t Or_error.t =
    args |> infile_raw |> Io.In_source.of_string_opt

  let outfile_raw (args : t) : string option = args.outfile

  let outfile_fpath (args : t) : Fpath.t option Or_error.t =
    args |> outfile_raw |> Io.fpath_of_string_option

  let outfile_sink (args : t) : Io.Out_sink.t Or_error.t =
    args |> outfile_raw |> Io.Out_sink.of_string_opt
end

module Other = struct
  open Command.Param

  let flag_to_enum_choice (type a) (enum : a) (str : string) ~(doc : string)
      : a option t =
    map ~f:(Fn.flip Option.some_if enum) (flag str no_arg ~doc)

  let compiler_id_type = Arg_type.create Id.of_string

  let arch_type = Arg_type.create Id.of_string

  let arch ?(name : string = "-arch")
      ?(doc : string = "the architecture to target") () :
      Id.t option Command.Param.t =
    flag name (optional arch_type) ~doc:("ARCH_ID " ^ doc)

  let compiler_id_or_arch =
    choose_one
      [ map
          ~f:(Option.map ~f:(fun x -> `Id x))
          (flag "compiler"
             (optional compiler_id_type)
             ~doc:"COMPILER_ID ID of the compiler to target")
      ; map ~f:(Option.map ~f:(fun x -> `Arch x)) (arch ()) ]
      ~if_nothing_chosen:`Raise

  let file_type =
    choose_one
      [ flag_to_enum_choice `C "c"
          ~doc:"if given, assume input is C (and compile it)"
      ; flag_to_enum_choice `C_litmus "c-litmus"
          ~doc:
            "if given, assume input is C/litmus (and delitmusify and \
             compile it)"
      ; flag_to_enum_choice `Assembly "asm"
          ~doc:"if given, assume input is assembly" ]
      ~if_nothing_chosen:(`Default_to `Infer)

  let c_variables_arg_type =
    optional
      (Arg_type.comma_separated ~unique_values:true ~strip_whitespace:true
         string)

  let c_globals =
    flag "c-globals" c_variables_arg_type
      ~doc:"IDS comma-separated list of C global variables to track"

  let c_locals =
    flag "c-locals" c_variables_arg_type
      ~doc:"IDS comma-separated list of C local variables to track"

  let sanitiser_passes =
    flag "sanitiser-passes"
      (optional
         (sexp_conv [%of_sexp: Config.Sanitiser_pass.Selector.t Blang.t]))
      ~doc:"PREDICATE select which sanitiser passes to use"

  let compiler_predicate =
    flag "filter-compilers"
      (optional (sexp_conv [%of_sexp: Config.Compiler.Property.t Blang.t]))
      ~doc:"PREDICATE filter compilers using this predicate"

  let machine_predicate =
    flag "filter-machines"
      (optional (sexp_conv [%of_sexp: Config.Machine.Property.t Blang.t]))
      ~doc:"PREDICATE filter machines using this predicate"
end

include Other
