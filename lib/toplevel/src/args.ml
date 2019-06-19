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
open Act_utils
open Args_intf

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

module Other = struct
  open Command.Param

  let flag_to_enum_choice (type a) (enum : a) (str : string) ~(doc : string)
      : a option t =
    map ~f:(Fn.flip Option.some_if enum) (flag str no_arg ~doc)

  let id_type = Arg_type.create Id.of_string

  let simulator ?(name : string = "-simulator")
      ?(doc : string = "the simulator to use") () :
      Id.t option Command.Param.t =
    flag name (optional id_type) ~doc:("SIM_ID " ^ doc)

  let arch ?(name : string = "-arch")
      ?(doc : string = "the architecture to target") () :
      Id.t option Command.Param.t =
    flag name (optional id_type) ~doc:("ARCH_ID " ^ doc)

  let asm_target : Asm_target.t Command.Param.t =
    choose_one
      [ map
          ~f:(Option.map ~f:Asm_target.compiler_id)
          (flag "compiler" (optional id_type)
             ~doc:"COMPILER_ID ID of the compiler to target")
      ; map ~f:(Option.map ~f:Asm_target.arch) (arch ()) ]
      ~if_nothing_chosen:`Raise

  let aux_file : string option Command.Param.t =
    flag "aux-file"
      (optional Filename.arg_type)
      ~doc:
        "FILE path to a JSON file containing auxiliary litmus information \
         for this file"

  let sanitiser_passes :
      Act_sanitiser.Pass_group.Selector.t Blang.t option Command.Param.t =
    flag "sanitiser-passes"
      (optional
         (sexp_conv [%of_sexp: Act_sanitiser.Pass_group.Selector.t Blang.t]))
      ~doc:"PREDICATE select which sanitiser passes to use"

  let compiler_predicate =
    flag "filter-compilers"
      (optional (sexp_conv [%of_sexp: Act_compiler.Property.t Blang.t]))
      ~doc:"PREDICATE filter compilers using this predicate"

  let machine_predicate =
    flag "filter-machines"
      (optional (sexp_conv [%of_sexp: Act_machine.Property.t Blang.t]))
      ~doc:"PREDICATE filter machines using this predicate"
end

include Other

module Standard : sig
  type t

  include S_standard with type t := t and type s := t
end = struct
  type t =
    { verbose: bool
    ; no_warnings: bool
    ; colour: Fmt.style_renderer option
    ; config_file: string }
  [@@deriving fields]

  let as_standard_args = Fn.id

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

module Standard_with_files :
  S_standard_with_files with type s := Standard.t = struct
  type t =
    {rest: Standard.t; infile_raw: string option; outfile_raw: string option}
  [@@deriving fields]

  let as_standard_args t = t.rest

  module Rest = Standard

  include Inherit.Helpers (struct
    type nonrec t = t

    type c = Rest.t

    let component = as_standard_args
  end)

  let is_verbose = forward Rest.is_verbose

  let are_warnings_enabled = forward Rest.are_warnings_enabled

  let config_file = forward Rest.config_file

  let colour = forward Rest.colour

  let get =
    Command.Let_syntax.(
      let%map_open infile_raw = anon (maybe ("FILE" %: Filename.arg_type))
      and outfile_raw =
        flag "output"
          (optional Filename.arg_type)
          ~doc:"FILE the output file (default: stdout)"
      and rest = Standard.get in
      {rest; infile_raw; outfile_raw})

  let infile_fpath (args : t) : Fpath.t option Or_error.t =
    args |> infile_raw |> Plumbing.Fpath_helpers.of_string_option

  let infile_source (args : t) : Plumbing.Input.t Or_error.t =
    args |> infile_raw |> Plumbing.Input.of_string_opt

  let outfile_fpath (args : t) : Fpath.t option Or_error.t =
    args |> outfile_raw |> Plumbing.Fpath_helpers.of_string_option

  let outfile_sink (args : t) : Plumbing.Output.t Or_error.t =
    args |> outfile_raw |> Plumbing.Output.of_string_opt
end

module Standard_asm : S_standard_asm with type s := Standard.t = struct
  type t =
    { rest: Standard_with_files.t
    ; aux_file: string option
    ; target: Asm_target.t
    ; sanitiser_passes: Act_sanitiser.Pass_group.Selector.t Blang.t option
    }
  [@@deriving fields]

  module Rest = Standard_with_files

  include Inherit.Helpers (struct
    type nonrec t = t

    type c = Rest.t

    let component t = t.rest
  end)

  let is_verbose = forward Rest.is_verbose

  let as_standard_args = forward Rest.as_standard_args

  let are_warnings_enabled = forward Rest.are_warnings_enabled

  let config_file = forward Rest.config_file

  let colour = forward Rest.colour

  let infile_raw = forward Rest.infile_raw

  let infile_fpath = forward Rest.infile_fpath

  let infile_source = forward Rest.infile_source

  let outfile_raw = forward Rest.outfile_raw

  let outfile_fpath = forward Rest.outfile_fpath

  let outfile_sink = forward Rest.outfile_sink

  let get =
    Command.Let_syntax.(
      let%map_open target = Other.asm_target
      and aux_file = Other.aux_file
      and sanitiser_passes = Other.sanitiser_passes
      and rest = Standard_with_files.get in
      {rest; target; aux_file; sanitiser_passes})
end
