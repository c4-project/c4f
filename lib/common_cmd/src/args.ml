(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core
open Act_common
module Tx = Travesty_base_exts

module Colour_table = Act_utils.String_table.Make (struct
  let equal_style_renderer (x : Fmt.style_renderer) (y : Fmt.style_renderer)
      : bool =
    match (x, y) with
    | `Ansi_tty, `Ansi_tty | `None, `None ->
        true
    | `Ansi_tty, `None | `None, `Ansi_tty ->
        false

  type t = Fmt.style_renderer option

  let equal : t -> t -> bool = Option.equal equal_style_renderer

  let table =
    [(Some `None, "never"); (Some `Ansi_tty, "always"); (None, "auto")]
end)

let colour_map : Fmt.style_renderer option String.Map.t =
  Map.of_alist_exn (module String) (List.Assoc.inverse Colour_table.table)

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

  let fpath_type : Fpath.t Arg_type.t =
    Arg_type.map ~f:Fpath.v Filename.arg_type

  let input_type : Plumbing.Input.t Arg_type.t =
    Arg_type.map
      ~f:(Fn.compose Or_error.ok_exn Plumbing.Input.of_string)
      Filename.arg_type

  let output_type : Plumbing.Output.t Arg_type.t =
    Arg_type.map
      ~f:(Fn.compose Or_error.ok_exn Plumbing.Output.of_string)
      Filename.arg_type

  let backend ?(name : string = "-backend")
      ?(doc : string = "the backend to use") () : Id.t option Command.Param.t
      =
    flag name (optional id_type) ~doc:("SIM_ID " ^ doc)

  let arch ?(name : string = "-arch")
      ?(doc : string = "the architecture to target") () :
      Id.t option Command.Param.t =
    flag name (optional id_type) ~doc:("ARCH_ID " ^ doc)

  let backend_arch : Act_backend.Arch.t option Command.Param.t =
    Command.Param.(
      choose_one
        [ flag_to_enum_choice Act_backend.Arch.c "-c"
            ~doc:
              "Tells the backend to treat the input as C, with no \
               underlying target architecture"
        ; map
            ~f:
              (Option.map ~f:(fun x ->
                   Act_backend.Arch.C {underlying_arch= Some x}))
            (arch ~name:"-carch"
               ~doc:
                 "tells the backend to treat the input as C, with the given \
                  underlying target architecture"
               ())
        ; map
            ~f:(Option.map ~f:(fun x -> Act_backend.Arch.Assembly x))
            (arch
               ~doc:
                 "tells the backend to treat the input as assembly, with \
                  the given target architecture"
               ()) ]
        ~if_nothing_chosen:Return_none)

  let aux_file : string option Command.Param.t =
    flag "aux-file"
      (optional Filename.arg_type)
      ~doc:
        "FILE path to a JSON file containing auxiliary litmus information \
         for this file"

  let backend_predicate =
    flag "filter-backends"
      (optional (sexp_conv [%of_sexp: Act_backend.Property.t Blang.t]))
      ~doc:"PREDICATE filter backends using this predicate"

  let machine_predicate =
    flag "filter-machines"
      (optional (sexp_conv [%of_sexp: Act_machine.Property.t Blang.t]))
      ~doc:"PREDICATE filter machines using this predicate"
end

include Other

module Standard = struct
  type t =
    { verbose: bool
    ; no_warnings: bool
    ; colour: Fmt.style_renderer option
    ; config_file: string }
  [@@deriving fields]

  let is_verbose t = t.verbose

  let are_warnings_enabled t = not t.no_warnings

  let default_config_file = "act.conf"

  let load_config (x : t) : Act_config.Global.t Or_error.t =
    Or_error.(
      x.config_file |> Plumbing.Input.of_string
      >>= Act_config.Global.Load.load)

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

  let make_output (args : t) : Act_common.Output.t =
    Act_common.Output.make ~verbose:(is_verbose args)
      ~warnings:(are_warnings_enabled args)

  let setup_colour (args : t) : unit =
    let style_renderer = colour args in
    Fmt_tty.setup_std_outputs ?style_renderer ()

  let lift_command (args : t) ~(f : Act_common.Output.t -> unit Or_error.t) :
      unit =
    setup_colour args ;
    let o = make_output args in
    let result = f o in
    if Or_error.is_error result then (
      Act_common.Output.print_error o result ;
      exit 1 )

  let lift_command_with_config (args : t)
      ~(f : Act_common.Output.t -> Act_config.Global.t -> unit Or_error.t) :
      unit =
    lift_command args ~f:(fun o -> Or_error.(args |> load_config >>= f o))
end

module With_files = struct
  type 'a t = {rest: 'a; infiles_raw: string list; outfile_raw: string option}
  [@@deriving fields]

  let out : string option Command.Param.t =
    Command.Param.(
      flag "output"
        (optional Filename.arg_type)
        ~doc:"FILE the output file (default: stdout)")

  let get (type a) (rest : a Command.Param.t) : a t Command.Param.t =
    Command.Let_syntax.(
      let%map_open infile_raw = anon (maybe ("FILE" %: Filename.arg_type))
      and outfile_raw = out
      and rest = rest in
      {rest; infiles_raw= Option.to_list infile_raw; outfile_raw})

  let get_with_multiple_inputs (type a) (rest : a Command.Param.t) :
      a t Command.Param.t =
    Command.Let_syntax.(
      let%map_open infiles_raw =
        anon (sequence ("FILE" %: Filename.arg_type))
      and outfile_raw = out
      and rest = rest in
      {rest; infiles_raw; outfile_raw})

  let infiles_fpath (args : _ t) : Fpath.t list Or_error.t =
    Tx.Or_error.combine_map (infiles_raw args)
      ~f:Plumbing.Fpath_helpers.of_string

  let infile_raw (args : _ t) : string option Or_error.t =
    args |> infiles_raw |> Tx.List.at_most_one

  let infile_fpath (args : _ t) : Fpath.t option Or_error.t =
    Or_error.(args |> infile_raw >>= Plumbing.Fpath_helpers.of_string_option)

  let infile_source (args : _ t) : Plumbing.Input.t Or_error.t =
    Or_error.(args |> infile_raw >>= Plumbing.Input.of_string_opt)

  let outfile_fpath (args : _ t) : Fpath.t option Or_error.t =
    args |> outfile_raw |> Plumbing.Fpath_helpers.of_string_option

  let outfile_sink (args : _ t) : Plumbing.Output.t Or_error.t =
    args |> outfile_raw |> Plumbing.Output.of_string_opt

  let run_filter (type i o)
      (module F : Plumbing.Filter_types.S
        with type aux_i = i
         and type aux_o = o) (args : _ t) ~(aux_in : i) : o Or_error.t =
    Or_error.Let_syntax.(
      let%bind input = infile_source args in
      let%bind output = outfile_sink args in
      F.run aux_in input output)
end
