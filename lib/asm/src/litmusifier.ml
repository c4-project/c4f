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

open Base
open Stdio
open Litmusifier_intf
module Ac = Act_common
module Tx = Travesty_base_exts

module Format = struct
  type t = Full | Programs_only [@@deriving sexp, equal]

  let default = Full
end

module Config = struct
  type t =
    { format: Format.t [@default Format.default]
    ; aux: Act_delitmus.Aux.t [@default Act_delitmus.Aux.empty] }
  [@@deriving equal, fields, make]

  let default : unit -> t = make

  module W = Travesty.Traversable.Helpers (Or_error)

  let transform (initial : t) ~(format : Format.t -> Format.t Or_error.t)
      ~(aux : Act_delitmus.Aux.t -> Act_delitmus.Aux.t Or_error.t) :
      t Or_error.t =
    Fields.fold ~init:(Or_error.return initial)
      ~format:(W.proc_field format) ~aux:(W.proc_field aux)
end

module Make (B : Runner_intf.Basic) :
  S
    with type config = Config.t
     and type fmt = Format.t
     and type program =
          ( B.Src_lang.Element.t
          , B.Src_lang.Program.t )
          Act_sanitiser.Output.Program.t
     and module Redirect := B.Src_lang.Symbol.R_map = struct
  type config = Config.t

  type fmt = Format.t

  type program =
    ( B.Src_lang.Element.t
    , B.Src_lang.Program.t )
    Act_sanitiser.Output.Program.t

  module Litmus = B.Litmus_ast
  module Sanitiser = Act_sanitiser.Instance.Make (B.Sanitiser_hook)

  let print_litmus : Format.t -> Out_channel.t -> Litmus.Validated.t -> unit
      = function
    | Full ->
        B.Litmus_pp.print
    | Programs_only ->
        B.Litmus_pp.print_programs

  let make_litmus_program (program : Sanitiser.Output.Program.t) =
    program |> Sanitiser.Output.Program.listing |> B.convert_program

  let make_litmus_programs = Tx.Or_error.combine_map ~f:make_litmus_program

  module Make_aux = Litmusifier_aux.Make (struct
    module Symbol = B.Src_lang.Symbol
    module Constant = B.Dst_lang.Constant
  end)

  let make_aux (config : Config.t)
      (redirect_map : B.Src_lang.Symbol.R_map.t) :
      B.Dst_lang.Constant.t Act_litmus.Aux.t Or_error.t =
    let dl_aux = Config.aux config in
    Make_aux.of_delitmus_aux dl_aux ~redirect_map

  let make ~(config : Config.t) ~(redirects : B.Src_lang.Symbol.R_map.t)
      ~(name : string) ~(programs : program list) =
    Or_error.Let_syntax.(
      let%bind aux = make_aux config redirects in
      let init = Act_litmus.Aux.init aux in
      let postcondition = Act_litmus.Aux.postcondition aux in
      let locations = Act_litmus.Aux.locations aux in
      let%bind l_programs = make_litmus_programs programs in
      Or_error.tag ~tag:"Couldn't build litmus file."
        (Litmus.Validated.make ~name ~init ~programs:l_programs
           ?postcondition ?locations ()))

  let collate_warnings (programs : Sanitiser.Output.Program.t list) =
    List.concat_map programs ~f:Sanitiser.Output.Program.warnings

  let unstringify_symbol (sym : string) : B.Src_lang.Symbol.t Or_error.t =
    Result.of_option
      (B.Src_lang.Symbol.of_string_opt sym)
      ~error:
        (Error.create_s
           [%message "Symbol can't be converted from string" ~symbol:sym])

  let unstringify_symbols :
      string list -> B.Src_lang.Symbol.t list Or_error.t =
    Tx.Or_error.combine_map ~f:unstringify_symbol

  let output_litmus (outp : Out_channel.t) ~(in_name : string)
      ~(program : B.Src_lang.Program.t) ~(config : Config.t)
      ~(passes : Set.M(Act_sanitiser.Pass_group).t) :
      Job.Output.t Or_error.t =
    let open Or_error.Let_syntax in
    let aux = Config.aux config in
    let symbol_strs = Act_delitmus.Aux.symbols aux in
    let%bind symbols = unstringify_symbols symbol_strs in
    let%bind o = Sanitiser.sanitise ~passes ~symbols program in
    let redirects = Sanitiser.Output.redirects o in
    let programs = Sanitiser.Output.programs o in
    let warnings = collate_warnings programs in
    let%map lit = make ~config ~redirects ~name:in_name ~programs in
    print_litmus (Config.format config) outp lit ;
    Out_channel.newline outp ;
    let redirects = Sanitiser.Output.redirects o in
    Job.Output.make Sanitiser.Warn.pp in_name
      (B.Src_lang.Symbol.R_map.to_string_alist redirects)
      warnings

  module Filter : Runner_intf.S with type cfg = config = Runner.Make (struct
    module Symbol = B.Src_lang.Symbol
    module Program = B.Program

    type cfg = config

    let name = "Litmusifier"

    let tmp_file_ext = "litmus"

    let default_config = Config.default

    let run = output_litmus
  end)
end
