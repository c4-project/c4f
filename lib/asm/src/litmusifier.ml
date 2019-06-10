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
  type 'const t =
    { format: Format.t [@default Format.default]
    ; postcondition: 'const Act_litmus.Ast_base.Postcondition.t option
    ; c_variables: Ac.C_variables.Map.t option }
  [@@deriving sexp, equal, fields, make]

  let default : unit -> 'a t = make

  module W = Travesty.Traversable.Helpers (Or_error)

  let transform (type a b) (initial : a t)
      ~(format : Format.t -> Format.t Or_error.t)
      ~(postcondition :
            a Act_litmus.Ast_base.Postcondition.t
         -> b Act_litmus.Ast_base.Postcondition.t Or_error.t)
      ~(c_variables :
         Ac.C_variables.Map.t -> Ac.C_variables.Map.t Or_error.t) :
      b t Or_error.t =
    Fields.fold ~init:(Or_error.return initial)
      ~format:(W.proc_field format)
      ~postcondition:(fun x_or_error _ ->
        let open Or_error.Let_syntax in
        let%bind x = x_or_error in
        let post = x.postcondition in
        let%map post' = Tx.Option.With_errors.map_m ~f:postcondition post in
        {x with postcondition= post'} )
      ~c_variables:
        (W.proc_field (Tx.Option.With_errors.map_m ~f:c_variables))
end

module Make (B : Runner_intf.Basic) :
  S
  with type config = B.Src_lang.Constant.t Config.t
   and type fmt = Format.t
   and type program =
              ( B.Src_lang.Element.t
              , B.Src_lang.Program.t )
              Act_sanitiser.Output.Program.t
   and module Redirect := B.Src_lang.Symbol.R_map = struct
  type config = B.Src_lang.Constant.t Config.t

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

  let get_program_heap_symbols prog =
    prog |> Sanitiser.Output.Program.symbol_table
    |> Act_abstract.Symbol.(Fn.flip Table.set_of_sort Sort.Heap)

  let get_heap_symbols (programs : Sanitiser.Output.Program.t list) :
      Act_abstract.Symbol.Set.t =
    programs
    |> List.map ~f:get_program_heap_symbols
    |> Act_abstract.Symbol.Set.union_list

  let make_post :
         B.Src_lang.Constant.t Act_litmus.Ast_base.Postcondition.t
      -> B.Dst_lang.Constant.t Act_litmus.Ast_base.Postcondition.t
         Or_error.t =
    Act_litmus.Ast_base.Postcondition.On_constants.With_errors.map_m
      ~f:B.convert_const

  let make_aux (config : B.Src_lang.Constant.t Config.t)
      (redirects : B.Src_lang.Symbol.R_map.t)
      (heap_symbols : Act_abstract.Symbol.Set.t) :
      B.Dst_lang.Constant.t Litmusifier_aux.t Or_error.t =
    let cvars_opt = Config.c_variables config in
    Or_error.Let_syntax.(
      let%bind redirected_cvars_opt =
        Tx.Option.With_errors.map_m cvars_opt
          ~f:(B.Src_lang.Symbol.R_map.transform_c_variables redirects)
      in
      let%map postcondition =
        Tx.Option.With_errors.map_m ~f:make_post
          (Config.postcondition config)
      in
      Litmusifier_aux.make ?c_variables:redirected_cvars_opt ~heap_symbols
        ?postcondition ~of_int:B.Dst_lang.Constant.of_int)

  let make ~(config : B.Src_lang.Constant.t Config.t)
      ~(redirects : B.Src_lang.Symbol.R_map.t) ~(name : string)
      ~(programs : program list) =
    let heap_symbols = get_heap_symbols programs in
    Or_error.Let_syntax.(
      let%bind aux = make_aux config redirects heap_symbols in
      let init = Litmusifier_aux.init aux in
      let postcondition = Litmusifier_aux.postcondition aux in
      let locations = Litmusifier_aux.locations aux in
      let%bind l_programs = make_litmus_programs programs in
      Or_error.tag ~tag:"Couldn't build litmus file."
        (Litmus.Validated.make ~name ~init ~programs:l_programs
           ?postcondition ?locations ()))

  let collate_warnings (programs : Sanitiser.Output.Program.t list) =
    List.concat_map programs ~f:Sanitiser.Output.Program.warnings

  let output_litmus (outp : Out_channel.t) ~(in_name : string)
      ~(program : B.Src_lang.Program.t)
      ~(symbols : B.Src_lang.Symbol.t list) ~(config : config)
      ~(passes : Set.M(Act_sanitiser.Pass_group).t) :
      Job.Output.t Or_error.t =
    let open Or_error.Let_syntax in
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

let get_filter (module B : Runner_intf.Basic) =
  ( module struct
    type cfg = Sexp.t Config.t

    module LS = B.Src_lang

    include Plumbing.Filter.Adapt (struct
      module Lf = Make (B)
      module Original = Lf.Filter

      type aux_i = Sexp.t Config.t Job.t

      type aux_o = Job.Output.t

      let adapt_constant (const : Sexp.t) : LS.Constant.t Or_error.t =
        Or_error.try_with (fun () -> [%of_sexp: LS.Constant.t] const)

      let adapt_postcondition :
             Sexp.t Act_litmus.Ast_base.Postcondition.t
          -> LS.Constant.t Act_litmus.Ast_base.Postcondition.t Or_error.t =
        Act_litmus.Ast_base.Postcondition.On_constants.With_errors.map_m
          ~f:adapt_constant

      let adapt_config :
          Sexp.t Config.t -> LS.Constant.t Config.t Or_error.t =
        Config.transform ~format:Or_error.return
          ~c_variables:Or_error.return ~postcondition:adapt_postcondition

      let adapt_i :
          Sexp.t Config.t Job.t -> LS.Constant.t Config.t Job.t Or_error.t =
        Job.map_m_config ~f:adapt_config

      let adapt_o = Or_error.return
    end)
  end
  : Runner_intf.S
    with type cfg = Sexp.t Config.t )
