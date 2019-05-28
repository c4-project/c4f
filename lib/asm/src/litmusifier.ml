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
module Tx = Travesty_core_kernel_exts
module San = Act_sanitiser

module Format = struct
  type t = Full | Programs_only [@@deriving sexp, equal]

  let default = Full
end

module Output = struct
  type t = (unit, unit, (string, string) List.Assoc.t) San.Output.t
end

module Config = struct
  type 'const t =
    { format: Format.t [@default Format.default]
    ; postcondition: 'const Act_litmus.Ast_base.Postcondition.t option
    ; passes : Set.M(San.Pass_group).t [@default Set.empty (module San.Pass_group)]
    ; c_variables: Ac.C_variables.Map.t option }
  [@@deriving sexp, fields, make]

  let default () : 'a t = make ()

  module W = Travesty.Traversable.Helpers (Or_error)

  let transform (type a b) (initial : a t)
      ~(format : Format.t -> Format.t Or_error.t)
      ~(passes : Set.M(San.Pass_group).t -> Set.M(San.Pass_group).t Or_error.t)
      ~(postcondition :
            a Act_litmus.Ast_base.Postcondition.t
         -> b Act_litmus.Ast_base.Postcondition.t Or_error.t)
      ~(c_variables :
         Ac.C_variables.Map.t -> Ac.C_variables.Map.t Or_error.t) :
      b t Or_error.t =
    Fields.fold ~init:(Or_error.return initial)
      ~format:(W.proc_field format)
      ~passes:(W.proc_field passes)
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

  let make_litmus_program (program : (_, B.Program.t) Act_sanitiser.Output.Program.t) =
    program |> Act_sanitiser.Output.Program.listing |> B.convert_program

  let make_litmus_programs = Tx.Or_error.combine_map ~f:make_litmus_program

  let get_program_heap_symbols prog =
    prog |> Act_sanitiser.Output.Program.symbol_table
    |> Act_abstract.Symbol.(Fn.flip Table.set_of_sort Sort.Heap)

  let get_heap_symbols (programs : (_, B.Program.t) Act_sanitiser.Output.Program.t list) :
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

  let collate_warnings (programs : ('w, _) San.Output.Program.t list)
    : 'w San.Warn.t list =
    List.concat_map programs ~f:San.Output.Program.warnings

  let output_warnings (programs: (B.Src_lang.Element.t, _) San.Output.Program.t list)
      : unit =
    match collate_warnings programs with
    | [] -> ()
    | warns ->
      Fmt.(epr "@[Sanitiser warnings:@,%a@]@."
             (list ~sep:sp Sanitiser.Warn.pp)) warns

  let symbols_of_c_variables (cvs : Ac.C_variables.Map.t) :
    B.Src_lang.Symbol.t list Or_error.t =
    cvs
    |> Ac.C_id.Map.keys
    |> List.map ~f:Ac.C_id.to_string
    |> Tx.Or_error.combine_map ~f:B.Src_lang.Symbol.require_of_string

  let symbols_of_config (config : config) :
    B.Src_lang.Symbol.t list Or_error.t =
    config
    |> Config.c_variables
    |> Tx.Option.With_errors.map_m ~f:symbols_of_c_variables
    |> Or_error.map ~f:(Option.value ~default:[])

  let stringify_redirects _ = failwith "blergh"

  let strip_output (o : (_, _, _) San.Output.t) : Output.t =
    o
    |> San.Output.map_programs ~f:San.Output.Program.strip_listings
    |> San.Output.map_redirects ~f:stringify_redirects

  let run_litmus
      (ctx : config Plumbing.Filter_context.t)
      (_ic : In_channel.t)
      (oc : Out_channel.t)
    : Output.t Or_error.t =
    let config = Plumbing.Filter_context.aux ctx in
    let input = Plumbing.Filter_context.input ctx in
    let in_name = Plumbing.Input.to_string input in
    let passes = Config.passes config in
    Or_error.Let_syntax.(
      let%bind symbols = symbols_of_config config in
      let%bind program = B.Program.load_from_isrc input in
      let%bind o = Sanitiser.sanitise ~passes ~symbols program in
      let redirects = San.Output.redirects o in
      let programs = San.Output.programs o in
      output_warnings programs;
      let%map lit = make ~config ~redirects ~name:in_name ~programs in
      print_litmus (Config.format config) oc lit ;
      Out_channel.newline oc ;
      strip_output o

  module Filter : Plumbing.Filter_types.S
    with type aux_i = config
     and type aux_o = Output.t
    = Plumbing.Filter.Make (struct
    type aux_i = config
    type aux_o = Output.t

    let name = "Litmusifier"

    let tmp_file_ext = Fn.const "s.litmus"

    let run = run_filter
  end)
end

(*
module Make_fi (module B : Runner_intf.Basic) =
  ( Plumbing.Filter.Adapt (struct
      module LS = B.Src_lang
      module Lf = Make (B)
      module Original = Lf.Filter

      type aux_i = Sexp.t Config.t Job.t

      type aux_o = Output.t

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

      let adapt_o : Lf.Filter.aux_o -> aux_o Or_error.t = Or_error.return
    end)

let get_filter (module B : Runner_intf.Basic) =
  : Runner_intf.S
    with type cfg = Sexp.t Config.t
     and type aux_o = Output.t
 )
 *)
