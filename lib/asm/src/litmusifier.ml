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

open Core_kernel
include Litmusifier_intf
module Ac = Act_common
module Tx = Travesty_core_kernel_exts

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

module type Basic_aux = sig
  module Src_constant : sig
    type t
  end

  module Dst_constant : sig
    type t

    val of_int : int -> t

    val zero : t
  end

  val convert_const : Src_constant.t -> Dst_constant.t Or_error.t

  module Redirect : Ac.Redirect_map.S
end

module Make_aux (B : Basic_aux) = struct
  type t =
    { locations: Ac.C_id.t list option
    ; init: (Ac.C_id.t, B.Dst_constant.t) List.Assoc.t
    ; postcondition:
        B.Dst_constant.t Act_litmus.Ast_base.Postcondition.t option }
  [@@deriving fields, make]

  let record_to_constant (r : Ac.C_variables.Record.t) : B.Dst_constant.t =
    r |> Ac.C_variables.Record.initial_value |> Option.value ~default:0
    |> B.Dst_constant.of_int

  let make_init_from_vars (cvars : Ac.C_variables.Map.t) :
      (Ac.C_id.t, B.Dst_constant.t) List.Assoc.t =
    cvars |> Ac.C_id.Map.to_alist
    |> Tx.Alist.bi_map ~left:Fn.id ~right:record_to_constant

  let make_init_from_all_heap_symbols
      (heap_syms : Act_abstract.Symbol.Set.t) :
      (Ac.C_id.t, B.Dst_constant.t) List.Assoc.t =
    heap_syms |> Act_abstract.Symbol.Set.to_list
    |> List.map ~f:(fun s -> (Ac.C_id.of_string s, B.Dst_constant.zero))

  (** [make_init config redirects progs] makes an init block either by
      taking the information given in [config] and applying [redirects] to
      it, or by forcing the heap symbol set [heap_syms] and initialising
      each heap symbol to zero. *)
  let make_init (cvars_opt : Ac.C_variables.Map.t option)
      (heap_syms : Act_abstract.Symbol.Set.t) :
      (Ac.C_id.t, B.Dst_constant.t) List.Assoc.t =
    cvars_opt
    |> Option.map ~f:make_init_from_vars
    |> Tx.Option.value_f ~default_f:(fun () ->
           make_init_from_all_heap_symbols heap_syms )

  let make_locations_from_config (cvars : Ac.C_variables.Map.t) :
      Ac.C_id.t list =
    cvars |> Ac.C_variables.Map.globals |> Set.to_list

  let make_locations_from_init
      (init : (Ac.C_id.t, B.Dst_constant.t) List.Assoc.t) : Ac.C_id.t list =
    List.map ~f:fst init

  (** [make_locations cvars_opt init] makes a 'locations' stanza, either by
      taking the variables in [cvars_opt] and applying [redirects] to them,
      or just by taking the LHS of [init]. *)
  let make_locations (cvars_opt : Ac.C_variables.Map.t option)
      (init : (Ac.C_id.t, B.Dst_constant.t) List.Assoc.t) : Ac.C_id.t list =
    match cvars_opt with
    | Some cvars ->
        make_locations_from_config cvars
    | None ->
        make_locations_from_init init

  let make_post (_redirects : B.Redirect.t) :
         B.Src_constant.t Act_litmus.Ast_base.Postcondition.t
      -> B.Dst_constant.t Act_litmus.Ast_base.Postcondition.t Or_error.t =
    Act_litmus.Ast_base.Postcondition.On_constants.With_errors.map_m
      ~f:B.convert_const

  let is_live_symbol (heap_symbols : Act_abstract.Symbol.Set.t)
      (cid : Ac.C_id.t) =
    Act_abstract.Symbol.Set.mem heap_symbols (Ac.C_id.to_string cid)

  let live_symbols_only (heap_symbols : Act_abstract.Symbol.Set.t) :
      Ac.C_variables.Map.t -> Ac.C_variables.Map.t =
    Ac.C_id.Map.filter_keys ~f:(is_live_symbol heap_symbols)

  let live_config_variables (config : B.Src_constant.t Config.t)
      (redirects : B.Redirect.t) (heap_symbols : Act_abstract.Symbol.Set.t)
      : Ac.C_variables.Map.t option Or_error.t =
    let open Or_error.Let_syntax in
    let cvars_opt = Config.c_variables config in
    let%map redirected_cvars_opt =
      Tx.Option.With_errors.map_m cvars_opt
        ~f:(B.Redirect.transform_c_variables redirects)
    in
    Option.map ~f:(live_symbols_only heap_symbols) redirected_cvars_opt

  let make (config : B.Src_constant.t Config.t) (redirects : B.Redirect.t)
      (heap_symbols : Act_abstract.Symbol.Set.t) : t Or_error.t =
    let open Or_error.Let_syntax in
    let%bind cvars_opt =
      live_config_variables config redirects heap_symbols
    in
    let init = make_init cvars_opt heap_symbols in
    let locations = make_locations cvars_opt init in
    let src_post_opt = Config.postcondition config in
    let%map postcondition =
      Tx.Option.With_errors.map_m ~f:(make_post redirects) src_post_opt
    in
    make ~locations ~init ?postcondition ()
end

let%test_module "Aux tests" =
  ( module struct
    module Aux = Make_aux (struct
      module Src_constant = Act_language.Constant.Int_direct
      module Dst_constant = Act_language.Constant.Int_direct
      module Redirect = Act_language.Symbol.String_direct.R_map

      let convert_const = Or_error.return
    end)

    let test_init : (Ac.C_id.t, int) List.Assoc.t =
      Ac.C_id.
        [(of_string "foo", 42); (of_string "bar", 27); (of_string "baz", 53)]

    let%expect_test "make_locations_from_init: test init" =
      Stdio.print_s
        [%sexp (Aux.make_locations_from_init test_init : Ac.C_id.t list)] ;
      [%expect {| (foo bar baz) |}]

    let test_heap_symbols : Act_abstract.Symbol.Set.t =
      Act_abstract.Symbol.Set.of_list ["foo"; "barbaz"; "splink"]

    let test_global_cvars : Ac.C_variables.Map.t =
      Ac.C_variables.Map.of_single_scope_map
        ~scope:Ac.C_variables.Scope.Global
        Ac.C_id.(
          Map.of_alist_exn
            [ (of_string "foo", Some 42)
            ; (of_string "bar", Some 27)
            ; (of_string "barbaz", None)
            ; (of_string "blep", Some 63) ])

    let test_local_cvars : Ac.C_variables.Map.t =
      Ac.C_variables.Map.of_single_scope_map
        ~scope:Ac.C_variables.Scope.Local
        Ac.C_id.(
          Map.of_alist_exn
            [ (of_string "burble", Some 99)
            ; (of_string "splink", None)
            ; (of_string "herp", Some 21) ])

    let test_cvars : Ac.C_variables.Map.t =
      Ac.C_variables.Map.merge test_global_cvars test_local_cvars

    let%expect_test "make_locations_from_config: unfiltered example" =
      Stdio.print_s
        [%sexp (Aux.make_locations_from_config test_cvars : Ac.C_id.t list)] ;
      [%expect {| (bar barbaz blep foo) |}]

    let filtered_cvars : Ac.C_variables.Map.t =
      Aux.live_symbols_only test_heap_symbols test_cvars

    let%expect_test "make_locations_from_config: filtered example" =
      Stdio.print_s
        [%sexp
          (Aux.make_locations_from_config filtered_cvars : Ac.C_id.t list)] ;
      [%expect {| (barbaz foo) |}]
  end )

module Make (B : Runner.Basic) :
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
  module Sanitiser = Act_sanitiser.Instance.Make_multi (B.Sanitiser_hook)

  module Aux = Make_aux (struct
    module Src_constant = B.Src_lang.Constant
    module Dst_constant = B.Dst_lang.Constant
    module Redirect = B.Src_lang.Symbol.R_map
    module Program = Sanitiser.Output.Program

    let convert_const = B.convert_const
  end)

  let print_litmus : Format.t -> Out_channel.t -> Litmus.Validated.t -> unit
      = function
    | Full ->
        B.Litmus_pp.print
    | Programs_only ->
        B.Litmus_pp.print_programs

  let make_litmus_program (program : Sanitiser.Output.Program.t) =
    program |> Sanitiser.Output.Program.listing |> B.convert_program

  let make_litmus_programs = List.map ~f:make_litmus_program

  let get_program_heap_symbols prog =
    prog |> Sanitiser.Output.Program.symbol_table
    |> Act_abstract.Symbol.(Fn.flip Table.set_of_sort Sort.Heap)

  let get_heap_symbols (programs : Sanitiser.Output.Program.t list) :
      Act_abstract.Symbol.Set.t =
    programs
    |> List.map ~f:get_program_heap_symbols
    |> Act_abstract.Symbol.Set.union_list

  let make ~(config : B.Src_lang.Constant.t Config.t)
      ~(redirects : B.Src_lang.Symbol.R_map.t) ~(name : string)
      ~(programs : program list) =
    let open Or_error.Let_syntax in
    let heap_symbols = get_heap_symbols programs in
    let%bind {init; locations; postcondition} =
      Aux.make config redirects heap_symbols
    in
    let l_programs = make_litmus_programs programs in
    Or_error.tag ~tag:"Couldn't build litmus file."
      (Litmus.Validated.make ~name ~init ~programs:l_programs ?postcondition
         ?locations ())

  module LS = B.Src_lang

  let collate_warnings (programs : Sanitiser.Output.Program.t list) =
    List.concat_map programs ~f:Sanitiser.Output.Program.warnings

  let output_litmus (_osrc : Act_utils.Io.Out_sink.t) (outp : Out_channel.t)
      ~(in_name : string) ~(program : LS.Program.t)
      ~(symbols : LS.Symbol.t list) ~(config : config)
      ~(passes : Act_config.Sanitiser_pass.Set.t) : Job.Output.t Or_error.t
      =
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
      (LS.Symbol.R_map.to_string_alist redirects)
      warnings

  module Filter : Runner.S with type cfg = config = Runner.Make (struct
    module Symbol = B.Src_lang.Symbol
    module Program = B.Program

    type cfg = config

    let name = "Litmusifier"

    let tmp_file_ext = "litmus"

    let default_config = Config.default

    let run = output_litmus
  end)
end

let get_filter (module B : Runner.Basic) =
  ( module struct
    type cfg = Sexp.t Config.t

    module LS = B.Src_lang

    include Act_utils.Filter.Adapt (struct
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
  : Runner.S
    with type cfg = Sexp.t Config.t )
