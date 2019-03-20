(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Core
open Utils
include Asm_job_intf

type 'cfg t =
  { config : 'cfg option
  ; passes : Config.Sanitiser_pass.Set.t [@default Config.Sanitiser_pass.standard]
  ; symbols : string list
  }
[@@deriving make]

module Litmus_config = struct
  module Format = struct
    type t =
      | Full
      | Programs_only
    [@@deriving sexp, equal]

    let default = Full
  end

  type 'const t =
    { format : Format.t [@default Format.default]
    ; postcondition : 'const Litmus.Ast_base.Postcondition.t option
    ; locations : C_identifier.t list option
    ; variable_info : Config.C_variables.Map.t option
    }
  [@@deriving sexp, equal, make]

  let default : unit -> 'a t = make
end

module Explain_config = struct
  module Format = struct
    type t =
      | Assembly
      | Detailed
    [@@deriving sexp, equal]

    let default = Assembly
  end

  type t = { format : Format.t [@default Format.default] } [@@deriving sexp, equal, make]

  let default : t = make ()
end

module Output = struct
  type t =
    { symbol_map : (string, string) List.Assoc.t
    ; warn : Formatter.t -> unit
    }
  [@@deriving fields]

  (* Overriding to get the right order. *)
  let warn (f : Formatter.t) (o : t) : unit = warn o f
end

module type Runner = sig
  type const [@@deriving sexp]

  include
    Gen_runner
    with type 'fmt inp := 'fmt t
     and type aux := Output.t
     and type lcfg := const Litmus_config.t
     and type ecfg := Explain_config.t
end

(** [make_litmus_name src] tries to make a Litmus test name
    representative of [src]. *)
let make_litmus_name (src : Io.In_source.t) : string =
  src
  |> Io.In_source.to_string
  |> Filename.basename
  |> Filename.chop_extension
  (* TODO(@MattWindsor91): a lot of this is ad-hoc
     stopgapping to make [make_litmus_name] generate things that
     the current C litmus test parser will accept; ideally we should
     just fix that parser. *)
  |> String.tr ~target:'.' ~replacement:'_'
  |> String.tr ~target:' ' ~replacement:'_'
;;

let%expect_test "make_litmus_name: multi-extension filename" =
  let result =
    Or_error.(
      "example.foo.c.litmus"
      |> Io.fpath_of_string
      >>| Io.In_source.of_fpath
      >>| make_litmus_name)
  in
  Sexp.output_hum stdout [%sexp (result : string Or_error.t)];
  [%expect {| (Ok example_foo_c) |}]
;;

module Make_runner (B : Runner_deps) : Runner with type const = B.Src_lang.Constant.t =
struct
  type const = B.Src_lang.Constant.t

  let const_of_sexp = B.Src_lang.Constant.t_of_sexp
  let sexp_of_const = B.Src_lang.Constant.sexp_of_t

  (* Shorthand for modules we use a _lot_. *)
  module L = B.Litmus_ast
  module LP = B.Litmus_pp
  module LS = B.Src_lang
  module LD = B.Dst_lang
  module MS = B.Multi_sanitiser
  module SS = B.Single_sanitiser
  module E = B.Explainer

  let parse isrc inp =
    let iname = Io.In_source.to_string isrc in
    Or_error.tag_arg
      (B.Frontend.load_from_ic ~path:iname inp)
      "Error while parsing assembly"
      iname
      String.sexp_of_t
  ;;

  module Redirect = MS.Redirect

  let resolve_location_alist_in_redirects
      (redirects : Redirect.t)
      (alist : (C_identifier.t, 'a) List.Assoc.t)
    : (C_identifier.t, 'a) List.Assoc.t Or_error.t =
    alist
    |> List.map ~f:(fun (x, y) ->
        Or_error.(x |> Redirect.resolve_id redirects >>| fun x' -> (x', y))
      )
    |> Or_error.combine_errors
  ;;

  let record_to_constant (r : Config.C_variables.Record.t) : LD.Constant.t =
    r
    |> Config.C_variables.Record.initial_value
    |> Option.value ~default:0
    |> LD.Constant.of_int
  ;;

  let make_init_from_vars
    (cvars : Config.C_variables.Map.t)
    (redirects : Redirect.t)
    : (C_identifier.t, LD.Constant.t) List.Assoc.t Or_error.t =
    Or_error.(
      cvars
      |> Map.to_alist
      |> resolve_location_alist_in_redirects redirects
      >>| Travesty.T_alist.bi_map ~left:Fn.id ~right:record_to_constant
    )

  let make_init_from_progs
      (progs : MS.Output.Program.t list) :
      (C_identifier.t, LD.Constant.t) List.Assoc.t =
    let get_hsyms prog =
      prog
      |> MS.Output.Program.symbol_table
      |> Fn.flip Abstract.Symbol.Table.set_of_sort Abstract.Symbol.Sort.Heap
    in
    let syms = Abstract.Symbol.Set.union_list (List.map ~f:get_hsyms progs) in
    List.map
      ~f:(fun s -> C_identifier.of_string s, LD.Constant.zero)
      (Abstract.Symbol.Set.to_list syms)

  (** [make_init config redirects progs] makes an init block
      either by taking the information given in [config] and
      applying [redirects] to it, or just by taking the symbol table from
      [progs] and pulling out the heap symbols. *)
  let make_init
      (config : LS.Constant.t Litmus_config.t)
      (redirects : Redirect.t)
      (progs : MS.Output.Program.t list) :
      (C_identifier.t, LD.Constant.t) List.Assoc.t Or_error.t =
    match config.variable_info with
    | Some vars -> make_init_from_vars vars redirects
    | None -> Or_error.return (make_init_from_progs progs)
  ;;

  let emit_warnings iname = function
    | [] -> Fn.const ()
    | ws ->
      let pp_warning f w = Format.fprintf f "@[<h>-@ @[<hov>%a@]@]@," MS.Warn.pp w in
      fun f ->
        Format.fprintf
          f
          "Warnings@ for@ %s:@ @[<v>%a@]@."
          iname
          (fun f -> List.iter ~f:(pp_warning f))
          ws
  ;;

  let make_output iname (symbol_map : (string, string) List.Assoc.t) warnings : Output.t =
    { symbol_map; warn = emit_warnings iname warnings }
  ;;

  let pp_for_litmus_format : Litmus_config.Format.t -> L.Validated.t Fmt.t = function
    | Full -> LP.pp
    | Programs_only -> LP.pp_programs
  ;;

  let make_litmus_program (program : MS.Output.Program.t) =
    program |> MS.Output.Program.listing |> B.convert_program
  ;;

  let make_litmus_programs = List.map ~f:make_litmus_program

  let make_litmus
      (config : LS.Constant.t Litmus_config.t)
      (redirects : Redirect.t)
      (locations : C_identifier.t list)
      (name : string)
      (output_programs : MS.Output.Program.t list)
      (postcondition : L.Postcondition.t option) =
    let open Or_error.Let_syntax in
    let%bind init = make_init config redirects output_programs in
    let programs = make_litmus_programs output_programs in
    Or_error.tag
      ~tag:"Couldn't build litmus file."
      (L.Validated.make
         ~name
         ~init
         ~programs
         ?postcondition
         ~locations
         ())
  ;;

  let collate_warnings (programs : MS.Output.Program.t list) =
    List.concat_map programs ~f:MS.Output.Program.warnings
  ;;

  let make_post (_redirects : Redirect.t)
      : LS.Constant.t Litmus.Ast_base.Postcondition.t -> L.Postcondition.t Or_error.t =
    Litmus.Ast_base.Postcondition.On_constants.With_errors.map_m ~f:B.convert_const
  ;;

  (** [make_locations_from_redirects redirects] makes a 'locations'
      stanza by taking the right-hand side of the redirects table
      [redirects] created by the sanitiser process. *)
  let make_locations_from_redirects (redirects : Redirect.t)
      : C_identifier.t list Or_error.t =
    Or_error.(
      redirects
      |> Redirect.image_ids
      >>| C_identifier.Set.to_list
    )
  ;;

  let make_locations_from_config
      (config_locs : C_identifier.t list)
      (redirects : Redirect.t) :
      C_identifier.t list Or_error.t =
    config_locs
    |> List.map ~f:(Redirect.resolve_id redirects)
    |> Or_error.combine_errors
  ;;

  (** [make_locations config redirects] makes a 'locations'
      stanza, either by taking the stanza given in [config] and
      applying [redirects] to it, or just by taking the RHS of the
      [redirects]. *)
  let make_locations
      (config : LS.Constant.t Litmus_config.t)
      (redirects : Redirect.t) :
      C_identifier.t list Or_error.t =
    match config.locations with
    | Some locs -> make_locations_from_config locs redirects
    | None -> make_locations_from_redirects redirects
  ;;

  let output_litmus
      ?(config : LS.Constant.t Litmus_config.t = Litmus_config.default ())
      (name : string)
      (passes : Config.Sanitiser_pass.Set.t)
      (symbols : LS.Symbol.t list)
      (program : LS.Program.t)
      (_osrc : Io.Out_sink.t)
      (outp : Out_channel.t) : Output.t Or_error.t =
    let open Or_error.Let_syntax in
    let%bind o = MS.sanitise ~passes ~symbols program in
    let redirects = MS.Output.redirects o in
    let programs = MS.Output.programs o in
    let warnings = collate_warnings programs in
    let%bind post =
      Travesty.T_option.With_errors.map_m config.postcondition ~f:(make_post redirects)
    in
    let%bind locations = make_locations config redirects in
    let%map lit = make_litmus config redirects locations name programs post in
    let f = Format.formatter_of_out_channel outp in
    pp_for_litmus_format config.format f lit;
    Format.pp_print_newline f ();
    let redirects = MS.Output.redirects o in
    make_output name (Redirect.to_string_alist redirects) warnings
  ;;

  let pp_for_explain_format : Explain_config.Format.t -> E.t Fmt.t = function
    | Assembly -> E.pp_as_assembly
    | Detailed -> E.pp
  ;;

  let output_explanation output_format name outp exp redirects =
    let f = Format.formatter_of_out_channel outp in
    pp_for_explain_format output_format f exp;
    Format.pp_print_newline f ();
    make_output name redirects []
  ;;

  let run_explanation
      ?(config : Explain_config.t = Explain_config.default)
      (name : string)
      (passes : Config.Sanitiser_pass.Set.t)
      (symbols : LS.Symbol.t list)
      (program : LS.Program.t)
      (_osrc : Io.Out_sink.t)
      (outp : Out_channel.t) : Output.t Or_error.t =
    let open Or_error.Let_syntax in
    let%map san = SS.sanitise ~passes ~symbols program in
    let program = SS.Output.programs san in
    let listing = SS.Output.Program.listing program in
    let s_table = SS.Output.Program.symbol_table program in
    let exp = E.explain listing s_table in
    let redirects = SS.Output.redirects san in
    output_explanation config.format name outp exp
      (SS.Redirect.to_string_alist redirects)
  ;;

  let stringify_symbol sym =
    Result.of_option
      (LS.Symbol.of_string_opt sym)
      ~error:
        (Error.create_s [%message "Symbol can't be converted from string" ~symbol:sym])
  ;;

  let stringify_symbols syms =
    syms |> List.map ~f:stringify_symbol |> Or_error.combine_errors
  ;;

  let run ~f { Filter.aux; src; sink } ic oc : Output.t Or_error.t =
    let open Result.Let_syntax in
    let name = Filename.(chop_extension (basename (Io.In_source.to_string src))) in
    let%bind asm = parse src ic in
    let%bind symbols = stringify_symbols aux.symbols in
    f ?config:aux.config name aux.passes symbols (B.program asm) sink oc
  ;;

  module Litmusify :
    Filter.S with type aux_i = LS.Constant.t Litmus_config.t t and type aux_o = Output.t =
  Filter.Make (struct
    type aux_i = LS.Constant.t Litmus_config.t t
    type aux_o = Output.t

    let name = "Litmusifier"
    let tmp_file_ext = Fn.const "litmus"
    let run = run ~f:output_litmus
  end)

  module Explain :
    Filter.S with type aux_i = Explain_config.t t and type aux_o = Output.t =
  Filter.Make (struct
    type aux_i = Explain_config.t t
    type aux_o = Output.t

    let tmp_file_ext = Fn.const "s"
    let name = "Explainer"
    let run = run ~f:run_explanation
  end)
end

let get_litmusify_sexp (module Runner : Runner) =
  (module Filter.Adapt (struct
     module Original = Runner.Litmusify

     type aux_i = Sexp.t Litmus_config.t t
     type aux_o = Output.t

     let adapt_constant (const : Sexp.t) : Runner.const Or_error.t =
       Or_error.try_with (fun () -> [%of_sexp: Runner.const] const)
     ;;

     let adapt_postcondition
         :  Sexp.t Litmus.Ast_base.Postcondition.t
         -> Runner.const Litmus.Ast_base.Postcondition.t Or_error.t =
       Litmus.Ast_base.Postcondition.On_constants.With_errors.map_m ~f:adapt_constant
     ;;

     let adapt_config (config : Sexp.t Litmus_config.t) :
         Runner.const Litmus_config.t Or_error.t =
       let open Or_error.Let_syntax in
       let%map postcondition =
         Travesty.T_option.With_errors.map_m config.postcondition ~f:adapt_postcondition
       in
       { config with postcondition }
     ;;

     let adapt_i job =
       let open Or_error.Let_syntax in
       let%map config = Travesty.T_option.With_errors.map_m job.config ~f:adapt_config in
       { job with config }
     ;;

     let adapt_o = Or_error.return
  end)
  : Filter.S
    with type aux_i = Sexp.t Litmus_config.t t
     and type aux_o = Output.t)
;;

let get_explain (module Runner : Runner) =
  (module Runner.Explain
  : Filter.S
    with type aux_i = Explain_config.t t
     and type aux_o = Output.t)
;;
