(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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

open Core_kernel
open Lib
open Utils

type target =
  [ `Spec of Compiler.Spec.With_id.t
  | `Arch of Id.t
  ]
;;

type file_type =
  [`Assembly | `C | `C_litmus | `Infer]
;;

let warn_if_not_tracking_symbols (o : Output.t) = function
  | [] ->
    Format.fprintf o.wf
      "@[%a@]@."
      (Format.pp_print_list ~pp_sep:Format.pp_print_space String.pp)
      [ "The set of known C variables is empty."
      ; "This can lead to `act` making incorrect assumptions;"
      ; "for example, it may fail to work out which assembly symbols"
      ; "refer to heap locations."
      ; "To fix this, specify `-cvars 'symbol1,symbol2,etc'`."
      ]
  | _ :: _ -> ()
;;

let is_c (src : Io.In_source.t)
  : [> `C | `Infer] -> bool = function
  | `C -> true
  | `Infer -> Option.exists (Io.In_source.file_type src)
                ~f:(String.equal "c")
  | _ -> false
;;

let is_c_litmus (src : Io.In_source.t)
  : [> `C_litmus | `Infer] -> bool = function
  | `C_litmus -> true
  | `Infer -> Option.exists (Io.In_source.file_type src)
                ~f:(String.equal "litmus")
  | _ -> false
;;

let get_target cfg = function
  | `Id id ->
    let open Or_error.Let_syntax in
    let%map spec = Compiler.Spec.Set.get (Lib.Config.M.compilers cfg) id in
    `Spec spec
  | `Arch _ as arch -> Or_error.return arch
;;

let arch_of_target : target -> Id.t = function
  | `Spec spec -> Compiler.Spec.With_id.emits spec
  | `Arch arch -> arch
;;

let asm_runner_of_target : target -> (module Asm_job.Runner) Or_error.t = function
  | `Spec spec -> Language_support.asm_runner_from_spec spec
  | `Arch arch -> Language_support.asm_runner_from_arch arch
;;

let ensure_spec : [> `Spec of Compiler.Spec.With_id.t]
  -> Compiler.Spec.With_id.t Or_error.t = function
    | `Spec spec -> Or_error.return spec
    | _ ->
      Or_error.error_string
        "To handle C files, you must supply a compiler ID."
;;

module Chain_with_compiler
    (Comp : Filter.S with type aux_i = unit and type aux_o = unit)
    (Onto : Filter.S)
  : Filter.S with type aux_i = (file_type * Onto.aux_i)
              and type aux_o = (unit option * Onto.aux_o) =
  Filter.Chain_conditional_first (struct
    module First  = Comp
    module Second = Onto
    type aux_i_combi = (file_type * Onto.aux_i)

    let select { Filter.aux = (file_type, rest); src; _ } =
      if is_c src file_type then `Both ((), rest) else `One rest
  end)
;;

let chain_with_compiler
  (type aux_i)
  (type aux_o)
  (target : target)
  (module Onto : Filter.S with type aux_i = aux_i and type aux_o = aux_o)
  : ( module Filter.S with type aux_i = (file_type * aux_i)
                       and type aux_o = (unit option * aux_o)
    ) Or_error.t =
  let open Result.Let_syntax in
  let%bind cspec = ensure_spec target in
  let%map (module C) = Language_support.compiler_filter_from_spec cspec in
  (module Chain_with_compiler (C) (Onto)
     : Filter.S with type aux_i = (file_type * aux_i)
                 and type aux_o = (unit option * aux_o)
  )
;;

module Chain_with_delitmus
    (Onto  : Filter.S)
  : Filter.S with type aux_i = (file_type * Onto.aux_i)
              and type aux_o = (unit option * Onto.aux_o) =
  Filter.Chain_conditional_first (struct
    module First  = C.Filters.Litmus
    module Second = Onto
    type aux_i_combi = (file_type * Onto.aux_i)

    let select { Filter.aux = (file_type, rest); src; _ } =
      if is_c_litmus src file_type
      then `Both (C.Filters.Delitmus, rest)
      else `One  rest
  end)
;;

let chain_with_delitmus
  (type aux_i)
  (type aux_o)
  (module Onto : Filter.S with type aux_i = aux_i and type aux_o = aux_o)
  : ( module Filter.S with type aux_i = (file_type * aux_i)
                       and type aux_o = (unit option * aux_o)
    ) =
  (module Chain_with_delitmus (Onto)
     : Filter.S with type aux_i = (file_type * aux_i)
                 and type aux_o = (unit option * aux_o)
  )
;;

let lift_command
    ?compiler_predicate
    ?machine_predicate
    ?sanitiser_passes
    ?with_compiler_tests
    ~f
    standard_args
  =
  let o =
    Output.make
      ~verbose:(Standard_args.is_verbose standard_args)
      ~warnings:(Standard_args.are_warnings_enabled standard_args)
  in
  Or_error.(
    (Standard_args.config_file standard_args)
    |> Io.fpath_of_string
    >>= Language_support.load_and_process_config
      ?compiler_predicate
      ?machine_predicate
      ?sanitiser_passes
      ?with_compiler_tests
    >>= f o
  ) |> Output.print_error o
;;
