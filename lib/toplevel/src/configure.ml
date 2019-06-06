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
open Act_common
module C_spec = Act_compiler.Spec
module Cq_spec = Act_machine.Qualified.Compiler

let pp_compiler (verbose : bool) (f : Formatter.t)
    (compiler : C_spec.With_id.t) : unit =
  Fmt.pf f "@[<v 2>@[<h>%a@]@ %a@]" Id.pp
    (C_spec.With_id.id compiler)
    (C_spec.pp_verbose verbose)
    (C_spec.With_id.spec compiler)

let run_list_compilers (standard_args : Args.Standard.t) (_o : Output.t)
    (cfg : Act_config.Act.t) : unit Or_error.t =
  let compilers = Act_config.Act.all_compilers cfg in
  let verbose = Args.Standard.is_verbose standard_args in
  Fmt.(
    pr "@[<v>%a@]@."
      (list (using Cq_spec.c_spec (pp_compiler verbose)))
      compilers) ;
  Result.ok_unit

let list_compilers_command : Command.t =
  Command.basic
    ~summary:"outputs information about the current compiler specs"
    Command.Let_syntax.(
      let%map_open standard_args = Args.Standard.get in
      fun () ->
        Common.lift_command standard_args ~with_compiler_tests:false
          ~f:run_list_compilers)

let predicate_lists : (string, (module Property.S)) List.Assoc.t =
  [ ( "Compiler predicates (-filter-compilers)"
    , (module Act_compiler.Property) )
  ; ("Machine predicates (-filter-machines)", (module Act_machine.Property))
  ; ("Identifier predicates", (module Id.Property))
  ; ( "Sanitiser passes (-sanitiser-passes)"
    , (module Act_sanitiser.Pass_group.Selector) ) ]

let pp_tree_module : (module Property.S) Fmt.t =
 fun f (module M) -> M.pp_tree f ()

(** [pp_predicate_list] is a pretty-printer for predicate lists. *)
let pp_predicate_list : (string, (module Property.S)) List.Assoc.t Fmt.t =
  Fmt.(
    list ~sep:(unit "@,@,")
      (vbox ~indent:2
         (pair ~sep:sp (suffix (unit ":") string) pp_tree_module)))

let run_list_predicates (_o : Output.t) (_cfg : Act_config.Act.t) :
    unit Or_error.t =
  Fmt.pr "@[<v>%a@]@." pp_predicate_list predicate_lists ;
  Result.ok_unit

let list_predicates_command : Command.t =
  Command.basic ~summary:"describes the filtering predicate languages"
    Command.Let_syntax.(
      let%map_open standard_args = Args.Standard.get in
      fun () ->
        Common.lift_command standard_args ~with_compiler_tests:false
          ~f:(fun _args -> run_list_predicates))

let list_fuzzer_actions_readme () : string =
  Act_utils.My_string.format_for_readme
    {|
      Reads the current config file, and outputs information about each
      fuzzer action, including its computed weight after taking the config
      into consideration.
    |}

let fuzz_config (cfg : Act_config.Act.t) : Act_config.Fuzz.t =
  cfg |> Act_config.Act.fuzz
  |> Option.value ~default:(Act_config.Fuzz.make ())

let pp_fuzz_summaries : Act_c.Fuzzer.Action.Summary.t Id.Map.t Fmt.t =
  Id.pp_map Act_c.Fuzzer.Action.Summary.pp

let run_list_fuzzer_actions (_o : Output.t) (cfg : Act_config.Act.t) :
    unit Or_error.t =
  let open Or_error.Let_syntax in
  let fuzz = fuzz_config cfg in
  let%map summaries = Act_c.Fuzzer.summarise fuzz in
  Fmt.pr "@[<v>%a@]@." pp_fuzz_summaries summaries

let list_fuzzer_actions_command : Command.t =
  Command.basic ~summary:"outputs the current fuzzer weight table"
    ~readme:list_fuzzer_actions_readme
    Command.Let_syntax.(
      let%map_open standard_args = Args.Standard.get in
      fun () ->
        Common.lift_command standard_args ~with_compiler_tests:false
          ~f:(fun _args -> run_list_fuzzer_actions))

let command : Command.t =
  Command.group ~summary:"commands for dealing with act configuration"
    [ ("list-compilers", list_compilers_command)
    ; ("list-predicates", list_predicates_command)
    ; ("list-fuzzer-actions", list_fuzzer_actions_command) ]
