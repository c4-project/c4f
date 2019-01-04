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

open Core
open Lib

let run_list_compilers
    (standard_args : Standard_args.t) (_o : Output.t) (cfg : Lib.Config.M.t)
  : unit Or_error.t =
  let compilers = Lib.Config.M.compilers cfg in
  let verbose = Standard_args.is_verbose standard_args in
  Fmt.pr "@[<v>%a@]@." (Compiler.Spec.Set.pp_verbose verbose) compilers;
  Result.ok_unit
;;

let list_compilers_command : Command.t =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"outputs information about the current compiler specs"
    [%map_open
      let standard_args = Standard_args.get in
      fun () ->
        Common.lift_command standard_args
          ~with_compiler_tests:false
          ~f:(run_list_compilers standard_args)
    ]
;;

let predicate_lists : (string, (module Property.S)) List.Assoc.t =
  [ "Compiler predicates (-filter-compilers)", (module Compiler.Property)
  ; "Machine predicates (-filter-machines)", (module Machine.Property)
  ; "Identifier predicates", (module Id.Property)
  ; "Sanitiser passes (-sanitiser-passes)", (module Sanitiser_pass.Selector)
  ]
;;

let pp_tree_module : (module Property.S) Fmt.t =
  fun f (module M) -> M.pp_tree f ()
;;

let run_list_predicates
    (_o : Output.t) (_cfg : Lib.Config.M.t)
  : unit Or_error.t =
  Fmt.(
    pr "@[<v>%a@]@."
      (list ~sep:(unit "@,@,")
         (vbox ~indent:2
            (pair ~sep:sp (suffix (unit ":") string) pp_tree_module)
         )
      )
      predicate_lists
  );
  Result.ok_unit
;;

let list_predicates_command : Command.t =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"describes the filtering predicate languages"
    [%map_open
      let standard_args = Standard_args.get in
      fun () ->
        Common.lift_command standard_args
          ~with_compiler_tests:false
          ~f:run_list_predicates
    ]
;;

let command : Command.t =
  Command.group
    ~summary:"Commands for dealing with act configuration"
    [ "list-compilers", list_compilers_command
    ; "list-predicates", list_predicates_command
    ]
;;
