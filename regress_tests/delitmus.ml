(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Tests the output of the 'act-c delitmus' command. *)

open Core

open struct
  module Ac = Act_common
end

let print_aux : Act_delitmus.Output.t -> unit =
  Fmt.(
    pr
      "@[<v>@ /* --Begin auxiliary output--@ @ %a@ @ --End auxiliary \
       output-- */@]@."
      (using Act_delitmus.Output.aux Act_delitmus.Aux.pp))

let delitmus_file_with_style (style : Act_delitmus.Runner.Style.t)
    ~(path : Fpath.t) : unit Or_error.t =
  printf "//\n// style: %s\n//\n\n"
    (Act_delitmus.Runner.Style.to_string style) ;
  Or_error.Let_syntax.(
    let%map output =
      Act_delitmus.Filter.run style
        (Plumbing.Input.of_fpath path)
        Plumbing.Output.stdout
    in
    print_aux output)

let delitmus_file ~(file : Fpath.t) ~(path : Fpath.t) : unit Or_error.t =
  ignore file ;
  Or_error.combine_errors_unit
    (List.map Act_delitmus.Runner.Style.all
       ~f:(delitmus_file_with_style ~path))

let run (test_dir : Fpath.t) : unit Or_error.t =
  let full_dir = Fpath.(test_dir / "litmus" / "") in
  Common.regress_on_files "Delitmus" ~dir:full_dir ~ext:"litmus"
    ~f:delitmus_file

let command =
  Common.make_regress_command ~summary:"runs delitmusifier regressions" run

let () = Command.run command
