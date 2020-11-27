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
  module Tx = Travesty_base_exts
end

let print_aux : Act_delitmus.Output.t -> unit =
  Fmt.(
    pr
      "@[<v>@ /* --Begin auxiliary output--@ @ %a@ @ --End auxiliary \
       output-- */@]@."
      (using Act_delitmus.Output.aux Act_delitmus.Aux.pp))

let summarise_config (config : Act_delitmus.Config.t) : unit =
  Act_delitmus.Config.(
    printf "//\n// style: %s\n// qualify-locals: %b\n// suffix: %s\n//\n\n"
      (Style.to_string (style config))
      (qualify_locals config)
      (Option.value (impl_suffix config) ~default:"(none)"))

let delitmus_file_with_config (config : Act_delitmus.Config.t)
    ~(path : Fpath.t) : unit Or_error.t =
  summarise_config config ;
  Or_error.Let_syntax.(
    let%map output =
      Act_delitmus.Filter.run ~config
        (Plumbing.Input.of_fpath path)
        Plumbing.Output.stdout
    in
    print_aux output)

let configs_to_try : Act_delitmus.Config.t list Lazy.t =
  lazy
    (let pairs =
       List.cartesian_product
         (List.cartesian_product Act_delitmus.Config.Style.all Bool.all)
         [None; Some "_body"]
     in
     List.map pairs ~f:(fun ((style, qualify_locals), impl_suffix) ->
         Act_delitmus.Config.make ~style ~qualify_locals ?impl_suffix ()))

let delitmus_file ~(file : Fpath.t) ~(path : Fpath.t) : unit Or_error.t =
  ignore file ;
  Tx.Or_error.combine_map_unit
    (Lazy.force configs_to_try)
    ~f:(delitmus_file_with_config ~path)

let run (test_dir : Fpath.t) : unit Or_error.t =
  let full_dir = Fpath.(test_dir / "litmus" / "") in
  Common.regress_on_files "Delitmus" ~dir:full_dir ~ext:"litmus"
    ~f:delitmus_file

let command =
  Common.make_regress_command ~summary:"runs delitmusifier regressions" run

let () = Command.run command
