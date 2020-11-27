(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel
module Ac = Act_common
module Au = Act_utils

module Spec = struct
  module M = Plumbing.Jsonable.Alist.Make (String) (Act_delitmus.Aux)
  include M

  module Load : Plumbing.Loadable_types.S with type t := t =
    Plumbing.Loadable.Of_jsonable (M)

  include Load
end

let validate_path_and_file : (Fpath.t * Fpath.t) Validate.check =
  Validate.(
    pair
      ~fst:
        (booltest Fpath.is_dir_path ~if_false:"path should be a directory")
      ~snd:(booltest Fpath.is_file_path ~if_false:"file should be a file"))

let to_full_path ~(dir : Fpath.t) ~(file : Fpath.t) : Fpath.t Or_error.t =
  Or_error.(
    Validate.valid_or_error (dir, file) validate_path_and_file
    >>| Tuple2.uncurry Fpath.append)

let regress_on_file ~(dir : Fpath.t) (file : Fpath.t)
    ~(f : file:Fpath.t -> path:Fpath.t -> unit Or_error.t) : unit Or_error.t
    =
  Fmt.pr "## %a\n\n```@." Fpath.pp file ;
  Out_channel.flush stdout ;
  Or_error.Let_syntax.(
    let%bind path = to_full_path ~dir ~file in
    let%map () = f ~path ~file in
    printf "```\n")

let regress_on_files (bin_name : string) ~(dir : Fpath.t) ~(ext : string)
    ~(f : file:Fpath.t -> path:Fpath.t -> unit Or_error.t) : unit Or_error.t
    =
  Or_error.Let_syntax.(
    printf "# %s tests\n\n" bin_name ;
    let%bind test_files =
      Au.Fs.Unix.get_files ~predicate:(Fpath.has_ext ext) dir
    in
    let results = List.map test_files ~f:(regress_on_file ~dir ~f) in
    let%map () = Or_error.combine_errors_unit results in
    printf "\nRan %d test(s).\n" (List.length test_files))

let make_regress_command (regress_function : Fpath.t -> unit Or_error.t)
    ~(summary : string) : Command.t =
  Command.basic ~summary
    Command.Let_syntax.(
      let%map_open test_path_raw = anon ("TEST_PATH" %: string) in
      fun () ->
        let o = Ac.Output.make ~verbose:false ~warnings:true in
        Or_error.(
          test_path_raw |> Plumbing.Fpath_helpers.of_string
          >>= regress_function)
        |> Ac.Output.print_error o)
