(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

let no_make_harness (_arch : Arch.t) ~(input_path : Fpath.t)
    ~(output_dir : Fpath.t) : string list Or_error.t =
  ignore (input_path : Fpath.t) ;
  ignore (output_dir : Fpath.t) ;
  Or_error.error_string "This backend doesn't support harness making."

module Make (B : sig
  module Reader : Reader_types.S

  module Unchecked_filter : Filter.S

  val make_harness_unchecked :
       Arch.t
    -> input_path:Fpath.t
    -> output_dir:Fpath.t
    -> string list Or_error.t
end) : Runner_types.S = struct
  module Reader = B.Reader

  (* TODO(@MattWindsor91): check these! *)
  module Filter = B.Unchecked_filter

  let make_harness = B.make_harness_unchecked

  let run (arch : Arch.t) ~(input_path : Fpath.t) ~(output_path : Fpath.t) :
      Act_state.Observation.t Or_error.t =
    Or_error.Let_syntax.(
      let%bind () =
        Filter.run arch
          (Plumbing.Input.of_fpath input_path)
          (Plumbing.Output.of_fpath output_path)
      in
      B.Reader.load (Plumbing.Input.of_fpath output_path))
end

module Make_error_reader (B : sig
  val error : Error.t
end) : Reader_types.S = Plumbing.Loadable.Make (struct
  type t = Act_state.Observation.t

  let load_from_ic ?(path : string option) (_ic : Stdio.In_channel.t) :
      Act_state.Observation.t Or_error.t =
    ignore path ; Result.Error B.error

  let load_from_string (_str : string) : Act_state.Observation.t Or_error.t
      =
    Result.Error B.error
end)

module Make_error (B : sig
  val error : Error.t
end) : Runner_types.S = struct
  module Reader = Make_error_reader (B)
  module Filter = Filter.Make_error (B)

  let make_harness = no_make_harness

  (* let name : Act_common.Id.t = Act_common.Id.of_string "error"

     let machine_id : Act_common.Id.t = Act_common.Id.of_string "none" *)

  type t = Filter.aux_i

  let run (_ctx : t) ~(input_path : Fpath.t) ~(output_path : Fpath.t) :
      Act_state.Observation.t Or_error.t =
    ignore input_path ; ignore output_path ; Result.Error B.error
end
