(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(** Signature of bulk (multi-file) simulation runners. *)
module type S = sig
  (** Opaque type of file maps. *)
  type file_map

  (** Bundle of inputs needed for a bulk job. *)
  module Job : sig
    type nonrec t =
      { input_paths: Fpath.t list
      ; output_path_f: Fpath.t -> Fpath.t
      ; arch: Arch.t }
  end

  val run : Job.t -> file_map Or_error.t
  (** [run job] runs the backend on a bulk job [job]. *)
end
