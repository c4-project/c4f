(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Make (B : sig
    type t

    val of_fpath : Fpath.t -> t
    val to_fpath_opt : t -> Fpath.t option
    val std : unit -> t
    val std_name: string
  end) : Io_types.Common with type t := B.t = struct
  include B

  let of_fpath_opt : Fpath.t option -> t =
    Option.value_map ~f:of_fpath ~default:(std ())

  let of_string_opt (str: string option) : t Or_error.t =
    Or_error.(str |> Fpath_helpers.of_string_option >>| of_fpath_opt)

  let of_string (str : string) : t Or_error.t =
    of_string_opt (Option.some_if (String.equal str "-") str)

  let to_fpath_err (io: t) : Fpath.t Or_error.t =
    Result.of_option (to_fpath_opt io)
      ~error:(Error.createf "Expected a file, got %s" std_name)

  let to_string_opt : t -> string option =
    Fn.compose
      (Option.map ~f:Fpath.to_string)
      to_fpath_opt

  let to_string (io : t) : string =
    Option.value (to_string_opt io) ~default:("(" ^ std_name ^ ")")

  let pp : t Fmt.t = Fmt.of_to_string to_string
end
