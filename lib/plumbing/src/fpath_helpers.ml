(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

let of_string (s : string) : Fpath.t Or_error.t =
  Result.map_error (Fpath.of_string s) ~f:(function `Msg s ->
      Error.of_string s )

let lift_str (s : string option) ~(f : Fpath.t -> 'a) ~(default : 'a) :
    'a Or_error.t =
  match s with
  | None -> Or_error.return default
  | Some s -> Or_error.(s |> of_string >>| f)

let of_string_option : string option -> Fpath.t option Or_error.t =
  lift_str ~f:Option.some ~default:None

let filename_no_ext (f : Fpath.t) : string =
  Fpath.(filename (rem_ext ~multi:true f))
