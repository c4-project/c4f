(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module M = struct
  type t = Insert | Transform | Transform_list [@@deriving enum]

  let table : (t, string) List.Assoc.t =
    [ (Insert, "insert")
    ; (Transform, "transform")
    ; (Transform_list, "transform_list") ]
end

include M
include Act_utils.Enum.Extend_table (M)

module With_action = struct
  type 'a transformer = 'a -> 'a Or_error.t

  type t =
    | Insert of Subject.Statement.t list
    | Transform of Subject.Statement.t transformer
    | Transform_list of Subject.Statement.t list transformer

  let to_kind : t -> M.t = function
    | Insert _ ->
        Insert
    | Transform _ ->
        Transform
    | Transform_list _ ->
        Transform_list
end
