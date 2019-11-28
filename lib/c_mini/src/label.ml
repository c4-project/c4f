(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Mini-C: labels.

    The label structure is used both for labels and gotos. *)

open Base

type 'meta t = {meta: 'meta; name: Act_common.C_id.t}
[@@deriving fields, make, sexp, equal]

let of_c_id (name : Act_common.C_id.t) : unit t = make ~name ~meta:()

module On_meta = Travesty.Traversable.Make1 (struct
  type nonrec 'meta t = 'meta t

  module On_monad (M : Monad.S) = struct
    let map_m (type m1 m2) ({name; meta} : m1 t) ~(f : m1 -> m2 M.t) :
        m2 t M.t =
      M.(meta |> f >>| fun meta' -> {name; meta= meta'})
  end
end)
