(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Tx = Travesty_base_exts

module Aux = struct
  type t = { litmus_aux : Sexp.t Act_litmus.Aux.t
           ; c_variables : Act_common.C_variables.Map.t
           } [@@deriving make, fields]

  (* TODO(@MattWindsor91): validate litmus locations/etc against the
     variable map. *)

  let symbols (aux : t) : string list =
    aux
    |> c_variables
    |> Act_common.C_variables.Map.vars_satisfying ~f:Tx.Fn.always
    |> Set.to_list
    |> List.map ~f:Act_common.C_id.to_string

  let empty : t =
    { litmus_aux = Act_litmus.Aux.make ()
    ; c_variables = Or_error.ok_exn (Act_common.C_variables.Map.of_litmus_id_alist [])
    }
end

type t = {program: Act_c.Mini.Program.t; aux: Aux.t}
[@@deriving make, fields]


