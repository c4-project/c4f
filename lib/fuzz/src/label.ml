(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Ac = Act_common
end

module Label = struct
  module M = struct
    (* As is oft the case in ACT, this weird 'module M' wrapper serves to set
       up the Comparable functor call later. *)
    type t = Metadata.t Act_c_mini.Label.t [@@deriving compare, equal, sexp]
  end

  include M
  include Comparable.Make (M)
end
include Label

let labels_of_thread (thread : unit Act_c_mini.Function.t Ac.C_named.t)
  : t list =
  ignore thread;
  (*|> C_named.value
  |> Act_c_mini.Function.body_stms
  |> List.filter *)
  []

let labels_of_test (test : Act_c_mini.Litmus.Test.t) :
    (t, comparator_witness) Set.t =
  test
  |> Act_c_mini.Litmus.Test.threads
  |> List.concat_map ~f:labels_of_thread
  |> Set.of_list (module Label)
