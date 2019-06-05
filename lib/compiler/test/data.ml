(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Act_compiler
module Ac = Act_common

module Spec_sets = struct
  let gcc_spec : Spec.t Lazy.t =
    lazy
      (Spec.make ~cmd:"gcc" ~style:(Ac.Id.of_string "gcc")
         ~emits:(Ac.Id.of_string "x86.att")
         ~enabled:true ())

  let single_gcc_compiler : Spec.Set.t Lazy.t =
    Lazy.Let_syntax.(
      let%map gcc_spec = gcc_spec in
      Or_error.ok_exn
        (Spec.Set.of_list
           [ Spec.With_id.make
               ~id:(Ac.Id.of_string "gcc.x86.normal")
               ~spec:gcc_spec ]))
end
