(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ac = Act_common
module Am = Act_machine

module Spec_sets = struct
  let localhost_herd_spec : Act_sim.Spec.t Lazy.t =
    lazy (Act_sim.Spec.make ~cmd:"herd7" ~style:(Ac.Id.of_string "herd") ())

  let localhost_sims : Act_sim.Spec.Set.t Lazy.t =
    Lazy.Let_syntax.(
      let%map localhost_herd_spec = localhost_herd_spec in
      Or_error.ok_exn
        (Act_sim.Spec.Set.of_list
           [ Act_sim.Spec.With_id.make ~id:(Ac.Id.of_string "herd")
               ~spec:localhost_herd_spec ]))

  let localhost_spec : Am.Spec.t Lazy.t =
    Lazy.Let_syntax.(
      let%bind sims = localhost_sims in
      let%map compilers =
        Act_compiler_test.Data.Spec_sets.single_gcc_compiler
      in
      Am.Spec.make ~enabled:true ~via:Am.Via.local ~compilers ~sims ())

  let single_local_machine : Am.Spec.Set.t Lazy.t =
    Lazy.Let_syntax.(
      let%map localhost_spec = localhost_spec in
      Or_error.ok_exn
        (Am.Spec.Set.of_list
           [ Am.Spec.With_id.make
               ~id:(Ac.Id.of_string "localhost")
               ~spec:localhost_spec ]))
end
