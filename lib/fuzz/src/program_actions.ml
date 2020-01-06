(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Make_empty : Action_types.S with type Payload.t = unit = struct
  let name = Act_common.Id.of_string "program.make.empty"

  let readme () =
    Act_utils.My_string.format_for_readme
      {|
    Generates a new, empty program at one end of the program list.
    This action isn't very useful on its own, but works well in combination
    with other actions that construct statements and control flows.
    |}

  module Payload = Payload.None

  let available (subject : Subject.Test.t) ~(param_map : Param_map.t)
    : bool State.Monad.t =
    State.Monad.Monadic.return (
      Or_error.Let_syntax.(
        let%map cap = Param_map.get_thread_cap param_map in
        List.length (Act_litmus.Test.Raw.threads subject) < cap
      )
    )

  let run (subject : Subject.Test.t) ~(payload : Payload.t) :
      Subject.Test.t State.Monad.t =
    ignore payload ;
    State.Monad.return (Subject.Test.add_new_thread subject)
end
