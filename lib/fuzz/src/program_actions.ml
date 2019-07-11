(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Make_empty : Action_types.S with type Random_state.t = unit = struct
  let name = Act_common.Id.of_string "program.make.empty"

  let readme () =
    Act_utils.My_string.format_for_readme
      {|
    Generates a new, empty program at one end of the program list.
    This action isn't very useful on its own, but works well in combination
    with other actions that construct statements and control flows.
    |}

  let default_weight = 1

  module Random_state = Action.No_random_state

  let available = Action.always

  let run (subject : Subject.Test.t) (() : Random_state.t) :
      Subject.Test.t State.Monad.t =
    State.Monad.return (Subject.Test.add_new_program subject)
end
