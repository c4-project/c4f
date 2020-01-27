(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

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

  let available (subject : Subject.Test.t) ~(param_map : Param_map.t) :
      bool State.Monad.t =
    State.Monad.Monadic.return
      Or_error.Let_syntax.(
        let%map cap = Param_map.get_thread_cap param_map in
        List.length (Act_litmus.Test.Raw.threads subject) < cap)

  let run (subject : Subject.Test.t) ~(payload : Payload.t) :
      Subject.Test.t State.Monad.t =
    ignore payload ;
    State.Monad.return (Subject.Test.add_new_thread subject)
end

module Label_payload = struct
  type t = {path: Path.Program.t; name: Act_common.C_id.t}
  [@@deriving make, sexp]

  module PP = Payload.Program_path (struct
    let action_id = Act_common.Id.of_string "program.label"

    let gen = Path_producers.Test.try_gen_insert_stm

    let build_filter = Fn.id
  end)

  let gen (subject : Subject.Test.t) ~(random : Splittable_random.State.t)
      ~(param_map : Param_map.t) : t State.Monad.t =
    State.Monad.with_labels_m (fun labels ->
        State.Monad.Let_syntax.(
          let%map path = PP.gen subject ~random ~param_map
          and name =
            Payload.Helpers.lift_quickcheck (Label.gen_fresh labels) ~random
          in
          make ~path ~name))
end

module Label : Action_types.S with type Payload.t = Label_payload.t = struct
  let name = Act_common.Id.of_string "program.label"

  let available = Action.always

  let readme () =
    Act_utils.My_string.format_for_readme
      {| Inserts a new, random label into the program. |}

  module Payload = Label_payload

  let run (subject : Subject.Test.t) ~(payload : Payload.t) :
      Subject.Test.t State.Monad.t =
    let {Label_payload.path; name} = payload in
    let tid = Path.Program.tid path in
    let lid = Act_common.Litmus_id.local tid name in
    let label_stm =
      Act_c_mini.(
        name |> Prim_statement.label |> Statement.prim Metadata.generated)
    in
    State.Monad.(
      Let_syntax.(
        let%bind () = register_label lid in
        Monadic.return
          (Path_consumers.Test.insert_stm path ~to_insert:label_stm
             ~target:subject)))
end
