(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module P = Payload
end

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

  let available (ctx : Availability.Context.t) : bool Or_error.t =
    let subject = Availability.Context.subject ctx in
    let param_map = Availability.Context.param_map ctx in
    Or_error.Let_syntax.(
      let%map cap = Param_map.get_thread_cap param_map in
      List.length (Act_litmus.Test.Raw.threads subject) < cap)

  let run (subject : Subject.Test.t) ~(payload : Payload.t) :
      Subject.Test.t State.Monad.t =
    ignore payload ;
    State.Monad.return (Subject.Test.add_new_thread subject)
end

module Label :
  Action_types.S with type Payload.t = Act_common.C_id.t P.Insertion.t =
struct
  let name = Act_common.Id.of_string "program.label"

  let available = Availability.has_threads

  let readme () =
    Act_utils.My_string.format_for_readme
      {| Inserts a new, random label into the program. |}

  module Payload = P.Insertion.Make (struct
    type t = Act_common.C_id.t [@@deriving sexp]

    let name = name

    let path_filter = State.Monad.return Path_filter.empty

    let gen (_ : Path.Program.t) (_ : Subject.Test.t)
        ~(random : Splittable_random.State.t) ~(param_map : Param_map.t) :
        t State.Monad.t =
      ignore param_map ;
      State.Monad.with_labels_m (fun labels ->
          Payload.Helpers.lift_quickcheck (Label.gen_fresh labels) ~random)
  end)

  let run (subject : Subject.Test.t) ~(payload : Payload.t) :
      Subject.Test.t State.Monad.t =
    let path = P.Insertion.where payload in
    let name = P.Insertion.to_insert payload in
    let tid = Path.Program.tid path in
    let lid = Act_common.Litmus_id.local tid name in
    let label_stm =
      Act_fir.(
        name |> Prim_statement.label |> Statement.prim Metadata.generated)
    in
    State.Monad.(
      Let_syntax.(
        let%bind () = register_label lid in
        Monadic.return
          (Path_consumers.Test.insert_stm path ~to_insert:label_stm
             ~target:subject)))
end
