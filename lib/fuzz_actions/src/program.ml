(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module F = Act_fuzz
end

module Make_empty : F.Action_types.S with type Payload.t = unit = struct
  let name = Act_common.Id.of_string "program.make.empty"

  let readme () =
    Act_utils.My_string.format_for_readme
      {|
    Generates a new, empty program at one end of the program list.
    This action isn't very useful on its own, but works well in combination
    with other actions that construct statements and control flows.
    |}

  module Payload = F.Payload.None

  let available (ctx : F.Availability.Context.t) : bool Or_error.t =
    let subject = F.Availability.Context.subject ctx in
    let param_map = F.Availability.Context.param_map ctx in
    Or_error.Let_syntax.(
      let%map cap = F.Param_map.get_thread_cap param_map in
      List.length (Act_litmus.Test.Raw.threads subject) < cap)

  let run (subject : F.Subject.Test.t) ~(payload : Payload.t) :
      F.Subject.Test.t F.State.Monad.t =
    ignore payload ;
    F.State.Monad.return (F.Subject.Test.add_new_thread subject)
end

module Label :
  F.Action_types.S
    with type Payload.t = Act_common.C_id.t F.Payload.Insertion.t = struct
  let name = Act_common.Id.of_string "program.label"

  let available : F.Availability.t = F.Availability.has_threads

  let readme () =
    Act_utils.My_string.format_for_readme
      {| Inserts a new, random label into the program. |}

  module Payload = F.Payload.Insertion.Make (struct
    type t = Act_common.C_id.t [@@deriving sexp]

    let name = name

    let path_filter = F.State.Monad.return F.Path_filter.empty

    let gen (_ : F.Path.Test.t) (_ : F.Subject.Test.t)
        ~(random : Splittable_random.State.t) ~(param_map : F.Param_map.t) :
        t F.State.Monad.t =
      ignore param_map ;
      F.State.Monad.with_labels_m (fun labels ->
          F.Payload.Helpers.lift_quickcheck
            (F.Label.gen_fresh labels)
            ~random)
  end)

  let run (subject : F.Subject.Test.t) ~(payload : Payload.t) :
      F.Subject.Test.t F.State.Monad.t =
    let path = F.Payload.Insertion.where payload in
    let name = F.Payload.Insertion.to_insert payload in
    let tid = F.Path.Test.tid path in
    let lid = Act_common.Litmus_id.local tid name in
    let label_stm =
      Act_fir.(
        name |> Prim_statement.label |> Statement.prim F.Metadata.generated)
    in
    F.State.Monad.(
      Let_syntax.(
        let%bind () = register_label lid in
        Monadic.return
          (F.Path_consumers.consume subject ~path
             ~action:(Insert [label_stm]))))
end
