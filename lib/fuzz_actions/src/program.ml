(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Make_empty : Fuzz.Action_types.S with type Payload.t = unit = struct
  let name = Common.Id.of_string "program.make.empty"

  let readme () =
    Act_utils.My_string.format_for_readme
      {|
    Generates a new, empty program at one end of the program list.
    This action isn't very useful on its own, but works well in combination
    with other actions that construct statements and control flows.
    |}

  module Payload = Fuzz.Payload_impl.None

  let available : Fuzz.Availability.t =
    Fuzz.Availability.M.Inner.lift (fun ctx ->
        let subject = Fuzz.Availability.Context.subject ctx in
        let param_map = Fuzz.Availability.Context.param_map ctx in
        Or_error.Let_syntax.(
          let%map cap = Fuzz.Param_map.get_thread_cap param_map in
          List.length (Act_litmus.Test.Raw.threads subject) < cap))

  let run (subject : Fuzz.Subject.Test.t) ~(payload : Payload.t) :
      Fuzz.Subject.Test.t Fuzz.State.Monad.t =
    ignore payload ;
    Fuzz.State.Monad.return (Fuzz.Subject.Test.add_new_thread subject)
end

module Label :
  Fuzz.Action_types.S
    with type Payload.t = Common.C_id.t Fuzz.Payload_impl.Pathed.t = struct
  let name = Common.Id.of_string "program.label"

  let available : Fuzz.Availability.t = Fuzz.Availability.has_threads

  let readme () =
    Act_utils.My_string.format_for_readme
      {| Inserts a new, random label into the program. |}

  module Payload = struct
    type t = Common.C_id.t Fuzz.Payload_impl.Pathed.t [@@deriving sexp]

    let path_filter _ = Fuzz.Path_filter.empty

    let gen' (_ : Fuzz.Path.Flagged.t) : Common.C_id.t Fuzz.Payload_gen.t =
      Fuzz.Payload_gen.(
        let* labels = lift (Fn.compose Fuzz.State.labels Context.state) in
        lift_quickcheck (Fuzz.Label.gen_fresh labels))

    let gen = Fuzz.Payload_impl.Pathed.gen Insert path_filter gen'
  end

  let run (test : Fuzz.Subject.Test.t) ~(payload : Payload.t) :
      Fuzz.Subject.Test.t Fuzz.State.Monad.t =
    let path = payload.where in
    let name = payload.payload in
    let tid = Fuzz.Path.tid path.path in
    let lid = Common.Litmus_id.local tid name in
    let label_stm =
      Act_fir.(
        name
        |> Accessor.construct Prim_statement.label
        |> Fuzz.Subject.Statement.make_generated_prim)
    in
    Fuzz.State.Monad.(
      Let_syntax.(
        let%bind () = register_label lid in
        Monadic.return
          (Fuzz.Path_consumers.consume_with_flags test ~path
             ~action:(Insert [label_stm]))))
end
