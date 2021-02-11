(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Make_empty : Fuzz.Action_types.S with type Payload.t = unit = struct
  let name = Common.Id.of_string "program.make.empty"

  let readme =
    lazy
      {|
    Generates a new, empty program at one end of the program list.
    This action isn't very useful on its own, but works well in combination
    with other actions that construct statements and control flows.
    |}

  module Payload = Fuzz.Payload_impl.None

  let recommendations () =
    [ (* There should be quite a lot of recommendations here, eventually! *)
      Var.Make.name ]

  let available : Fuzz.Availability.t =
    Fuzz.Availability.(
      M.(
        let* cap = param Fuzz.Config_tables.thread_cap_param in
        let+ threads =
          lift_acc
            (Context.subject @> Accessor.getter C4f_litmus.Test.Raw.threads)
        in
        List.length threads < cap))

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

  let readme = lazy {| Inserts a new, random label into the program. |}

  module Payload = struct
    type t = Common.C_id.t Fuzz.Payload_impl.Pathed.t [@@deriving sexp]

    let path_filter _ = Fuzz.Path_filter.zero

    let gen' (_ : Fuzz.Path.With_meta.t) : Common.C_id.t Fuzz.Payload_gen.t =
      Fuzz.Payload_gen.(
        let* labels = lift_acc (Context.state @> Fuzz.State.labels) in
        lift_quickcheck (Fuzz.Label.gen_fresh labels))

    let gen = Fuzz.Payload_impl.Pathed.gen Insert path_filter gen'
  end

  let recommendations (_ : Payload.t) : Common.Id.t list =
    [Flow_dead.Insert.Goto.name]

  let run (test : Fuzz.Subject.Test.t) ~(payload : Payload.t) :
      Fuzz.Subject.Test.t Fuzz.State.Monad.t =
    let path = payload.where in
    let name = payload.payload in
    let tid = Fuzz.Path.tid path.path in
    let lid = Common.Litmus_id.local tid name in
    let label_stm =
      C4f_fir.(
        name
        |> Accessor.construct Prim_statement.label
        |> Fuzz.Subject.Statement.make_generated_prim)
    in
    Fuzz.State.Monad.(
      Let_syntax.(
        let%bind () = register_label lid in
        Monadic.return
          (Fuzz.Path_consumers.consume test ~path
             ~action:(Insert [label_stm]) )))
end
