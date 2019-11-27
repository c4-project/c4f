(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

let early_out_name =
  Act_common.Id.of_string_list ["flow"; "dead"; "early-out"]

module Early_out_payload = struct
  type t = {path: Path.program; kind: Act_c_mini.Early_out.Kind.t}
  [@@deriving sexp, make]

  let quickcheck_path (test : Subject.Test.t) : Path.program Opt_gen.t =
    let filter = Path_filter.(empty |> in_dead_code_only) in
    Path_producers.Test.try_gen_insert_stm ~filter test

  let quickcheck_payload (test : Subject.Test.t) : t Opt_gen.t =
    Or_error.map (quickcheck_path test) ~f:(fun path_gen ->
        Base_quickcheck.Generator.Let_syntax.(
          let%bind path = path_gen in
          let%map kind = Act_c_mini.Early_out.Kind.quickcheck_generator in
          make ~path ~kind))

  let gen (test : Subject.Test.t) ~(random : Splittable_random.State.t) :
      t State.Monad.t =
    Payload.Helpers.lift_quickcheck_opt (quickcheck_payload test) ~random
      ~action_id:early_out_name
end

module Early_out : Action_types.S with type Payload.t = Early_out_payload.t =
struct
  let name = early_out_name

  let readme () =
    Act_utils.My_string.format_for_readme
      {| Inserts a valid 'early-out' statement (break or return) into a random
         dead-code location. |}

  let available (test : Subject.Test.t) : bool State.Monad.t =
    test |> Subject.Test.has_dead_code_blocks |> State.Monad.return

  module Payload = Early_out_payload

  let make_early_out (kind : Act_c_mini.Early_out.Kind.t) :
      Subject.Statement.t =
    Act_c_mini.(
      Statement.prim
        (Prim_statement.early_out
           (Early_out.make ~meta:Metadata.generated ~kind)))

  let run (test : Subject.Test.t) ~(payload : Payload.t) :
      Subject.Test.t State.Monad.t =
    let {Early_out_payload.path; kind} = payload in
    State.Monad.Monadic.return
      (Path_consumers.Test.insert_stm path ~target:test
         ~to_insert:(make_early_out kind))
end
