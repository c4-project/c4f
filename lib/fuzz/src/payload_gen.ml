(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Context = struct
  type t =
    { action_id: Act_common.Id.t
    ; actx: Availability.Context.t
    ; random: Splittable_random.State.t }
  [@@deriving accessors, make]

  let state = [%accessor actx @> Availability.Context.state]

  let params = [%accessor actx @> Availability.Context.params]
end

include Utils.Reader.Fix_context (Act_utils.Reader.With_errors) (Context)

let lift_acc acc = lift (Accessor.get acc)

let lift_state (f : State.t -> 'a) : 'a t =
  lift (fun {actx= {state; _}; _} -> f state)

let lift_opt_gen (type a) (g : a Opt_gen.t) : a t =
  Inner.lift (fun {action_id; random; _} ->
      let g' =
        Or_error.tag_s g
          ~tag:
            [%message
              "Payload generator instantiation failed."
                ~action_id:(action_id : Act_common.Id.t)]
      in
      (* TODO(@MattWindsor91): size? *)
      Or_error.(g' >>| Base_quickcheck.Generator.generate ~size:10 ~random))

let lift_quickcheck (type a) (g : a Base_quickcheck.Generator.t) : a t =
  lift_opt_gen (Ok g)

let path_with_flags (kind : Path_kind.t) ~(filter : Path_filter.t) :
    Path.t Path_flag.Flagged.t t =
  lift_acc (Context.actx @> Availability.Context.subject)
  >>| Path_producers.try_gen_with_flags ~filter ~kind
  >>= lift_opt_gen

let flag (id : Act_common.Id.t) : bool t =
  let* param_map = lift_acc Context.params in
  let* random = lift_acc Context.random in
  let+ f = Inner.return (Param_map.get_flag param_map ~id) in
  Flag.eval f ~random

let vars : Var.Map.t t = lift_acc (Context.state @> State.vars)

let fresh_var ?(such_that : (Act_common.Litmus_id.t -> bool) option)
    (scope : Act_common.Scope.t) : Act_common.Litmus_id.t t =
  let* v = vars in
  let cgen = Var.Map.gen_fresh_var v in
  let gen' =
    Base_quickcheck.Generator.(
      match such_that with
      | None ->
          map cgen ~f:(fun id -> Act_common.Litmus_id.make ~id ~scope)
      | Some f ->
          filter_map cgen ~f:(fun id ->
              let id' = Act_common.Litmus_id.make ~id ~scope in
              Option.some_if (f id') id'))
  in
  lift_quickcheck gen'
