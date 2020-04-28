(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Various lazily-evaluated tables used to source information about fuzzer
    configurables. *)

open Base

open struct
  module Tx = Travesty_base_exts
end

let actions : Action.With_default_weight.t list Lazy.t =
  lazy
    Action.With_default_weight.
      [ make ~action:(module Dead_actions.Early_out) ~default_weight:20
      ; make ~action:(module Dead_actions.Goto) ~default_weight:20
      ; make ~action:(module If_actions.Invert) ~default_weight:10
      ; make
          ~action:(module If_actions.Surround.Tautology)
          ~default_weight:25
      ; make
          ~action:(module If_actions.Surround.Duplicate)
          ~default_weight:15
      ; make ~action:(module Loop_actions.Surround) ~default_weight:20
      ; make ~action:(module Mem_actions.Fence) ~default_weight:15
      ; make ~action:(module Mem_actions.Strengthen) ~default_weight:15
      ; make ~action:(module Program_actions.Make_empty) ~default_weight:10
      ; make ~action:(module Program_actions.Label) ~default_weight:15
      ; make ~action:(module Store_actions.Int) ~default_weight:30
      ; make ~action:(module Store_actions.Int_dead) ~default_weight:20
      ; make ~action:(module Store_actions.Int_redundant) ~default_weight:15
      ; make ~action:(module Var_actions.Make) ~default_weight:20 ]

let action_map : Action.With_default_weight.t Map.M(Act_common.Id).t Lazy.t =
  Lazy.(
    actions
    >>| List.map ~f:(fun a -> (Action.With_default_weight.name a, a))
    >>| Map.of_alist_exn (module Act_common.Id))

let make_param_spec_map
    (xs : (Act_common.Id.t, 'a Param_spec.t) List.Assoc.t) :
    'a Param_spec.t Map.M(Act_common.Id).t =
  xs |> Map.of_alist_exn (module Act_common.Id)

let param_map : Param_spec.Int.t Map.M(Act_common.Id).t Lazy.t =
  lazy
    (make_param_spec_map
       [ ( Act_common.Id.("cap" @: "actions" @: empty)
         , Param_spec.make ~default:30
             ~description:
               {|
              Caps the number of action passes that the fuzzer will run.
            |}
         )
       ; ( Act_common.Id.("cap" @: "threads" @: empty)
         , Param_spec.make ~default:16
             ~description:
               {|
              Caps the maximum number of threads that the fuzzer can construct
              during a fuzz run.

              If the input to the fuzzer already has more threads than the cap,
              no more threads will be created, but no threads will be removed
              to meet the cap.

              If fuzzing to target Litmus7, the cap should be set as the number
              of logical cores in the target machine.
            |}
         ) ])

let flag_map : Param_spec.Bool.t Map.M(Act_common.Id).t Lazy.t =
  (* Space for rent. *)
  lazy
    (make_param_spec_map
       [ ( Storelike.forbid_already_written_flag_key
         , Param_spec.make ~default:(Flag.exact false)
             ~description:
               {|
              If 'true', stops store actions from selecting a destination
              variable to which there already exists a write action.

              Note that, because of the limitations of the flag system, setting
              this flag to a ratio will cause any dependent actions to stop
              firing when no non-written variables exist (but, once the action
              fires, it can decide with the given probability to restrict
              itself to already-written variables).
            |}
         )
       ; ( Mem_actions.unsafe_weaken_orders_flag_key
         , Param_spec.make ~default:(Flag.exact false)
             ~description:
               {|
              If 'true', lets actions that would normally strengthen memory
              orders weaken them too (possibly changing semantics in undesirable
              ways).
            |}
         )
       ; ( Var_actions.make_global_flag_key
         , Param_spec.make
             ~default:(Or_error.ok_exn (Flag.try_make ~wins:1 ~losses:1))
             ~description:
               {|
               If 'true', the Make action generates global variables; if not, it
               generates local variables over the range of current threads.

               To permit both global and local variable generation, this should be an inexact flag.
|}
         ) ])
