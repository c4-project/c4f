(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Various lazily-evaluated tables used to source information about fuzzer
    configurables. *)

open Base
open Import

let action_cap_lower_param : Common.Id.t =
  Common.Id.("action" @: "cap" @: "lower" @: empty)

let action_cap_upper_param : Common.Id.t =
  Common.Id.("action" @: "cap" @: "upper" @: empty)

let thread_cap_param : Common.Id.t =
  (* TODO(@MattWindsor91): this should be given a better name, but the tester
     depends on it having this name. *)
  Common.Id.("cap" @: "threads" @: empty)

let unsafe_weaken_orders_flag : Common.Id.t =
  Common.Id.("mem" @: "unsafe-weaken-orders" @: empty)

let make_global_flag : Common.Id.t =
  Common.Id.("var" @: "make" @: "global" @: empty)

let wrap_early_out_flag : Common.Id.t =
  Common.Id.("flow" @: "dead" @: "early-out-loop-end" @: "wrap" @: empty)

let extra_action_flag : Common.Id.t =
  Common.Id.("action" @: "pick-extra" @: empty)

let take_recommendation_flag : Common.Id.t =
  Common.Id.("action" @: "take-recommendation" @: empty)

let make_param_spec_map (xs : (Common.Id.t, 'a Param_spec.t) List.Assoc.t) :
    'a Param_spec.t Map.M(Common.Id).t =
  xs |> Map.of_alist_exn (module Common.Id)

let param_map : Param_spec.Int.t Map.M(Common.Id).t Lazy.t =
  lazy
    (make_param_spec_map
       [ ( action_cap_lower_param
         , Param_spec.make ~default:20
             ~description:
               {|
              Sets the guaranteed number of action passes that the fuzzer will
              run, before any extra actions.
            |}
         )
       ; ( action_cap_upper_param
         , Param_spec.make ~default:200
             ~description:
               {|
              Caps the number of action passes that the fuzzer will run,
              including any extra actions.
            |}
         )
       ; ( thread_cap_param
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

let flag_map : Param_spec.Bool.t Map.M(Common.Id).t Lazy.t =
  lazy
    (make_param_spec_map
       [ ( unsafe_weaken_orders_flag
         , Param_spec.make ~default:(Flag.exact false)
             ~description:
               {|
              If 'true', lets actions that would normally strengthen memory
              orders weaken them too (possibly changing semantics in undesirable
              ways).
            |}
         )
       ; ( make_global_flag
         , Param_spec.make
             ~default:(Or_error.ok_exn (Flag.try_make ~wins:1 ~losses:1))
             ~description:
               {|
               If 'true', variable making actions generate global variables;
               else, they generate local variables over the range of current
               threads.

               To permit both global and local variable generation, this should be an inexact flag.
|}
         )
       ; ( wrap_early_out_flag
         , Param_spec.make
           (* Wrapping is probably more fun for compilers than not wrapping. *)
             ~default:(Or_error.ok_exn (Flag.try_make ~wins:3 ~losses:1))
             ~description:
               {|
               If 'true', loop-end early-out actions wrap the early-out
               in a truthy if statement; else, they emit the early-out directly.

               To permit both possibilities, this should be an inexact flag.
|}
         )
       ; ( extra_action_flag
         , Param_spec.make
             ~default:(Or_error.ok_exn (Flag.try_make ~wins:9 ~losses:1))
             ~description:
               (String.concat
                  [ {|
            Checked every time the fuzzer considers taking an additional
            action (between |}
                  ; Act_common.Id.to_string action_cap_lower_param
                  ; " and "
                  ; Act_common.Id.to_string action_cap_upper_param
                  ; {|.
            If 'true', the fuzzer will take the action; if false, it will
            abandon doing so.

            This should usually be an inexact flag, to induce a geometric
            distribution on the number of extra actions.
            |}
                  ]) )
       ; ( take_recommendation_flag
         , Param_spec.make
             ~default:(Or_error.ok_exn (Flag.try_make ~wins:1 ~losses:2))
             ~description:
               {|
             If 'true', then whenever there is an action at the front of the
             fuzzer's recommended actions queue, it will pick that action
             instead of making a weighted choice from the action deck.

             The fuzzer will always pick from the deck if no recommendations
             are available in the queue, regardless of the value of this flag.

             Making this flag inexact, as it is by default, means that the
             fuzzer will sometimes pick from the queue and sometimes pick from
             the deck.
             |}
         ) ])
