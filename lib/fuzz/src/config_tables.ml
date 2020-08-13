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
  module Ac = Act_common
end

let forbid_already_written_flag : Ac.Id.t =
  Ac.Id.("store" @: "forbid-already-written" @: empty)

let unsafe_weaken_orders_flag : Ac.Id.t =
  Ac.Id.("mem" @: "unsafe-weaken-orders" @: empty)

let make_global_flag : Ac.Id.t = Ac.Id.("var" @: "make" @: "global" @: empty)

let make_param_spec_map (xs : (Ac.Id.t, 'a Param_spec.t) List.Assoc.t) :
    'a Param_spec.t Map.M(Ac.Id).t =
  xs |> Map.of_alist_exn (module Ac.Id)

let param_map : Param_spec.Int.t Map.M(Ac.Id).t Lazy.t =
  lazy
    (make_param_spec_map
       [ ( Ac.Id.("cap" @: "actions" @: empty)
         , Param_spec.make ~default:40
             ~description:
               {|
              Caps the number of action passes that the fuzzer will run.
            |}
         )
       ; ( Ac.Id.("cap" @: "threads" @: empty)
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

let flag_map : Param_spec.Bool.t Map.M(Ac.Id).t Lazy.t =
  lazy
    (make_param_spec_map
       [ ( forbid_already_written_flag
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
       ; ( unsafe_weaken_orders_flag
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
               If 'true', the Make action generates global variables; if not, it
               generates local variables over the range of current threads.

               To permit both global and local variable generation, this should be an inexact flag.
|}
         ) ])
