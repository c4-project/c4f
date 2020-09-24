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

let unsafe_weaken_orders_flag : Common.Id.t =
  Common.Id.("mem" @: "unsafe-weaken-orders" @: empty)

let make_global_flag : Common.Id.t =
  Common.Id.("var" @: "make" @: "global" @: empty)

let wrap_early_out_flag : Common.Id.t =
  Common.Id.("flow" @: "dead" @: "early-out-loop-end" @: "wrap" @: empty)

let make_param_spec_map (xs : (Common.Id.t, 'a Param_spec.t) List.Assoc.t) :
    'a Param_spec.t Map.M(Common.Id).t =
  xs |> Map.of_alist_exn (module Common.Id)

let param_map : Param_spec.Int.t Map.M(Common.Id).t Lazy.t =
  lazy
    (make_param_spec_map
       [ ( Common.Id.("cap" @: "actions" @: empty)
         , Param_spec.make ~default:40
             ~description:
               {|
              Caps the number of action passes that the fuzzer will run.
            |}
         )
       ; ( Common.Id.("cap" @: "threads" @: empty)
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
         ) ])
