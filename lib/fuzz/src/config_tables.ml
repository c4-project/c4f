(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
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

let var_make (rest : Common.Id.t) : Common.Id.t =
  Common.Id.("var" @: "make" @: rest)

let var_cap_param : Common.Id.t = Common.Id.(var_make ("cap" @: empty))

let unsafe_weaken_orders_flag : Common.Id.t =
  Common.Id.("mem" @: "unsafe-weaken-orders" @: empty)

let make_global_flag : Common.Id.t = Common.Id.(var_make ("global" @: empty))

let wrap_early_out_flag : Common.Id.t =
  Common.Id.("dead" @: "early-out-loop-end" @: "wrap" @: empty)

let action_enable_flag : Common.Id.t =
  Common.Id.("action" @: "enable" @: empty)

let extra_action_flag : Common.Id.t =
  Common.Id.("action" @: "pick-extra" @: empty)

let accept_recommendation_flag : Common.Id.t =
  Common.Id.("action" @: "recommendation" @: "accept" @: empty)

let use_recommendation_flag : Common.Id.t =
  Common.Id.("action" @: "recommendation" @: "use" @: empty)

let make_param_spec_map (xs : (Common.Id.t, 'a Param_spec.t) List.Assoc.t) :
    'a Param_spec.t Map.M(Common.Id).t =
  xs |> Map.of_alist_exn (module Common.Id)

let resource_cap_description ?(caveat : string option)
    ?(details : string option) (plural : string) : string =
  Printf.sprintf
    {| Caps the maximum number of %s%s that the fuzzer can construct during a
       fuzz run.

       If the input to the fuzzer already has more %s than the cap, no more
       %s will be created, but no %s will be removed to meet the cap.%s|}
    plural
    (Option.value_map ~f:(fun x -> "(" ^ x ^ ")") ~default:"" caveat)
    plural plural plural
    (Option.value_map ~f:(fun x -> "\n\n" ^ x) ~default:"" details)

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
         , Param_spec.make ~default:1000
             ~description:
               {|
              Caps the number of action passes that the fuzzer will run,
              including any extra actions.
            |}
         )
       ; ( thread_cap_param
         , Param_spec.make ~default:16
             ~description:
               (resource_cap_description "threads"
                  ~details:
                    "If targeting Litmus7, consider capping to the number \
                     of logical cores in the target machine." ) )
       ; ( var_cap_param
         , Param_spec.make ~default:20
             ~description:
               (resource_cap_description "variables"
                  ~caveat:"across all scopes"
                  ~details:
                    "If targeting particular ISA simulators, consider \
                     capping to the number of parameters that can be passed \
                     by register in that ISA." ) ) ] )

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
           (* Wrapping is probably more fun for compilers than not
              wrapping. *)
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
             ~default:(Or_error.ok_exn (Flag.try_make ~wins:100 ~losses:1))
             ~description:
               (String.concat
                  [ {|
            Checked every time the fuzzer considers taking an additional
            action (between |}
                  ; C4f_common.Id.to_string action_cap_lower_param
                  ; " and "
                  ; C4f_common.Id.to_string action_cap_upper_param
                  ; {|.
            If 'true', the fuzzer will take the action; if false, it will
            abandon doing so.

            This should usually be an inexact flag, to induce a geometric
            distribution on the number of extra actions.
            |}
                  ] ) )
       ; ( accept_recommendation_flag
         , Param_spec.make
             ~default:(Or_error.ok_exn (Flag.try_make ~wins:1 ~losses:1))
             ~description:
               {|
             When the fuzzer receives recommended actions from an action,
             it evaluates this flag for each recommendation, and pushes the
             recommended action into its recommendation queue if the flag is
             'true'.

             Making this flag inexact, as it is by default, means that the
             fuzzer will sometimes drop recommendations.
             |}
         )
       ; ( use_recommendation_flag
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
         )
       ; ( action_enable_flag
         , Param_spec.make
             ~default:(Or_error.ok_exn (Flag.try_make ~wins:1 ~losses:1))
             ~description:
               {| The fuzzer checks this flag once for each action before
                  starting; if the flag is false, the action is globally
                  and permanently disabled.

                  If all actions are disabled, the fuzzer will not perform any
                  fuzzing.

                  Making this flag inexact, as it is by default, means that the
                  fuzzer will randomly choose some subset of the available
                  actions.
              |}
         ) ] )
