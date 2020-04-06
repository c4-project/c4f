(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

let make_global_flag_key : Act_common.Id.t =
  Act_common.Id.("var" @: "make" @: "global" @: empty)

let make_global_flag (param_map : Param_map.t) : Flag.t State.Monad.t =
  Param_map.get_flag_m param_map ~id:make_global_flag_key

module Make_payload = struct
  type t =
    { basic_type: Act_c_mini.Type.Basic.t
    ; initial_value: Act_c_mini.Constant.t
    ; var: Act_common.Litmus_id.t }
  [@@deriving sexp]

  module G = Base_quickcheck.Generator

  let gen_type_from_constant (k : Act_c_mini.Constant.t) :
      Act_c_mini.Type.Basic.t G.t =
    let ty = Act_c_mini.Constant.type_of k in
    let bt = Act_c_mini.Type.basic_type ty in
    G.of_list [bt; Act_c_mini.Type.Basic.as_atomic bt]

  let generator (vars : Var.Map.t) ~(gen_scope : Act_common.Scope.t G.t) :
      t G.t =
    G.Let_syntax.(
      (* TODO(@MattWindsor91): ideally, this should be
         [quickcheck_generator], ie it should generate Booleans as well.
         However, this would presently result in the fuzzer generating init
         blocks with `true` and `false` in them, which neither our parser nor
         Litmus's parser can comprehend. Until this issue is worked around,
         we only generate integers. *)
      let%bind scope = gen_scope in
      let%bind initial_value = Act_c_mini.Constant.gen_int32 in
      let%bind basic_type = gen_type_from_constant initial_value in
      let%map id = Var.Map.gen_fresh_var vars in
      let var = Act_common.Litmus_id.make ~scope ~id in
      {basic_type; initial_value; var})
end

module Make : Action_types.S with type Payload.t = Make_payload.t = struct
  let name = Act_common.Id.("var" @: "make" @: empty)

  let readme () =
    Act_utils.My_string.format_for_readme
      {|
    Generates a new variable, with a random name, initial value,
    and primitive type.

      If the 'var.make.global' flag is set, the variable will be global;
      else, it'll be a local variable assigned to a random thread.
    |}

  module Payload = struct
    include Make_payload
    module G = Base_quickcheck.Generator

    let gen_scope (nthreads : int) (is_global : bool) :
        Act_common.Scope.t G.t =
      G.(
        if is_global then return Act_common.Scope.Global
        else
          map
            ~f:(fun x -> Act_common.Scope.Local x)
            (int_inclusive 0 (nthreads - 1)))

    let gen (subject : Subject.Test.t) ~(random : Splittable_random.State.t)
        ~(param_map : Param_map.t) : t State.Monad.t =
      State.Monad.(
        Let_syntax.(
          let%bind global_flag = make_global_flag param_map in
          let is_global = Flag.eval global_flag ~random in
          let nthreads = Act_litmus.Test.Raw.num_threads subject in
          let gen_scope = gen_scope nthreads is_global in
          let%map generator = with_vars (generator ~gen_scope) in
          Base_quickcheck.Generator.generate generator ~random ~size:10))
  end

  let available = Action.always

  let add_to_init (subject : Subject.Test.t) (var : Act_common.Litmus_id.t)
      (initial_value : Act_c_mini.Constant.t) : Subject.Test.t Or_error.t =
    var |> Act_common.Litmus_id.as_global
    |> Option.value_map
         ~f:(fun name ->
           Subject.Test.add_var_to_init subject name initial_value)
         ~default:(Ok subject)

  let run (subject : Subject.Test.t)
      ~payload:({basic_type; initial_value; var} : Payload.t) :
      Subject.Test.t State.Monad.t =
    let ty = Act_c_mini.Type.of_basic basic_type ~pointer:true in
    let open State.Monad.Let_syntax in
    let%bind () = State.Monad.register_var ty var ~initial_value in
    State.Monad.Monadic.return (add_to_init subject var initial_value)
end
