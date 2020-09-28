(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Make_payload = struct
  type t =
    { basic_type: Fir.Type.Basic.t
    ; initial_value: Fir.Constant.t
    ; var: Common.Litmus_id.t }
  [@@deriving compare, sexp, quickcheck]

  module G = Base_quickcheck.Generator

  let gen_type_from_constant (k : Fir.Constant.t) : Fir.Type.Basic.t G.t =
    let ty = Fir.Constant.type_of k in
    let bt = Fir.Type.basic_type ty in
    G.of_list [bt; Fir.Type.Basic.as_atomic bt]

  let gen_initial_value (scope : Common.Scope.t) : Fir.Constant.t G.t =
    (* TODO(@MattWindsor91): ideally, this should always be
       [quickcheck_generator], ie it should generate Booleans as well.
       However, this would presently result in the fuzzer generating init
       blocks with `true` and `false` in them, which neither our parser nor
       Litmus's parser can comprehend. Until this issue is worked around, we
       only generate integers at global scope. *)
    if Common.Scope.is_global scope then Fir.Constant.gen_int32
    else Fir.Constant.quickcheck_generator

  let generator (vars : Fuzz.Var.Map.t) ~(gen_scope : Common.Scope.t G.t) :
      t G.t =
    G.Let_syntax.(
      let%bind scope = gen_scope in
      let%bind initial_value = gen_initial_value scope in
      let%bind basic_type = gen_type_from_constant initial_value in
      let%map id = Fuzz.Var.Map.gen_fresh_var vars in
      let var = Common.Litmus_id.make ~scope ~id in
      {basic_type; initial_value; var})
end

module Make : Fuzz.Action_types.S with type Payload.t = Make_payload.t =
struct
  let name = Common.Id.("var" @: "make" @: empty)

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

    let gen_scope (nthreads : int) (is_global : bool) : Common.Scope.t G.t =
      G.(
        if is_global then return Common.Scope.Global
        else
          map
            ~f:(fun x -> Common.Scope.Local x)
            (int_inclusive 0 (nthreads - 1)))

    let gen : t Fuzz.Payload_gen.t =
      Fuzz.(
        Payload_gen.(
          let* is_global = flag Fuzz.Config_tables.make_global_flag in
          let* nthreads =
            lift
              (Fn.compose Act_litmus.Test.Raw.num_threads
                 (Accessor.get
                    (Context.actx @> Availability.Context.subject)))
          in
          let* vars = lift_acc (Context.state @> State.vars) in
          let gen_scope = gen_scope nthreads is_global in
          lift_quickcheck (generator vars ~gen_scope)))
  end

  let available : Fuzz.Availability.t = Fuzz.Availability.has_threads

  let run (subject : Fuzz.Subject.Test.t)
      ~payload:({basic_type; initial_value; var} : Payload.t) :
      Fuzz.Subject.Test.t Fuzz.State.Monad.t =
    let is_global = Common.Litmus_id.is_global var in
    let ty = Fir.Type.make basic_type ~is_pointer:is_global in
    let init = Fir.Initialiser.make ~ty ~value:initial_value in
    Fuzz.State.Monad.register_and_declare_var var init subject
end

module Volatile :
  Fuzz.Action_types.S with type Payload.t = Common.Litmus_id.t = struct
  let name = Common.Id.("var" @: "volatile" @: empty)

  let readme () =
    Act_utils.My_string.format_for_readme
      {|
    Adds the 'volatile' qualifier to a local variable.
    |}

  let is_viable (r : Fuzz.Var.Record.t) : bool =
    (* TODO(@MattWindsor91): the not-is-global check arises because we want
       to produce Litmus-compatible tests, and we can't easily translate
       volatile global variables into Litmus. If we can work out a way of
       doing this, eg by having shadowed global variables, we should relax
       this. *)
    Fuzz.Var.Record.(not (is_global r))

  module Payload = struct
    type t = Common.Litmus_id.t [@@deriving sexp]

    module Q = Base_quickcheck

    let gen_on_vars (vars : Fuzz.Var.Map.t) :
        Common.Litmus_id.t Q.Generator.t =
      vars
      |> Common.Scoped_map.filter ~f:is_viable
      |> Common.Scoped_map.to_litmus_id_map |> Map.keys
      |> Q.Generator.of_list

    let gen : t Fuzz.Payload_gen.t =
      Fuzz.Payload_gen.(vars >>| gen_on_vars >>= lift_quickcheck)
  end

  let available : Fuzz.Availability.t =
    Fuzz.Availability.has_variables ~predicates:[is_viable]

  let update_state (id : Common.Litmus_id.t) : unit Fuzz.State.Monad.t =
    Fuzz.State.(
      Monad.modify
        (Accessor.map vars
           ~f:
             (Common.Scoped_map.map_record ~id
                ~f:
                  (Accessor.set
                     ( Fuzz.Var.Record.Access.type_of
                     @> Fir.Type.Access.is_volatile )
                     ~to_:true))))

  let update_thread_decl (d : Fir.Initialiser.t Common.C_named.t)
      ~(target : Common.C_id.t) : Fir.Initialiser.t Common.C_named.t =
    Common.(
      if C_id.(target = d.@(C_named.name)) then
        d.@(C_named.value @> Fir.Initialiser.ty
            @> Fir.Type.Access.is_volatile) <- true
      else d)

  let update_decls (id : Common.Litmus_id.t) (subject : Fuzz.Subject.Test.t)
      : Fuzz.Subject.Test.t Or_error.t =
    match Common.Litmus_id.as_local id with
    | Some (index, target) ->
        (* TODO(@MattWindsor91): surely this can be cleaned up *)
        Act_litmus.Test.Raw.try_map_thread subject ~index ~f:(fun x ->
            Ok
              (Fuzz.Subject.Thread.map_decls x
                 ~f:(update_thread_decl ~target)))
    | None ->
        Or_error.error_s
          [%message
            "Internal: this action can't yet make globals volatile"
              ~tried_to_change:(id : Common.Litmus_id.t)]

  let run (subject : Fuzz.Subject.Test.t) ~(payload : Common.Litmus_id.t) :
      Fuzz.Subject.Test.t Fuzz.State.Monad.t =
    Fuzz.State.Monad.(
      Let_syntax.(
        let%bind () = update_state payload in
        Monadic.return (update_decls payload subject)))
end
