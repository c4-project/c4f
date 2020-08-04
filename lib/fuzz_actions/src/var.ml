(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Ac = Act_common
  module Fir = Act_fir
  module F = Act_fuzz
end

module Make_payload = struct
  type t =
    { basic_type: Fir.Type.Basic.t
    ; initial_value: Fir.Constant.t
    ; var: Ac.Litmus_id.t }
  [@@deriving compare, sexp, quickcheck]

  module G = Base_quickcheck.Generator

  let gen_type_from_constant (k : Fir.Constant.t) : Fir.Type.Basic.t G.t =
    let ty = Fir.Constant.type_of k in
    let bt = Fir.Type.basic_type ty in
    G.of_list [bt; Fir.Type.Basic.as_atomic bt]

  let gen_initial_value (scope : Ac.Scope.t) : Fir.Constant.t G.t =
    (* TODO(@MattWindsor91): ideally, this should always be
       [quickcheck_generator], ie it should generate Booleans as well.
       However, this would presently result in the fuzzer generating init
       blocks with `true` and `false` in them, which neither our parser nor
       Litmus's parser can comprehend. Until this issue is worked around, we
       only generate integers at global scope. *)
    if Ac.Scope.is_global scope then Fir.Constant.gen_int32
    else Fir.Constant.quickcheck_generator

  let generator (vars : F.Var.Map.t) ~(gen_scope : Ac.Scope.t G.t) : t G.t =
    G.Let_syntax.(
      let%bind scope = gen_scope in
      let%bind initial_value = gen_initial_value scope in
      let%bind basic_type = gen_type_from_constant initial_value in
      let%map id = F.Var.Map.gen_fresh_var vars in
      let var = Ac.Litmus_id.make ~scope ~id in
      {basic_type; initial_value; var})
end

module Make : F.Action_types.S with type Payload.t = Make_payload.t = struct
  let name = Ac.Id.("var" @: "make" @: empty)

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

    let gen_scope (nthreads : int) (is_global : bool) : Ac.Scope.t G.t =
      G.(
        if is_global then return Ac.Scope.Global
        else
          map ~f:(fun x -> Ac.Scope.Local x) (int_inclusive 0 (nthreads - 1)))

    let gen (subject : F.Subject.Test.t)
        ~(random : Splittable_random.State.t) ~(param_map : F.Param_map.t) :
        t F.State.Monad.t =
      F.State.Monad.(
        Let_syntax.(
          let%bind global_flag =
            Monadic.return (F.Config_tables.make_global_flag param_map)
          in
          let is_global = F.Flag.eval global_flag ~random in
          let nthreads = Act_litmus.Test.Raw.num_threads subject in
          let gen_scope = gen_scope nthreads is_global in
          let%bind generator = with_vars (generator ~gen_scope) in
          F.Payload.Helpers.lift_quickcheck ~random generator))
  end

  let available : F.Availability.t = F.Availability.has_threads

  let run (subject : F.Subject.Test.t)
      ~payload:({basic_type; initial_value; var} : Payload.t) :
      F.Subject.Test.t F.State.Monad.t =
    let is_global = Ac.Litmus_id.is_global var in
    let ty = Fir.Type.make basic_type ~is_pointer:is_global in
    let init = Fir.Initialiser.make ~ty ~value:initial_value () in
    F.State.Monad.(
      Let_syntax.(
        let%bind () = register_var var init in
        Monadic.return (F.Subject.Test.declare_var subject var init)))
end

module Volatile : F.Action_types.S with type Payload.t = Ac.Litmus_id.t =
struct
  let name = Ac.Id.("var" @: "volatile" @: empty)

  let readme () =
    Act_utils.My_string.format_for_readme
      {|
    Adds the 'volatile' qualifier to a local variable.
    |}

  let is_viable (r : F.Var.Record.t) : bool =
    (* TODO(@MattWindsor91): the not-is-global check arises because we want
       to produce Litmus-compatible tests, and we can't easily translate
       volatile global variables into Litmus. If we can work out a way of
       doing this, eg by having shadowed global variables, we should relax
       this. *)
    F.Var.Record.(not (is_global r))

  module Payload = struct
    type t = Ac.Litmus_id.t [@@deriving sexp]

    module Q = Base_quickcheck

    let gen_on_vars (vars : F.Var.Map.t) : Ac.Litmus_id.t Q.Generator.t =
      vars
      |> Ac.Scoped_map.filter ~f:is_viable
      |> Ac.Scoped_map.to_litmus_id_map |> Map.keys |> Q.Generator.of_list

    let gen (_ : F.Subject.Test.t) ~(random : Splittable_random.State.t)
        ~(param_map : F.Param_map.t) : t F.State.Monad.t =
      ignore param_map ;
      F.State.Monad.(
        Let_syntax.(
          let%bind generator = with_vars gen_on_vars in
          F.Payload.Helpers.lift_quickcheck ~random generator))
  end

  let available (ctx : F.Availability.Context.t) : bool Or_error.t =
    (* TODO(@MattWindsor91): this is quite circuitous. *)
    Ok
      ( ctx |> F.Availability.Context.state |> F.State.vars
      |> Ac.Scoped_map.to_litmus_id_map |> Map.exists ~f:is_viable )

  let update_state (id : Ac.Litmus_id.t) : unit F.State.Monad.t =
    F.State.(
      Monad.modify
        (map_vars
           ~f:
             (Ac.Scoped_map.map_record ~id
                ~f:(F.Var.Record.map_type ~f:Fir.Type.as_volatile))))

  let update_thread_decl (d : Fir.Initialiser.t Ac.C_named.t)
      ~(target : Ac.C_id.t) : Fir.Initialiser.t Ac.C_named.t =
    if Ac.C_id.equal target (Ac.C_named.name d) then
      Ac.C_named.map_right d ~f:(fun init ->
          Fir.Initialiser.(
            make ~ty:(Fir.Type.as_volatile (ty init)) ?value:(value init) ()))
    else d

  let update_decls (id : Ac.Litmus_id.t) (subject : F.Subject.Test.t) :
      F.Subject.Test.t Or_error.t =
    match Ac.Litmus_id.as_local id with
    | Some (index, target) ->
        Act_litmus.Test.Raw.try_map_thread subject ~index ~f:(fun x ->
            Ok (F.Subject.Thread.map_decls x ~f:(update_thread_decl ~target)))
    | None ->
        Or_error.error_s
          [%message
            "Internal: this action can't yet make globals volatile"
              ~tried_to_change:(id : Ac.Litmus_id.t)]

  let run (subject : F.Subject.Test.t) ~(payload : Ac.Litmus_id.t) :
      F.Subject.Test.t F.State.Monad.t =
    F.State.Monad.(
      Let_syntax.(
        let%bind () = update_state payload in
        Monadic.return (update_decls payload subject)))
end
