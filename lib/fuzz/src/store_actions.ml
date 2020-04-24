(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Ac = Act_common
end

let readme_preamble : string =
  {|
    Generates a store operation on a randomly selected fuzzer-generated
    global variable.

    If 'store.forbid-already-written' is true, this action will only store
    to variables that haven't previously been selected for store actions.
    This makes calculating candidate executions easier, but limits the degree
    of entropy somewhat.  (Note that if the value is stochastic, the action
    will only fire if such variables exist, but may or may not proceed to
    select a previously-stored variable.  This is a limitation of the flag
    system.)
  |}

let forbid_already_written_flag_key : Ac.Id.t =
  Ac.Id.("store" @: "forbid-already-written" @: empty)

let forbid_already_written_flag (param_map : Param_map.t) :
    Flag.t State.Monad.t =
  Param_map.get_flag_m param_map ~id:forbid_already_written_flag_key

module Store_payload = struct
  (* We don't give [gen] here, because it depends quite a lot on the functor
     arguments of [Make]. *)

  type t = {store: Act_c_mini.Atomic_store.t; path: Path.Program.t}
  [@@deriving fields, make, sexp]
end

let basic_dst_restrictions (dst_type : Act_c_mini.Type.Basic.t) :
    (Var.Record.t -> bool) list =
  Var.Record.[is_atomic; was_generated; has_basic_type ~basic:dst_type]

(** Lists the restrictions we put on destination variables. *)
let dst_restrictions ~(dst_type : Act_c_mini.Type.Basic.t)
    ~(forbid_already_written : bool) : (Var.Record.t -> bool) list =
  basic_dst_restrictions dst_type
  @ List.filter_opt
      [Option.some_if forbid_already_written (Fn.non Var.Record.has_writes)]

let dst_litmus_id (store : Act_c_mini.Atomic_store.t) :
    Act_common.Litmus_id.t =
  store |> Act_c_mini.Atomic_store.dst |> Act_c_mini.Address.variable_of
  |> Act_common.Litmus_id.global

let add_dependencies_to_store_src (store : Act_c_mini.Atomic_store.t)
    ~(tid : int) : unit State.Monad.t =
  State.Monad.add_expression_dependencies
    (Act_c_mini.Atomic_store.src store)
    ~scope:(Local tid)

(** Functor for generating variants of the store action. *)
module Make (B : sig
  val name_suffix : Ac.Id.t
  (** [name_suffix] is the name of the action, less 'store.make'. *)

  val readme_insert : string
  (** [readme_insert] is the part of the action readme specific to this form
      of the store action. *)

  val dst_type : Act_c_mini.Type.Basic.t
  (** [dst_type] is the value type of the destination. *)

  val path_filter : Path_filter.t
  (** [path_filter] is the filter to apply on statement insertion paths
      before considering them for the atomic store. *)

  val extra_dst_restrictions : (Var.Record.t -> bool) list
  (** [extra_dst_restrictions] is a list of additional restrictions to place
      on the destination variables (for example, 'must not have
      dependencies'). *)

  val respect_src_dependencies : bool
  (** [respect_src_dependencies] is a flag that, when true, causes the action
      to mark dependencies on source variables when finished. *)

  val erase_known_values : bool
  (** [erase_known_values] is a flag that, when true, causes the action to
      erase the known value when finished. *)

  (** A functor that produces a quickcheck instance for atomic stores given
      source and destination variable environments. *)
  module Quickcheck
      (Src : Act_c_mini.Env_types.S)
      (Dst : Act_c_mini.Env_types.S) :
    Act_utils.My_quickcheck.S_with_sexp
      with type t := Act_c_mini.Atomic_store.t
end) : Action_types.S with type Payload.t = Store_payload.t = struct
  module Name = struct
    let name = Act_common.Id.("store" @: "make" @: B.name_suffix)
  end

  include Name
  include Action.Make_log (Name)

  (** [readme_chunks ()] generates fragments of unformatted README text based
      on the configuration of this store module. *)
  let readme_chunks () : string list =
    [ readme_preamble
    ; B.readme_insert
    ; Printf.sprintf "This operation generates '%s's."
        (Act_c_mini.Type.Basic.to_string B.dst_type) ]

  let readme () =
    readme_chunks () |> String.concat ~sep:"\n\n"
    |> Act_utils.My_string.format_for_readme

  (** Lists the restrictions we put on source variables. *)
  let src_restrictions : (Var.Record.t -> bool) list Lazy.t = lazy []

  let src_env (vars : Var.Map.t) ~(tid : int) : Act_c_mini.Env.t =
    let predicates = Lazy.force src_restrictions in
    Var.Map.env_satisfying_all ~predicates ~scope:(Local tid) vars

  let dst_restrictions ~(forbid_already_written : bool) :
      (Var.Record.t -> bool) list =
    dst_restrictions ~dst_type:B.dst_type ~forbid_already_written
    @ B.extra_dst_restrictions

  let dst_env (vars : Var.Map.t) ~(tid : int)
      ~(forbid_already_written : bool) :
      Act_c_mini.Env.t =
    let predicates = dst_restrictions ~forbid_already_written in
    Var.Map.env_satisfying_all ~predicates ~scope:(Local tid) vars

  module Payload = struct
    type t = Store_payload.t [@@deriving sexp]

    module G = Base_quickcheck.Generator

    let error_if_empty (env_name : string) (env : Act_c_mini.Env.t) :
        unit Or_error.t =
      if Map.is_empty env then
        Or_error.error_s
          [%message
            "Internal error: Environment was empty." ~here:[%here] ~env_name]
      else Ok ()

    let log_environment (o : Ac.Output.t) (env_name : string)
        (env : Act_c_mini.Env.t) : unit =
      log o "%s environment: %a@." env_name Sexp.pp_hum
        [%sexp (env : Act_c_mini.Env.t)]

    let log_environments (o : Ac.Output.t)
        (src_env : Act_c_mini.Env.t)
        (dst_env : Act_c_mini.Env.t) : unit =
      log_environment o "source" src_env ;
      log_environment o "dest" dst_env

    let gen_store_with_envs (src : Act_c_mini.Env.t)
        (dst : Act_c_mini.Env.t)
        (o : Ac.Output.t) ~(random : Splittable_random.State.t) :
        Act_c_mini.Atomic_store.t Or_error.t =
      Or_error.Let_syntax.(
        let%bind () = error_if_empty "src" src in
        let%map () = error_if_empty "dst" dst in
        log o "Environments are non-empty" ;
        log_environments o src dst ;
        let module Src = struct let env = src end in
        let module Dst = struct let env = dst end in
        let module Gen = B.Quickcheck (Src) (Dst) in
        log o "Built generator module" ;
        Base_quickcheck.Generator.generate ~random ~size:10
          [%quickcheck.generator: Gen.t])

    let gen_store (o : Ac.Output.t) (vars : Var.Map.t) ~(tid : int)
        ~(random : Splittable_random.State.t)
        ~(forbid_already_written : bool) :
        Act_c_mini.Atomic_store.t Or_error.t =
      log o "Generating store" ;
      let src_mod = src_env vars ~tid in
      let dst_mod = dst_env vars ~tid ~forbid_already_written in
      log o "Found environments for store" ;
      gen_store_with_envs src_mod dst_mod o ~random

    let gen_path (o : Ac.Output.t) (subject : Subject.Test.t)
        ~(random : Splittable_random.State.t) : Path.Program.t State.Monad.t
        =
      log o "Generating path" ;
      Payload.Helpers.lift_quickcheck_opt ~random ~action_id:name
        (Path_producers.Test.try_gen_insert_stm subject
           ~filter:B.path_filter)

    let gen' (o : Ac.Output.t) (subject : Subject.Test.t) (vars : Var.Map.t)
        ~(random : Splittable_random.State.t)
        ~(forbid_already_written : bool) : t State.Monad.t =
      State.Monad.Let_syntax.(
        let%bind path = gen_path o subject ~random in
        let tid = Path.Program.tid path in
        let%map store =
          State.Monad.Monadic.return
            (gen_store o vars ~tid ~random ~forbid_already_written)
        in
        Store_payload.make ~store ~path)

    let gen (subject : Subject.Test.t) ~(random : Splittable_random.State.t)
        ~(param_map : Param_map.t) : t State.Monad.t =
      State.Monad.Let_syntax.(
        let%bind faw_flag = forbid_already_written_flag param_map in
        let forbid_already_written = Flag.eval faw_flag ~random in
        let%bind o = State.Monad.output () in
        log o "forbid already written: %b" forbid_already_written ;
        State.Monad.with_vars_m
          (gen' o subject ~random ~forbid_already_written))
  end

  let available (subject : Subject.Test.t) ~(param_map : Param_map.t) :
      bool State.Monad.t =
    State.Monad.(
      Let_syntax.(
        let%bind faw_flag = forbid_already_written_flag param_map in
        (* If the flag is stochastic, then we can't tell whether its value
           will be the same in the payload check. As such, we need to be
           pessimistic and assume that we _can't_ make writes to
           already-written variables if we can't guarantee an exact value.

           See https://github.com/MattWindsor91/act/issues/172. *)
        let forbid_already_written =
          Option.value (Flag.to_exact_opt faw_flag) ~default:true
        in
        let%map has_vars =
          with_vars
            (Var.Map.exists_satisfying_all ~scope:Ac.Scope.Global
               ~predicates:(dst_restrictions ~forbid_already_written))
        in
        has_vars && Path_filter.is_constructible B.path_filter ~subject))

  let do_bookkeeping (store : Act_c_mini.Atomic_store.t) ~(tid : int) :
      unit State.Monad.t =
    let dst_var = dst_litmus_id store in
    State.Monad.Let_syntax.(
      let%bind () = State.Monad.add_write dst_var in
      let%bind () =
        State.Monad.when_m B.erase_known_values ~f:(fun () ->
            State.Monad.erase_var_value dst_var)
      in
      State.Monad.when_m B.respect_src_dependencies ~f:(fun () ->
          add_dependencies_to_store_src store ~tid))

  let run (subject : Subject.Test.t)
      ~payload:({store; path} : Store_payload.t) :
      Subject.Test.t State.Monad.t =
    let store_stm =
      Act_c_mini.(
        store |> Prim_statement.atomic_store
        |> Statement.prim Metadata.generated)
    in
    let tid = Path.Program.tid path in
    State.Monad.Let_syntax.(
      let%bind o = State.Monad.output () in
      log o "Doing bookkeeping..." ;
      let%bind () = do_bookkeeping store ~tid in
      State.Monad.Monadic.return
        (Path_consumers.Test.insert_stm path ~to_insert:store_stm
           ~target:subject))
end

module Int : Action_types.S with type Payload.t = Store_payload.t =
Make (struct
  let name_suffix = Ac.Id.of_string "int.normal"

  let readme_insert : string =
    "This variant can insert anywhere and target any source and destination."

  let path_filter = Path_filter.empty

  let extra_dst_restrictions = [Fn.non Var.Record.has_dependencies]

  let erase_known_values = true

  let respect_src_dependencies = true

  let dst_type = Act_c_mini.Type.Basic.int ~atomic:true ()

  module Quickcheck = Act_c_mini.Atomic_store.Quickcheck_ints
end)

module Int_dead : Action_types.S with type Payload.t = Store_payload.t =
Make (struct
  let name_suffix = Ac.Id.of_string "int.dead"

  let readme_insert : string =
    {| This variant can target any source and destination, but only inserts
       into dead code.  As it only targets dead code, it does not add
       dependences or erase known-values. |}

  let path_filter = Path_filter.(empty |> in_dead_code_only)

  let extra_dst_restrictions = []

  let erase_known_values = false

  let respect_src_dependencies = false

  let dst_type = Act_c_mini.Type.Basic.int ~atomic:true ()

  module Quickcheck = Act_c_mini.Atomic_store.Quickcheck_ints
end)

module Int_redundant : Action_types.S with type Payload.t = Store_payload.t =
Make (struct
  let name_suffix = Ac.Id.of_string "int.redundant"

  let readme_insert : string =
    {| This variant can insert anywhere, but only stores the known value of
       a destination back to itself. |}

  let path_filter = Path_filter.empty

  let extra_dst_restrictions = [Var.Record.has_known_value]

  let erase_known_values = false

  let respect_src_dependencies = true

  let dst_type = Act_c_mini.Type.Basic.int ~atomic:true ()

  (* The quickcheck scheme for redundant stores needs to be very different
     from the usual scheme, as it must make sure the source is the
     destination's known value. *)
  module Quickcheck
      (Src : Act_c_mini.Env_types.S)
      (Dst : Act_c_mini.Env_types.S) :
    Act_utils.My_quickcheck.S_with_sexp
      with type t = Act_c_mini.Atomic_store.t = struct
    type t = Act_c_mini.Atomic_store.t [@@deriving sexp]

    module Q_dst = Act_c_mini.Address_gen.Atomic_int_pointers (Dst)

    let quickcheck_observer = Act_c_mini.Atomic_store.quickcheck_observer

    (* TODO(@MattWindsor91): allow shrinking the MO? *)
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic

    let known_value_expr_of_dest (dst : Q_dst.t) :
        Act_c_mini.Expression.t Or_error.t =
      Or_error.Let_syntax.(
        let id = Act_c_mini.Address.variable_of dst in
        let%bind kvo = Act_c_mini.Env.known_value Dst.env ~id in
        let%map kv =
          Result.of_option kvo
            ~error:(Error.of_string "No known value for this record.")
        in
        Act_c_mini.Expression.constant kv)

    let quickcheck_generator =
      (* Deliberately ignore the source environment. TODO(@MattWindsor91):
         optimise this? *)
      Base_quickcheck.Generator.map2 [%quickcheck.generator: Q_dst.t]
        Act_c_mini.Mem_order.gen_store ~f:(fun dst mo ->
          (* We're really hoping that the availability check over known
             values works here, because there's no way we can safely return
             an error if not. *)
          let src = Or_error.ok_exn (known_value_expr_of_dest dst) in
          Act_c_mini.Atomic_store.make ~src ~dst ~mo)
  end
end)
