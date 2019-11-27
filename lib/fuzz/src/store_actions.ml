(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ac = Act_common

module Random_state = struct
  (* We don't give [gen] here, because it depends quite a lot on the functor
     arguments of [Make]. *)

  type t = {store: Act_c_mini.Atomic_store.t; path: Path.Program.t}
  [@@deriving fields, make, sexp]
end

module Make (B : sig
  val name : Ac.Id.t

  val forbid_already_written : bool

  val dst_type : Act_c_mini.Type.Basic.t

  module Quickcheck
      (Src : Act_c_mini.Env_types.S)
      (Dst : Act_c_mini.Env_types.S) :
    Act_utils.My_quickcheck.S_with_sexp
      with type t := Act_c_mini.Atomic_store.t
end) : Action_types.S with type Payload.t = Random_state.t = struct
  let name = B.name

  include Action.Make_log (B)

  (** [readme_chunks ()] generates fragments of unformatted README text based
      on the configuration of this store module. *)
  let readme_chunks () : (bool * string) list =
    [ ( true
      , {|
       Generates a store operation on a randomly selected fuzzer-generated
       global variable.
       |}
      )
    ; ( true
      , Printf.sprintf "This operation generates '%s's."
          (Act_c_mini.Type.Basic.to_string B.dst_type) )
    ; ( B.forbid_already_written
      , {|
       This version of the action only stores to variables that haven't
       previously been selected for store actions.  This makes calculating
       candidate executions easier, but limits the degree of entropy
       somewhat.
       |}
      ) ]

  let select_and_format_chunk (select : bool) (chunk : string) :
      string option =
    if select then Some (Act_utils.My_string.format_for_readme chunk)
    else None

  let readme () =
    readme_chunks ()
    |> List.filter_map ~f:(fun (select, chunk) ->
           select_and_format_chunk select chunk)
    |> String.concat ~sep:"\n\n"

  (** Lists the restrictions we put on source variables. *)
  let src_restrictions : (Var.Record.t -> bool) list Lazy.t = lazy []

  (** Lists the restrictions we put on destination variables. *)
  let dst_restrictions : (Var.Record.t -> bool) list Lazy.t =
    lazy
      Var.Record.(
        [ is_atomic
        ; was_generated
        ; has_basic_type ~basic:B.dst_type
          (* This is to make sure that we don't change the observable
             semantics of the program over its original variables. *)
        ; Fn.non has_dependencies
          (* This action changes the value, so we can't do it to variables
             with depended-upon values. *) ]
        @ if B.forbid_already_written then [Fn.non has_writes] else [])

  module Payload = struct
    type t = Random_state.t [@@deriving sexp]

    module G = Base_quickcheck.Generator

    let src_env (vars : Var.Map.t) ~(tid : int) :
        (module Act_c_mini.Env_types.S) =
      let predicates = Lazy.force src_restrictions in
      Var.Map.env_module_satisfying_all ~predicates ~scope:(Local tid) vars

    let dst_env (vars : Var.Map.t) ~(tid : int) :
        (module Act_c_mini.Env_types.S) =
      let predicates = Lazy.force dst_restrictions in
      Var.Map.env_module_satisfying_all ~predicates ~scope:(Local tid) vars

    let error_if_empty (env : string) (module M : Act_c_mini.Env_types.S) :
        unit Or_error.t =
      if Map.is_empty M.env then
        Or_error.error_s
          [%message
            "Internal error: Environment was empty." ~here:[%here] ~env]
      else Ok ()

    let log_environment (o : Ac.Output.t) (env_name : string)
        (env : Act_c_mini.Type.t Map.M(Ac.C_id).t) : unit =
      Ac.Output.pv o "%s environment: %a@." env_name Sexp.pp_hum
        [%sexp (env : Act_c_mini.Type.t Map.M(Ac.C_id).t)]

    let log_environments (o : Ac.Output.t)
        (src_env : Act_c_mini.Type.t Map.M(Ac.C_id).t)
        (dst_env : Act_c_mini.Type.t Map.M(Ac.C_id).t) : unit =
      log_environment o "source" src_env ;
      log_environment o "dest" dst_env

    let gen_store_with_envs (module Src : Act_c_mini.Env_types.S)
        (module Dst : Act_c_mini.Env_types.S) (o : Ac.Output.t)
        ~(random : Splittable_random.State.t) :
        Act_c_mini.Atomic_store.t Or_error.t =
      Or_error.Let_syntax.(
        let%bind () = error_if_empty "src" (module Src) in
        let%map () = error_if_empty "dst" (module Dst) in
        log o "Environments are non-empty" ;
        log_environments o Src.env Dst.env ;
        let module Gen = B.Quickcheck (Src) (Dst) in
        log o "Built generator module" ;
        Base_quickcheck.Generator.generate ~random ~size:10
          [%quickcheck.generator: Gen.t])

    let gen_store (o : Ac.Output.t) (vars : Var.Map.t) ~(tid : int)
        ~(random : Splittable_random.State.t) :
        Act_c_mini.Atomic_store.t Or_error.t =
      log o "Generating store" ;
      let src_mod = src_env vars ~tid in
      let dst_mod = dst_env vars ~tid in
      log o "Found environments for store" ;
      gen_store_with_envs src_mod dst_mod o ~random

    let gen_path (o : Ac.Output.t) (subject : Subject.Test.t)
        ~(random : Splittable_random.State.t) : Path.Program.t State.Monad.t
        =
      log o "Generating path" ;
      Payload.Helpers.lift_quickcheck_opt ~random ~action_id:name
        (Path_producers.Test.try_gen_insert_stm subject)

    let gen' (o : Ac.Output.t) (subject : Subject.Test.t)
        ~(random : Splittable_random.State.t) (vars : Var.Map.t) :
        t State.Monad.t =
      State.Monad.Let_syntax.(
        let%bind path = gen_path o subject ~random in
        let tid = Path.Program.tid path in
        let%map store =
          State.Monad.Monadic.return (gen_store o vars ~tid ~random)
        in
        Random_state.make ~store ~path)

    let gen (subject : Subject.Test.t) ~(random : Splittable_random.State.t)
        : t State.Monad.t =
      State.Monad.Let_syntax.(
        let%bind o = State.Monad.output () in
        State.Monad.with_vars_m (gen' o subject ~random))
  end

  let available (_ : Subject.Test.t) =
    State.Monad.with_vars
      (Var.Map.exists_satisfying_all ~scope:Ac.Scope.Global
         ~predicates:(Lazy.force dst_restrictions))

  (* This action writes to the destination, so we no longer have a known
     value for it. *)
  let mark_store_dst (store : Act_c_mini.Atomic_store.t) : unit State.Monad.t
      =
    let open State.Monad.Let_syntax in
    let dst = Act_c_mini.Atomic_store.dst store in
    let dst_var =
      Act_common.Litmus_id.global (Act_c_mini.Address.variable_of dst)
    in
    let%bind () = State.Monad.erase_var_value dst_var in
    State.Monad.add_write dst_var

  (* This action also introduces dependencies on every variable in the
     source. *)
  let add_dependencies_to_store_src (store : Act_c_mini.Atomic_store.t)
      ~(tid : int) : unit State.Monad.t =
    State.Monad.add_expression_dependencies
      (Act_c_mini.Atomic_store.src store)
      ~scope:(Local tid)

  let do_bookkeeping (store : Act_c_mini.Atomic_store.t) ~(tid : int) :
      unit State.Monad.t =
    State.Monad.Let_syntax.(
      let%bind o = State.Monad.output () in
      log o "Erasing known value of store destination" ;
      let%bind () = mark_store_dst store in
      log o "Adding dependency to store source" ;
      add_dependencies_to_store_src store ~tid)

  let run (subject : Subject.Test.t)
      ~payload:({store; path} : Random_state.t) :
      Subject.Test.t State.Monad.t =
    let store_stm = Act_c_mini.Statement.atomic_store store in
    let tid = Path.Program.tid path in
    State.Monad.Let_syntax.(
      let%bind () = do_bookkeeping store ~tid in
      State.Monad.Monadic.return
        (Path_consumers.Test.insert_stm path ~to_insert:store_stm
           ~target:subject))
end

module Int : Action_types.S with type Payload.t = Random_state.t =
Make (struct
  let name = Ac.Id.of_string "store.make.int.single"

  let dst_type = Act_c_mini.Type.Basic.int ~atomic:true ()

  let forbid_already_written = true (* for now *)

  module Quickcheck = Act_c_mini.Atomic_store.Quickcheck_ints
end)
