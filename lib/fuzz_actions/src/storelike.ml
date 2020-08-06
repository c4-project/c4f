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
  module Fir = Act_fir
  module F = Act_fuzz
  module Tx = Travesty_base_exts
end

let readme_faw : string Lazy.t =
  lazy
    (Printf.sprintf
       {|
    If '%s' is true, this action will only store
    to variables that haven't previously been selected for store actions.
    This makes calculating candidate executions easier, but limits the degree
    of entropy somewhat.  (Note that if the value is stochastic, the action
    will only fire if such variables exist, but may or may not proceed to
    select a previously-stored variable.  This is a limitation of the flag
    system.)
  |}
       (Ac.Id.to_string F.Config_tables.forbid_already_written_flag))

(** Lists the restrictions we put on source variables. *)
let basic_src_restrictions : (F.Var.Record.t -> bool) list Lazy.t = lazy []

module Dst_restriction = struct
  type t = F.Var.Record.t -> bool

  let basic (dst_type : Fir.Type.Basic.t) : t list =
    F.Var.Record.[is_atomic; has_basic_type ~basic:dst_type]

  let with_user_flags ~(dst_type : Fir.Type.Basic.t)
      ~(forbid_already_written : bool) : t list =
    basic dst_type
    @ List.filter_opt
        [ Option.some_if forbid_already_written
            (Fn.non F.Var.Record.has_writes) ]

  let forbid_dependencies : t =
    Tx.Fn.(
      Fn.non F.Var.Record.has_dependencies
      (* We don't know whether variables that existed before fuzzing have any
         dependencies, as we don't do any flow analysis of them. Maybe one
         day this will be relaxed? *)
      &&& F.Var.Record.was_generated)
end

module Make (B : sig
  val name : Ac.Id.t
  (** [name] is the name of the action. *)

  val readme_preamble : string list
  (** [readme_preamble] is the part of the action readme specific to this
      form of the storelike action. *)

  val dst_type : Fir.Type.Basic.t
  (** [dst_type] is the value type of the destination. *)

  val path_filter : F.Path_filter.t
  (** [path_filter] is the filter to apply on statement insertion paths
      before considering them for the atomic store. *)

  val extra_dst_restrictions : Dst_restriction.t list
  (** [extra_dst_restrictions] is a list of additional restrictions to place
      on the destination variables (for example, 'must not have
      dependencies'). *)

  module Flags : Storelike_types.Flags

  include Storelike_types.Basic
end) :
  F.Action_types.S with type Payload.t = B.t F.Payload_impl.Insertion.t =
struct
  let name = B.name

  (** [readme_chunks ()] generates fragments of unformatted README text based
      on the configuration of this store module. *)
  let readme_chunks () : string list =
    B.readme_preamble
    @ [ Printf.sprintf "This operation generates '%s's."
          (Fir.Type.Basic.to_string B.dst_type)
      ; Lazy.force readme_faw ]

  let readme () =
    readme_chunks () |> String.concat ~sep:"\n\n"
    |> Act_utils.My_string.format_for_readme

  let src_env (vars : F.Var.Map.t) ~(tid : int) : Fir.Env.t =
    let predicates = Lazy.force basic_src_restrictions in
    F.Var.Map.env_satisfying_all ~predicates ~scope:(Local tid) vars

  let dst_restrictions ~(forbid_already_written : bool) :
      (F.Var.Record.t -> bool) list =
    Dst_restriction.with_user_flags ~dst_type:B.dst_type
      ~forbid_already_written
    @ B.extra_dst_restrictions

  let dst_env (vars : F.Var.Map.t) ~(tid : int)
      ~(forbid_already_written : bool) : Fir.Env.t =
    let predicates = dst_restrictions ~forbid_already_written in
    F.Var.Map.env_satisfying_all ~predicates ~scope:(Local tid) vars

  module Payload = F.Payload_impl.Insertion.Make (struct
    type t = B.t [@@deriving sexp]

    let path_filter _ = B.path_filter

    module G = Base_quickcheck.Generator

    let error_if_empty (env_name : string) (env : Fir.Env.t) :
        unit Or_error.t =
      if Map.is_empty env then
        Or_error.error_s
          [%message
            "Internal error: Environment was empty." ~here:[%here] ~env_name]
      else Ok ()

    let check_envs (src : Fir.Env.t) (dst : Fir.Env.t) : unit Or_error.t =
      Or_error.Let_syntax.(
        let%bind () = error_if_empty "src" src in
        error_if_empty "dst" dst)

    let gen' (vars : F.Var.Map.t) ~(where : F.Path.t)
        ~(forbid_already_written : bool) : t F.Opt_gen.t =
      let tid = F.Path.tid where in
      let src = src_env vars ~tid in
      let dst = dst_env vars ~tid ~forbid_already_written in
      Or_error.Let_syntax.(
        let%map () = check_envs src dst in
        B.gen ~src ~dst ~vars ~tid)

    let gen (where : F.Path.t) : t F.Payload_gen.t =
      F.Payload_gen.(
        let* forbid_already_written =
          flag F.Config_tables.forbid_already_written_flag
        in
        let* vars = vars in
        lift_opt_gen (gen' vars ~where ~forbid_already_written))
  end)

  let available (ctx : F.Availability.Context.t) : bool Or_error.t =
    let vars = F.State.vars (F.Availability.Context.state ctx) in
    let param_map = F.Availability.Context.param_map ctx in
    Or_error.Let_syntax.(
      let%bind faw_flag =
        F.Param_map.get_flag param_map
          ~id:F.Config_tables.forbid_already_written_flag
      in
      (* If the flag is stochastic, then we can't tell whether its value will
         be the same in the payload check. As such, we need to be pessimistic
         and assume that we _can't_ make writes to already-written variables
         if we can't guarantee an exact value.

         See https://github.com/MattWindsor91/act/issues/172. *)
      let forbid_already_written =
        Option.value (F.Flag.to_exact_opt faw_flag) ~default:true
      in
      let has_vars =
        F.Var.Map.exists_satisfying_all vars ~scope:Ac.Scope.Global
          ~predicates:(dst_restrictions ~forbid_already_written)
      in
      let%map is_constructible =
        F.Availability.is_filter_constructible B.path_filter ctx ~kind:Insert
      in
      has_vars && is_constructible)

  let bookkeep_dst (x : Ac.C_id.t) ~(tid : int) : unit F.State.Monad.t =
    F.State.Monad.(
      Let_syntax.(
        let%bind dst_var = resolve x ~scope:(Local tid) in
        let%bind () = add_write dst_var in
        when_m B.Flags.erase_known_values ~f:(fun () ->
            erase_var_value dst_var)))

  let bookkeep_dsts (xs : Ac.C_id.t list) ~(tid : int) : unit F.State.Monad.t
      =
    xs |> List.map ~f:(bookkeep_dst ~tid) |> F.State.Monad.all_unit

  let bookkeep_srcs (srcs : Fir.Expression.t list) ~(tid : int) :
      unit F.State.Monad.t =
    F.State.Monad.(
      when_m B.Flags.respect_src_dependencies ~f:(fun () ->
          add_multiple_expression_dependencies srcs ~scope:(Local tid)))

  module MList = Tx.List.On_monad (F.State.Monad)

  let bookkeep_new_locals (nls : Fir.Initialiser.t Ac.C_named.Alist.t)
      ~(tid : int) : unit F.State.Monad.t =
    MList.iter_m nls ~f:(fun (name, init) ->
        F.State.Monad.register_var (Ac.Litmus_id.local tid name) init)

  let do_bookkeeping (item : B.t) ~(tid : int) : unit F.State.Monad.t =
    F.State.Monad.Let_syntax.(
      let%bind () = bookkeep_new_locals ~tid (B.new_locals item) in
      let%bind () = bookkeep_dsts ~tid (B.dst_ids item) in
      bookkeep_srcs ~tid (B.src_exprs item))

  let insert_vars (target : F.Subject.Test.t)
      (new_locals : Fir.Initialiser.t Ac.C_named.Alist.t) ~(tid : int) :
      F.Subject.Test.t Or_error.t =
    Tx.List.With_errors.fold_m new_locals ~init:target
      ~f:(fun subject (id, init) ->
        F.Subject.Test.declare_var subject (Ac.Litmus_id.local tid id) init)

  let do_insertions (target : F.Subject.Test.t) ~(path : F.Path.t)
      ~(tid : int) ~(to_insert : B.t) : F.Subject.Test.t Or_error.t =
    let stms =
      Fir.(
        to_insert |> B.to_stms
        |> List.map ~f:(Statement.prim F.Metadata.generated))
    in
    Or_error.Let_syntax.(
      let%bind target' =
        F.Path_consumers.consume target ~filter:B.path_filter ~path
          ~action:(Insert stms)
      in
      insert_vars target' (B.new_locals to_insert) ~tid)

  let run (subject : F.Subject.Test.t)
      ~(payload : B.t F.Payload_impl.Insertion.t) :
      F.Subject.Test.t F.State.Monad.t =
    let to_insert = F.Payload_impl.Insertion.to_insert payload in
    let path = F.Payload_impl.Insertion.where payload in
    let tid = F.Path.tid path in
    F.State.Monad.(
      Let_syntax.(
        let%bind () = do_bookkeeping to_insert ~tid in
        Monadic.return (do_insertions subject ~path ~tid ~to_insert)))
end
