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
    F.Var.Record.
      [ is_atomic
      ; Fir.(
          Type.Basic.eq
            Accessor.(Access.ty @> Type.Access.basic_type)
            ~to_:dst_type) ]

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

  let approx_forbid_already_written (ctx : F.Availability.Context.t) :
      bool Or_error.t =
    (* If the flag is stochastic, then we can't tell whether its value will
       be the same in the payload check. As such, we need to be pessimistic
       and assume that we _can't_ make writes to already-written variables if
       we can't guarantee an exact value.

       See https://github.com/MattWindsor91/act/issues/172. *)
    Or_error.(
      F.Param_map.get_flag
        (F.Availability.Context.param_map ctx)
        ~id:F.Config_tables.forbid_already_written_flag
      >>| F.Flag.to_exact_opt
      >>| Option.value ~default:true)

  let path_filter ctx =
    let forbid_already_written =
      ctx |> approx_forbid_already_written |> Result.ok
      |> Option.value ~default:true
    in
    F.Availability.in_thread_with_variables ctx
      ~predicates:(dst_restrictions ~forbid_already_written)
    @@ F.Availability.in_thread_with_variables ctx
         ~predicates:(Lazy.force basic_src_restrictions)
    @@ B.path_filter

  module Payload = F.Payload_impl.Insertion.Make (struct
    type t = B.t [@@deriving sexp]

    let path_filter = path_filter

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

    let gen (wheref : F.Path.Flagged.t) : t F.Payload_gen.t =
      F.Payload_gen.(
        let* forbid_already_written =
          flag F.Config_tables.forbid_already_written_flag
        in
        let* vars = vars in
        let where = F.Path_flag.Flagged.path wheref in
        lift_opt_gen (gen' vars ~where ~forbid_already_written))
  end)

  let available : F.Availability.t =
    (* The path filter requires the path to be in a thread that has access to
       variables satisfying both source and destination restrictions, so we
       need not specify those restrictions separately. *)
    F.Availability.(
      M.(lift path_filter >>= is_filter_constructible ~kind:Insert))

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

  module MList = Tx.List.On_monad (F.State.Monad)

  let bookkeep_new_locals (nls : Fir.Initialiser.t Ac.C_named.Alist.t)
      ~(tid : int) : unit F.State.Monad.t =
    MList.iter_m nls ~f:(fun (name, init) ->
        F.State.Monad.register_var (Ac.Litmus_id.local tid name) init)

  let do_bookkeeping (item : B.t) ~(path : F.Path.Flagged.t) :
      unit F.State.Monad.t =
    let tid = path |> F.Path_flag.Flagged.path |> F.Path.tid in
    F.State.Monad.(
      Let_syntax.(
        let%bind () = bookkeep_new_locals ~tid (B.new_locals item) in
        let%bind () = bookkeep_dsts ~tid (B.dst_ids item) in
        add_expression_dependencies_at_path ~path (B.src_exprs item)))

  let insert_vars (target : F.Subject.Test.t)
      (new_locals : Fir.Initialiser.t Ac.C_named.Alist.t) ~(tid : int) :
      F.Subject.Test.t Or_error.t =
    Tx.List.With_errors.fold_m new_locals ~init:target
      ~f:(fun subject (id, init) ->
        F.Subject.Test.declare_var subject (Ac.Litmus_id.local tid id) init)

  let do_insertions (target : F.Subject.Test.t) ~(path : F.Path.Flagged.t)
      ~(to_insert : B.t) : F.Subject.Test.t Or_error.t =
    let tid = F.Path.tid (F.Path_flag.Flagged.path path) in
    let stms =
      Fir.(
        to_insert |> B.to_stms
        |> List.map ~f:(Statement.prim F.Metadata.generated))
    in
    Or_error.Let_syntax.(
      let%bind target' =
        F.Path_consumers.consume_with_flags target ~filter:B.path_filter
          ~path ~action:(Insert stms)
      in
      insert_vars target' (B.new_locals to_insert) ~tid)

  let run (subject : F.Subject.Test.t)
      ~(payload : B.t F.Payload_impl.Insertion.t) :
      F.Subject.Test.t F.State.Monad.t =
    let to_insert = F.Payload_impl.Insertion.to_insert payload in
    let path = F.Payload_impl.Insertion.where payload in
    F.State.Monad.(
      Let_syntax.(
        let%bind () = do_bookkeeping to_insert ~path in
        Monadic.return (do_insertions subject ~path ~to_insert)))
end
