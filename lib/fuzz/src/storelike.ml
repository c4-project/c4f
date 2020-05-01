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
  module Cm = Act_c_mini
  module P = Payload
end

let forbid_already_written_flag_key : Ac.Id.t =
  Ac.Id.("store" @: "forbid-already-written" @: empty)

let forbid_already_written_flag (param_map : Param_map.t) :
    Flag.t State.Monad.t =
  Param_map.get_flag_m param_map ~id:forbid_already_written_flag_key

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
       (Ac.Id.to_string forbid_already_written_flag_key))

(** Lists the restrictions we put on source variables. *)
let basic_src_restrictions : (Var.Record.t -> bool) list Lazy.t = lazy []

let basic_dst_restrictions (dst_type : Cm.Type.Basic.t) :
    (Var.Record.t -> bool) list =
  Var.Record.[is_atomic; was_generated; has_basic_type ~basic:dst_type]

(** Lists the restrictions we put on destination variables. *)
let dst_restrictions ~(dst_type : Cm.Type.Basic.t)
    ~(forbid_already_written : bool) : (Var.Record.t -> bool) list =
  basic_dst_restrictions dst_type
  @ List.filter_opt
      [Option.some_if forbid_already_written (Fn.non Var.Record.has_writes)]

module Make (B : sig
  val name : Ac.Id.t
  (** [name] is the name of the action. *)

  val readme_preamble : string list
  (** [readme_preamble] is the part of the action readme specific to this
      form of the storelike action. *)

  val dst_type : Cm.Type.Basic.t
  (** [dst_type] is the value type of the destination. *)

  val path_filter : Path_filter.t
  (** [path_filter] is the filter to apply on statement insertion paths
      before considering them for the atomic store. *)

  val extra_dst_restrictions : (Var.Record.t -> bool) list
  (** [extra_dst_restrictions] is a list of additional restrictions to place
      on the destination variables (for example, 'must not have
      dependencies'). *)

  module Flags : Storelike_types.Flags

  include Storelike_types.Basic
end) : Action_types.S with type Payload.t = B.t P.Insertion.t = struct
  let name = B.name

  (** [readme_chunks ()] generates fragments of unformatted README text based
      on the configuration of this store module. *)
  let readme_chunks () : string list =
    B.readme_preamble
    @ [ Printf.sprintf "This operation generates '%s's."
          (Cm.Type.Basic.to_string B.dst_type)
      ; Lazy.force readme_faw ]

  let readme () =
    readme_chunks () |> String.concat ~sep:"\n\n"
    |> Act_utils.My_string.format_for_readme

  let src_env (vars : Var.Map.t) ~(tid : int) : Cm.Env.t =
    let predicates = Lazy.force basic_src_restrictions in
    Var.Map.env_satisfying_all ~predicates ~scope:(Local tid) vars

  let dst_restrictions ~(forbid_already_written : bool) :
      (Var.Record.t -> bool) list =
    dst_restrictions ~dst_type:B.dst_type ~forbid_already_written
    @ B.extra_dst_restrictions

  let dst_env (vars : Var.Map.t) ~(tid : int)
      ~(forbid_already_written : bool) : Cm.Env.t =
    let predicates = dst_restrictions ~forbid_already_written in
    Var.Map.env_satisfying_all ~predicates ~scope:(Local tid) vars

  module Payload = Payload.Insertion.Make (struct
    type t = B.t [@@deriving sexp]

    let action_id = B.name

    let path_filter = State.Monad.return B.path_filter

    module G = Base_quickcheck.Generator

    let error_if_empty (env_name : string) (env : Cm.Env.t) : unit Or_error.t
        =
      if Map.is_empty env then
        Or_error.error_s
          [%message
            "Internal error: Environment was empty." ~here:[%here] ~env_name]
      else Ok ()

    let check_envs (src : Cm.Env.t) (dst : Cm.Env.t) : unit Or_error.t =
      Or_error.Let_syntax.(
        let%bind () = error_if_empty "src" src in
        error_if_empty "dst" dst)

    let gen_with_envs (src : Cm.Env.t) (dst : Cm.Env.t) : t Opt_gen.t =
      Or_error.Let_syntax.(
        let%map () = check_envs src dst in
        let module Src = struct
          let env = src
        end in
        let module Dst = struct
          let env = dst
        end in
        let module Gen = B.Quickcheck (Src) (Dst) in
        [%quickcheck.generator: Gen.t])

    let gen' (vars : Var.Map.t) ~(where : Path.Program.t)
        ~(forbid_already_written : bool) : t Opt_gen.t =
      let tid = Path.Program.tid where in
      let src = src_env vars ~tid in
      let dst = dst_env vars ~tid ~forbid_already_written in
      gen_with_envs src dst

    let gen (where : Path.Program.t) (_ : Subject.Test.t)
        ~(random : Splittable_random.State.t) ~(param_map : Param_map.t) :
        t State.Monad.t =
      State.Monad.Let_syntax.(
        let%bind faw_flag = forbid_already_written_flag param_map in
        let forbid_already_written = Flag.eval faw_flag ~random in
        State.Monad.with_vars_m (fun vars ->
            Payload.Helpers.lift_quickcheck_opt
              (gen' vars ~where ~forbid_already_written)
              ~random ~action_id:name))
  end)

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

  let bookkeep_dst (x : Ac.C_id.t) ~(tid : int) : unit State.Monad.t =
    State.Monad.(
      Let_syntax.(
        let%bind dst_var = resolve x ~scope:(Local tid) in
        let%bind () = add_write dst_var in
        when_m B.Flags.erase_known_values ~f:(fun () ->
            erase_var_value dst_var)))

  let bookkeep_dsts (xs : Ac.C_id.t list) ~(tid : int) : unit State.Monad.t =
    xs |> List.map ~f:(bookkeep_dst ~tid) |> State.Monad.all_unit

  let bookkeep_srcs (srcs : Cm.Expression.t list) ~(tid : int) :
      unit State.Monad.t =
    State.Monad.(
      when_m B.Flags.respect_src_dependencies ~f:(fun () ->
          add_multiple_expression_dependencies srcs ~scope:(Local tid)))

  let do_bookkeeping (item : B.t) ~(tid : int) : unit State.Monad.t =
    State.Monad.Let_syntax.(
      let%bind () = bookkeep_dsts ~tid (B.dst_ids item) in
      bookkeep_srcs ~tid (B.src_exprs item))

  let run (subject : Subject.Test.t) ~(payload : B.t P.Insertion.t) :
      Subject.Test.t State.Monad.t =
    let to_insert = P.Insertion.to_insert payload in
    let stm =
      Cm.(to_insert |> B.to_stm |> Statement.prim Metadata.generated)
    in
    let path = P.Insertion.where payload in
    let tid = Path.Program.tid path in
    State.Monad.Let_syntax.(
      let%bind () = do_bookkeeping to_insert ~tid in
      State.Monad.Monadic.return
        (Path_consumers.Test.insert_stm path ~to_insert:stm ~target:subject))
end
