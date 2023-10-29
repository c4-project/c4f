(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let lift_prims (ps : Fir.Prim_statement.t list) ~(meta : Fuzz.Metadata.t) :
    Fuzz.Subject.Statement.t list =
  List.map ps ~f:(fun value ->
      Accessor.construct Fir.Statement.prim {meta; value} )

(** Lists the restrictions we put on source variables. *)
let basic_src_restrictions : (Fuzz.Var.Record.t -> bool) list Lazy.t =
  lazy []

let basic_dst_restrictions (dst_type : Fir.Type.Basic.t) :
    (Fuzz.Var.Record.t -> bool) list =
  let bt =
    Accessor.(Fuzz.Var.Record.Access.type_of @> Fir.Type.Access.basic_type)
  in
  [Fir.Type.Basic.eq bt ~to_:dst_type]

module Make (B : Storelike_types.Basic) :
  Fuzz.Action_types.S with type Payload.t = B.t Fuzz.Payload_impl.Pathed.t =
struct
  let name = B.name

  (** [readme_chunks ()] generates fragments of unformatted README text based
      on the configuration of this store module. *)
  let readme_chunks () : string list =
    B.readme_preamble
    @ [ Printf.sprintf "This operation generates '%s's."
          (Fir.Type.Basic.to_string B.dst_type) ]

  let readme =
    lazy
      ( readme_chunks () |> String.concat ~sep:"\n\n"
      |> C4f_utils.My_string.format_for_readme )

  let src_env (vars : Fuzz.Var.Map.t) ~(tid : int) : Fir.Env.t =
    let predicates = Lazy.force basic_src_restrictions in
    Fuzz.Var.Map.env_satisfying_all ~predicates ~scope:(Local tid) vars

  let dst_restrictions : (Fuzz.Var.Record.t -> bool) list Lazy.t =
    lazy (basic_dst_restrictions B.dst_type @ B.extra_dst_restrictions)

  let dst_env (vars : Fuzz.Var.Map.t) ~(tid : int) : Fir.Env.t =
    let predicates = Lazy.force dst_restrictions in
    Fuzz.Var.Map.env_satisfying_all ~predicates ~scope:(Local tid) vars

  let execute_multi_path_filter (f : Fuzz.Path_filter.t) : Fuzz.Path_filter.t
      =
    match B.Flags.execute_multi_safe with
    | `Never ->
        (* There is no point in generating paths that insert into
           multi-execution contexts, as their construction will never be
           sound unless we insert into dead-code. Presently we don't have any
           way of overriding one flag with the other, so we
           over-approximate. *)
        Fuzz.Path_filter.(forbid_flag In_execute_multi + f)
    | `Always | `If_no_cycles ->
        (* We assume that if a storelike reports itself as constructible in
           execute-multi contexts, there will always exist some payload that
           can be generated at filtered paths that is sound. *)
        f

  let path_filter ({vars; _} : Fuzz.State.t) =
    Fuzz.Path_filter.(
      in_thread_with_variables vars
        ~predicates:
          (List.append
             (Lazy.force dst_restrictions)
             (Lazy.force basic_src_restrictions) )
      + execute_multi_path_filter B.path_filter )

  let has_dependency_cycle (to_insert : B.t) : bool =
    (* TODO(@MattWindsor91): this is very heavy-handed; we should permit
       references to the destination in the source wherever they are not
       depended-upon, but how do we do this? *)
    let dsts = B.dst_ids to_insert in
    let srcs = B.src_exprs to_insert in
    Fir.(
      Accessor.(
        exists (List.each @> Expression_traverse.depended_upon_idents) )
        ~f:(fun id ->
          Option.is_some (List.find dsts ~f:(Common.C_id.equal id)) ) )
      srcs

  (** [apply_once_only to_insert ~path_flags] decides whether [to_insert],
      inserted into a path containing [path_flags], needs the [Once_only]
      metadata restriction set. This prevents actions that would wrap
      [to_insert] in a multi-execution loop from doing so. *)
  let apply_once_only (to_insert : B.t)
      ~(path_flags : Set.M(Fuzz.Path_meta.Flag).t) : bool =
    if Set.mem path_flags In_dead_code then false
    else
      match B.Flags.execute_multi_safe with
      | `Always -> false
      | `If_no_cycles -> has_dependency_cycle to_insert
      | `Never -> true

  module Payload = struct
    type t = B.t Fuzz.Payload_impl.Pathed.t [@@deriving sexp]

    let path_filter = path_filter

    let error_if_empty (env_name : string) (env : Fir.Env.t) :
        unit Or_error.t =
      Tx.Or_error.when_m (Map.is_empty env) ~f:(fun () ->
          Or_error.error_s
            [%message
              "Internal error: Environment was empty." ~here:[%here]
                ~env_name] )

    let check_envs (src : Fir.Env.t) (dst : Fir.Env.t) : unit Or_error.t =
      Or_error.combine_errors_unit
        [error_if_empty "src" src; error_if_empty "dst" dst]

    let filter_for_loop_safety (gen : B.t Q.Generator.t)
        ~(path_flags : Set.M(Fuzz.Path_meta.Flag).t) : B.t Q.Generator.t =
      (* We need to make sure that there is no situation where both
         In_execute_multi and Execute_multi_unsafe are generated on the same
         item. *)
      if Set.mem path_flags In_execute_multi then
        Q.Generator.filter gen ~f:(Fn.non (apply_once_only ~path_flags))
      else gen

    let gen_opt (vars : Fuzz.Var.Map.t) ~(where : Fuzz.Path.With_meta.t) :
        B.t Fuzz.Opt_gen.t =
      let tid = Fuzz.Path.tid where.path in
      let src = src_env vars ~tid in
      let dst = dst_env vars ~tid in
      Or_error.Let_syntax.(
        let%map () = check_envs src dst in
        filter_for_loop_safety ~path_flags:where.meta.flags
          (B.gen ~src ~dst ~vars ~tid) )

    let gen' (where : Fuzz.Path.With_meta.t) : B.t Fuzz.Payload_gen.t =
      Fuzz.Payload_gen.(
        let* vars = vars in
        lift_opt_gen (gen_opt vars ~where) )

    let gen = Fuzz.Payload_impl.Pathed.gen Insert path_filter gen'
  end

  let recommendations = B.recommendations

  let available : Fuzz.Availability.t =
    (* The path filter requires the path to be in a thread that has access to
       variables satisfying both source and destination restrictions, so we
       need not specify those restrictions separately. *)
    Fuzz.Availability.(
      in_var_cap ~after_adding:B.new_local_cap
      + M.(lift_state path_filter >>= is_filter_constructible ~kind:Insert) )

  let bookkeep_dst (x : Common.C_id.t) ~(tid : int) : unit Fuzz.State.Monad.t
      =
    Fuzz.State.Monad.(
      Let_syntax.(
        let%bind dst_var = resolve x ~scope:(Local tid) in
        let%bind () = add_write dst_var in
        when_m B.Flags.erase_known_values ~f:(fun () ->
            erase_var_value dst_var ) ) )

  let bookkeep_dsts (xs : Common.C_id.t list) ~(tid : int) :
      unit Fuzz.State.Monad.t =
    xs |> List.map ~f:(bookkeep_dst ~tid) |> Fuzz.State.Monad.all_unit

  module AState = Accessor.Of_monad (struct
    include Fuzz.State.Monad

    let apply = `Define_using_bind
  end)

  let bookkeep_new_locals (nls : Fir.Initialiser.t Common.C_named.Alist.t)
      ~(tid : int) : unit Fuzz.State.Monad.t =
    AState.iter Accessor.List.each nls ~f:(fun (name, init) ->
        Fuzz.State.Monad.register_var (Common.Litmus_id.local tid name) init )

  let do_bookkeeping (item : B.t) ~(path : Fuzz.Path.With_meta.t) :
      unit Fuzz.State.Monad.t =
    let tid = Fuzz.Path.tid path.path in
    Fuzz.State.Monad.(
      all_unit
        [ bookkeep_new_locals ~tid (B.new_locals item)
        ; bookkeep_dsts ~tid (B.dst_ids item)
        ; add_expression_dependencies_at_path ~path (B.src_exprs item) ] )

  let insert_vars (target : Fuzz.Subject.Test.t)
      ~(new_locals : Fir.Initialiser.t Common.C_named.Alist.t) ~(tid : int) :
      Fuzz.Subject.Test.t Or_error.t =
    Tx.List.With_errors.fold_m new_locals ~init:target
      ~f:(fun subject (id, init) ->
        (* Variable registration happens as part of the bookkeeping pass. *)
        Fuzz.Subject.Test.declare_var subject
          (Common.Litmus_id.local tid id)
          init )

  let stm_metadata (to_insert : B.t)
      (path_flags : Set.M(Fuzz.Path_meta.Flag).t) : Fuzz.Metadata.t =
    let restrictions =
      if apply_once_only to_insert ~path_flags then
        Set.singleton (module Fuzz.Metadata.Restriction) Once_only
      else Set.empty (module Fuzz.Metadata.Restriction)
    in
    Generated (Fuzz.Metadata.Gen.make ~restrictions ())

  let to_stms_with_metadata (to_insert : B.t)
      (path_flags : Set.M(Fuzz.Path_meta.Flag).t) :
      Fuzz.Subject.Statement.t list =
    let meta = stm_metadata to_insert path_flags in
    B.to_stms to_insert ~meta

  let do_insertions (target : Fuzz.Subject.Test.t)
      ~(path : Fuzz.Path.With_meta.t) ~(to_insert : B.t)
      ~(filter : Fuzz.Path_filter.t) : Fuzz.Subject.Test.t Or_error.t =
    let tid = Fuzz.Path.tid path.path in
    let stms = to_stms_with_metadata to_insert path.meta.flags in
    Or_error.(
      target
      |> Fuzz.Path_consumers.consume ~filter ~path ~action:(Insert stms)
      >>= insert_vars ~new_locals:(B.new_locals to_insert) ~tid )

  let run (subject : Fuzz.Subject.Test.t)
      ~(payload : B.t Fuzz.Payload_impl.Pathed.t) :
      Fuzz.Subject.Test.t Fuzz.State.Monad.t =
    let to_insert = payload.payload in
    let path = payload.where in
    Fuzz.State.Monad.(
      Let_syntax.(
        (* This can't be moved after the bookkeeping stage; otherwise, the
           assumptions the path filter makes about the variables used in the
           cmpxchg will change and may make the filter overly restrictive. *)
        let%bind filter = peek path_filter in
        let%bind () = do_bookkeeping to_insert ~path in
        Monadic.return (do_insertions subject ~path ~to_insert ~filter) ) )
end
