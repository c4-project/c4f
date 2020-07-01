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
  module Tx = Travesty_base_exts
  module P = Payload
end

let early_out_name = Ac.Id.of_string_list ["flow"; "dead"; "early-out"]

let goto_name = Ac.Id.of_string_list ["flow"; "dead"; "goto"]

module Early_out_payload = struct
  (* TODO(@MattWindsor91): We can't easily refactor this into an
     Insertion.Make payload, because the path filter depends on the choice of
     early-out. We'd have to split it into three different actions first. *)

  type t = Act_fir.Early_out.t P.Insertion.t [@@deriving sexp]

  let quickcheck_path (test : Subject.Test.t)
      ~(filter_f : Path_filter.t -> Path_filter.t) : Path.Program.t Opt_gen.t
      =
    let filter = Path_filter.(empty |> in_dead_code_only |> filter_f) in
    Path_producers.try_gen_insert_stm ~filter test

  let quickcheck_generic_payload (test : Subject.Test.t)
      ~(kind_pred : Act_fir.Early_out.t -> bool)
      ~(filter_f : Path_filter.t -> Path_filter.t) : t Opt_gen.t =
    Opt_gen.map2
      (quickcheck_path test ~filter_f)
      (Or_error.return
         (Base_quickcheck.Generator.filter ~f:kind_pred
            Act_fir.Early_out.quickcheck_generator))
      ~f:(fun where to_insert -> P.Insertion.make ~where ~to_insert)

  let quickcheck_loop_payload : Subject.Test.t -> t Opt_gen.t =
    quickcheck_generic_payload ~kind_pred:Act_fir.Early_out.in_loop_only
      ~filter_f:Path_filter.in_loop_only

  let quickcheck_non_loop_payload : Subject.Test.t -> t Opt_gen.t =
    quickcheck_generic_payload
      ~kind_pred:(Fn.non Act_fir.Early_out.in_loop_only)
      ~filter_f:Fn.id

  let quickcheck_payload (test : Subject.Test.t) : t Opt_gen.t =
    Opt_gen.union
      [quickcheck_loop_payload test; quickcheck_non_loop_payload test]

  let gen (test : Subject.Test.t) ~(random : Splittable_random.State.t)
      ~(param_map : Param_map.t) : t State.Monad.t =
    ignore (param_map : Param_map.t) ;
    Payload.Helpers.lift_quickcheck_opt (quickcheck_payload test) ~random
      ~action_id:early_out_name
end

module Early_out : Action_types.S with type Payload.t = Early_out_payload.t =
struct
  let name = early_out_name

  let readme () =
    Act_utils.My_string.format_for_readme
      {| Inserts a valid 'early-out' statement (break or return) into a random
         dead-code location. |}

  let base_path_filter : Path_filter.t =
    Path_filter.(in_dead_code_only empty)

  let available : Availability.t =
    Availability.is_filter_constructible base_path_filter

  module Payload = Early_out_payload

  let kind_filter (kind : Act_fir.Early_out.t) :
      (Path_filter.t -> Path_filter.t) Staged.t =
    Staged.stage
      ( if Act_fir.Early_out.in_loop_only kind then Path_filter.in_loop_only
      else Fn.id )

  let make_early_out (kind : Act_fir.Early_out.t) : Subject.Statement.t =
    Act_fir.(
      Statement.prim Metadata.generated (Prim_statement.early_out kind))

  let check_path (target : Subject.Test.t) (path : Path.Program.t)
      (kind : Act_fir.Early_out.t) : Subject.Test.t Or_error.t =
    let f = Staged.unstage (kind_filter kind) in
    let filter = f base_path_filter in
    Path_consumers.Test.check_path path ~filter ~target

  let run_inner (test : Subject.Test.t) (path : Path.Program.t)
      (kind : Act_fir.Early_out.t) : Subject.Test.t Or_error.t =
    Or_error.Let_syntax.(
      let%bind target = check_path test path kind in
      Path_consumers.Test.insert_stm path ~target
        ~to_insert:(make_early_out kind))

  let run (test : Subject.Test.t) ~(payload : Payload.t) :
      Subject.Test.t State.Monad.t =
    let path = P.Insertion.where payload in
    let kind = P.Insertion.to_insert payload in
    State.Monad.Monadic.return (run_inner test path kind)
end

module Goto : Action_types.S with type Payload.t = Ac.C_id.t P.Insertion.t =
struct
  let name = goto_name

  let readme () : string =
    Act_utils.My_string.format_for_readme
      {|
        Inserts a jump to a random thread-local label inside a dead-code block.

        This action only fires in dead-code blocks for which there are available
        labels in the same thread; it does not jump outside the thread.
      |}

  let path_filter' (labels : Set.M(Ac.Litmus_id).t) : Path_filter.t =
    let threads_with_labels =
      Set.filter_map (module Int) ~f:Ac.Litmus_id.tid labels
    in
    Path_filter.(
      empty |> in_dead_code_only
      |> Path_filter.in_threads_only ~threads:threads_with_labels)

  module Payload = P.Insertion.Make (struct
    type t = Act_common.C_id.t [@@deriving sexp]

    let name = name

    let path_filter : Path_filter.t State.Monad.t =
      State.Monad.with_labels path_filter'

    let reachable_labels (path : Path.Program.t) :
        Ac.C_id.t list State.Monad.t =
      State.Monad.with_labels (fun labels ->
          labels
          |> Set.filter_map
               (module Ac.C_id)
               ~f:(fun x ->
                 Option.some_if
                   (Ac.Litmus_id.is_in_local_scope x
                      ~from:(Path.Program.tid path))
                   (Ac.Litmus_id.variable_name x) )
          |> Set.to_list )

    let gen (path : Path.Program.t) (_ : Subject.Test.t)
        ~(random : Splittable_random.State.t) ~(param_map : Param_map.t) :
        t State.Monad.t =
      ignore param_map ;
      State.Monad.Let_syntax.(
        let%bind labels_in_tid = reachable_labels path in
        Payload.Helpers.lift_quickcheck
          (Base_quickcheck.Generator.of_list labels_in_tid)
          ~random)
  end)

  let available (ctx : Availability.Context.t) : bool Or_error.t =
    let labels = ctx |> Availability.Context.state |> State.labels in
    Availability.is_filter_constructible (path_filter' labels) ctx

  let run (subject : Subject.Test.t) ~(payload : Payload.t) :
      Subject.Test.t State.Monad.t =
    let path = P.Insertion.where payload in
    let label = P.Insertion.to_insert payload in
    let goto_stm =
      Act_fir.(
        label |> Prim_statement.goto |> Statement.prim Metadata.generated)
    in
    State.Monad.Monadic.return
      (Path_consumers.Test.insert_stm path ~to_insert:goto_stm
         ~target:subject)
end
