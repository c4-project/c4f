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
end

let early_out_name = Ac.Id.of_string_list ["flow"; "dead"; "early-out"]

let goto_name = Ac.Id.of_string_list ["flow"; "dead"; "goto"]

module Early_out_payload = struct
  type t = {path: Path.Program.t; kind: Act_c_mini.Early_out.t}
  [@@deriving sexp, make]

  let quickcheck_path (test : Subject.Test.t)
      ~(filter_f : Path_filter.t -> Path_filter.t) : Path.Program.t Opt_gen.t
      =
    let filter = Path_filter.(empty |> in_dead_code_only |> filter_f) in
    Path_producers.Test.try_gen_insert_stm ~filter test

  let quickcheck_generic_payload (test : Subject.Test.t)
      ~(kind_pred : Act_c_mini.Early_out.t -> bool)
      ~(filter_f : Path_filter.t -> Path_filter.t) : t Opt_gen.t =
    Opt_gen.map2
      (quickcheck_path test ~filter_f)
      (Or_error.return
         (Base_quickcheck.Generator.filter ~f:kind_pred
            Act_c_mini.Early_out.quickcheck_generator))
      ~f:(fun path kind -> make ~path ~kind)

  let quickcheck_loop_payload : Subject.Test.t -> t Opt_gen.t =
    quickcheck_generic_payload ~kind_pred:Act_c_mini.Early_out.in_loop_only
      ~filter_f:Path_filter.in_loop_only

  let quickcheck_non_loop_payload : Subject.Test.t -> t Opt_gen.t =
    quickcheck_generic_payload
      ~kind_pred:(Fn.non Act_c_mini.Early_out.in_loop_only)
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

  let available (test : Subject.Test.t) ~(param_map : Param_map.t) :
      bool State.Monad.t =
    ignore (param_map : Param_map.t) ;
    test |> Subject.Test.has_dead_code_blocks |> State.Monad.return

  module Payload = Early_out_payload

  let kind_filter (kind : Act_c_mini.Early_out.t) :
      (Path_filter.t -> Path_filter.t) Staged.t =
    Staged.stage
      ( if Act_c_mini.Early_out.in_loop_only kind then
        Path_filter.in_loop_only
      else Fn.id )

  let make_early_out (kind : Act_c_mini.Early_out.t) : Subject.Statement.t =
    Act_c_mini.(
      Statement.prim Metadata.generated (Prim_statement.early_out kind))

  let check_path (target : Subject.Test.t) (path : Path.Program.t)
      (kind : Act_c_mini.Early_out.t) : Subject.Test.t Or_error.t =
    let filter =
      Path_filter.(
        empty |> in_dead_code_only |> Staged.unstage (kind_filter kind))
    in
    Path_consumers.Test.check_path path ~filter ~target

  let run_inner (test : Subject.Test.t) (path : Path.Program.t)
      (kind : Act_c_mini.Early_out.t) : Subject.Test.t Or_error.t =
    Or_error.Let_syntax.(
      let%bind target = check_path test path kind in
      Path_consumers.Test.insert_stm path ~target
        ~to_insert:(make_early_out kind))

  let run (test : Subject.Test.t) ~(payload : Payload.t) :
      Subject.Test.t State.Monad.t =
    let {Early_out_payload.path; kind} = payload in
    State.Monad.Monadic.return (run_inner test path kind)
end

module Goto_payload = struct
  type t = {path: Path.Program.t; label: Ac.C_id.t} [@@deriving sexp, make]

  let path_filter (labels : Set.M(Ac.Litmus_id).t) : Path_filter.t =
    let threads_with_labels =
      Set.filter_map (module Int) ~f:Ac.Litmus_id.tid labels
    in
    Path_filter.(
      empty |> in_dead_code_only
      |> Path_filter.in_threads_only ~threads:threads_with_labels)

  let gen (subject : Subject.Test.t) ~(random : Splittable_random.State.t)
      ~(param_map : Param_map.t) : t State.Monad.t =
    State.Monad.with_labels_m (fun labels ->
        let module PP = Payload.Program_path (struct
          let action_id = goto_name

          let gen = Path_producers.Test.try_gen_insert_stm

          let path_filter = path_filter labels
        end) in
        State.Monad.Let_syntax.(
          let%bind path = PP.gen subject ~random ~param_map in
          let tid = Path.Program.tid path in
          let labels_in_tid =
            labels
            |> Set.filter_map
                 (module Ac.C_id)
                 ~f:(fun x ->
                   Option.some_if
                     ([%equal: int option] (Ac.Litmus_id.tid x) (Some tid))
                     (Ac.Litmus_id.variable_name x))
            |> Set.to_list
          in
          let%map label =
            Payload.Helpers.lift_quickcheck
              (Base_quickcheck.Generator.of_list labels_in_tid)
              ~random
          in
          make ~path ~label))
end

module Goto : Action_types.S with type Payload.t = Goto_payload.t = struct
  let name = goto_name

  let readme () : string =
    Act_utils.My_string.format_for_readme
      {|
        Inserts a jump to a random thread-local label inside a dead-code block.

        This action only fires in dead-code blocks for which there are available
        labels in the same thread; it does not jump outside the thread.
      |}

  module Payload = Goto_payload

  let available (subject : Subject.Test.t) ~(param_map : Param_map.t) :
      bool State.Monad.t =
    ignore param_map ;
    State.Monad.with_labels (fun labels ->
        Path_filter.is_constructible ~subject
          (Goto_payload.path_filter labels))

  let run (subject : Subject.Test.t) ~(payload : Payload.t) :
      Subject.Test.t State.Monad.t =
    let {Goto_payload.path; label} = payload in
    let goto_stm =
      Act_c_mini.(
        label |> Prim_statement.goto |> Statement.prim Metadata.generated)
    in
    State.Monad.Monadic.return
      (Path_consumers.Test.insert_stm path ~to_insert:goto_stm
         ~target:subject)
end
