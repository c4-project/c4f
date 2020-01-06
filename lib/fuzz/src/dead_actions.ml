(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

let early_out_name =
  Act_common.Id.of_string_list ["flow"; "dead"; "early-out"]

module Early_out_payload = struct
  type t = {path: Path.Program.t; kind: Act_c_mini.Early_out.Kind.t}
  [@@deriving sexp, make]

  let quickcheck_path (test : Subject.Test.t)
      ~(filter_f : Path_filter.t -> Path_filter.t) : Path.Program.t Opt_gen.t
      =
    let filter = Path_filter.(empty |> in_dead_code_only |> filter_f) in
    Path_producers.Test.try_gen_insert_stm ~filter test

  let quickcheck_generic_payload (test : Subject.Test.t)
      ~(kind_pred : Act_c_mini.Early_out.Kind.t -> bool)
      ~(filter_f : Path_filter.t -> Path_filter.t) : t Opt_gen.t =
    Opt_gen.map2
      (quickcheck_path test ~filter_f)
      (Or_error.return
         (Base_quickcheck.Generator.filter ~f:kind_pred
            Act_c_mini.Early_out.Kind.quickcheck_generator))
      ~f:(fun path kind -> make ~path ~kind)

  let quickcheck_loop_payload : Subject.Test.t -> t Opt_gen.t =
    quickcheck_generic_payload
      ~kind_pred:Act_c_mini.Early_out.Kind.in_loop_only
      ~filter_f:Path_filter.in_loop_only

  let quickcheck_non_loop_payload : Subject.Test.t -> t Opt_gen.t =
    quickcheck_generic_payload
      ~kind_pred:(Fn.non Act_c_mini.Early_out.Kind.in_loop_only)
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

  let kind_filter (kind : Act_c_mini.Early_out.Kind.t) :
      (Path_filter.t -> Path_filter.t) Staged.t =
    Staged.stage
      ( if Act_c_mini.Early_out.Kind.in_loop_only kind then
        Path_filter.in_loop_only
      else Fn.id )

  let make_early_out (kind : Act_c_mini.Early_out.Kind.t) :
      Subject.Statement.t =
    Act_c_mini.(
      Statement.prim
        (Prim_statement.early_out
           (Early_out.make ~meta:Metadata.generated ~kind)))

  let check_path (target : Subject.Test.t) (path : Path.Program.t)
      (kind : Act_c_mini.Early_out.Kind.t) : Subject.Test.t Or_error.t =
    let filter =
      Path_filter.(
        empty |> in_dead_code_only |> Staged.unstage (kind_filter kind))
    in
    Path_consumers.Test.check_path path ~filter ~target

  let run_inner (test : Subject.Test.t) (path : Path.Program.t)
      (kind : Act_c_mini.Early_out.Kind.t) : Subject.Test.t Or_error.t =
    Or_error.Let_syntax.(
      let%bind target = check_path test path kind in
      Path_consumers.Test.insert_stm path ~target
        ~to_insert:(make_early_out kind))

  let run (test : Subject.Test.t) ~(payload : Payload.t) :
      Subject.Test.t State.Monad.t =
    let {Early_out_payload.path; kind} = payload in
    State.Monad.Monadic.return (run_inner test path kind)
end
