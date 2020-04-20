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
end

module Fence_payload = struct
  type t = {path: Path.Program.t; fence: Cm.Atomic_fence.t}
  [@@deriving sexp, make]
end

module Fence : Action_types.S with type Payload.t = Fence_payload.t = struct
  let name : Ac.Id.t = Ac.Id.of_string "mem.fence"

  let readme () : string =
    Act_utils.My_string.format_for_readme
      {| Inserts a randomly generated memory fence into the test. |}

  module Payload = struct
    include Fence_payload

    module PP = Payload.Program_path (struct
      let action_id = name

      let gen = Path_producers.Test.try_gen_insert_stm

      let build_filter = Fn.id
    end)

    let gen (subject : Subject.Test.t) ~(random : Splittable_random.State.t)
        ~(param_map : Param_map.t) : t State.Monad.t =
      State.Monad.Let_syntax.(
        let%map path = PP.gen subject ~random ~param_map
        and fence =
          Payload.Helpers.lift_quickcheck
            Cm.Atomic_fence.quickcheck_generator ~random
        in
        Fence_payload.make ~path ~fence)
  end

  let available = Action.always

  let run (subject : Subject.Test.t)
      ~payload:({path; fence} : Fence_payload.t) :
      Subject.Test.t State.Monad.t =
    let fence_stm =
      fence |> Act_c_mini.Prim_statement.atomic_fence
      |> Act_c_mini.Statement.prim Metadata.generated
    in
    (* We don't need to do any bookkeeping on fences. *)
    State.Monad.Monadic.return
      (Path_consumers.Test.insert_stm path ~to_insert:fence_stm
         ~target:subject)
end

let unsafe_weaken_orders_flag_key : Ac.Id.t =
  Ac.Id.("mem" @: "unsafe-weaken-orders" @: empty)

let unsafe_weaken_orders_flag (param_map : Param_map.t) :
    Flag.t State.Monad.t =
  Param_map.get_flag_m param_map ~id:unsafe_weaken_orders_flag_key

module Strengthen_payload = struct
  type t = {path: Path.Program.t; mo: Cm.Mem_order.t; can_weaken: bool}
  [@@deriving sexp, make]
end

module Strengthen :
  Action_types.S with type Payload.t = Strengthen_payload.t = struct
  module Name = struct
    let name : Ac.Id.t = Ac.Id.("mem" @: "strengthen" @: empty)
  end

  include Name
  include Action.Make_log (Name)

  let readme () : string =
    Act_utils.My_string.format_for_readme
      {| Replaces the memory order of a random atomic statement (not an atomic
         expression) with another memory order.

         Usually, this will only perform the replacement when the new memory
         order is compatible with the atomic action and also stronger than the
         old one.  If 'mem.unsafe-weaken-orders' is true, this action will
         permit weakening of memory orders, likely resulting in a loss of
         semantics preservation.
      |}

  let filter : Path_filter.t =
    Path_filter.(
      empty |> require_end_check ~check:End_check.Is_atomic_statement)

  module Payload = struct
    include Strengthen_payload

    let gen_path (o : Ac.Output.t) (subject : Subject.Test.t)
        ~(random : Splittable_random.State.t) : Path.Program.t State.Monad.t
        =
      log o "generating path" ;
      Payload.Helpers.lift_quickcheck_opt ~random ~action_id:name
        (Path_producers.Test.try_gen_transform_stm subject ~filter)

    let gen_mo (o : Ac.Output.t) ~(random : Splittable_random.State.t) :
        Act_c_mini.Mem_order.t State.Monad.t =
      log o "generating memory order" ;
      Payload.Helpers.lift_quickcheck ~random
        Act_c_mini.Mem_order.quickcheck_generator

    let gen (subject : Subject.Test.t) ~(random : Splittable_random.State.t)
        ~(param_map : Param_map.t) : t State.Monad.t =
      State.Monad.Let_syntax.(
        let%bind weaken_flag = unsafe_weaken_orders_flag param_map in
        let can_weaken = Flag.eval weaken_flag ~random in
        let%bind o = State.Monad.output () in
        let%bind path = gen_path o subject ~random in
        let%map mo = gen_mo o ~random in
        {path; mo; can_weaken})
  end

  let available (subject : Subject.Test.t) ~(param_map : Param_map.t) :
      bool State.Monad.t =
    ignore param_map ;
    State.Monad.return (Path_filter.is_constructible filter ~subject)

  module On_atomics =
    Travesty.Traversable.Chain0
      (Subject.Statement.On_primitives)
      (Act_c_mini.Prim_statement.On_atomics)

  let change_mo_atomic_cmpxchg
      (atom : Act_c_mini.Expression.t Act_c_mini.Atomic_cmpxchg.t)
      ~(mo : Act_c_mini.Mem_order.t) ~(direction : [< `Strengthen | `Any]) :
      Act_c_mini.Expression.t Act_c_mini.Atomic_cmpxchg.t Or_error.t =
    (* TODO(@MattWindsor91): don't change both succ and fail every time *)
    Or_error.return
      Act_c_mini.(
        Atomic_cmpxchg.(
          make ~obj:(obj atom) ~expected:(expected atom)
            ~desired:(desired atom)
            ~succ:
              Mem_order.(
                try_change (succ atom) ~replacement:mo
                  ~is_compatible:is_rmw_compatible ~direction)
            ~fail:
              Mem_order.(
                try_change (fail atom) ~replacement:mo
                  ~is_compatible:is_rmw_compatible ~direction)))

  let change_mo_atomic_fence (atom : Act_c_mini.Atomic_fence.t)
      ~(mo : Act_c_mini.Mem_order.t) ~(direction : [< `Strengthen | `Any]) :
      Act_c_mini.Atomic_fence.t Or_error.t =
    Or_error.return
      Act_c_mini.(
        Atomic_fence.make ~mode:(Atomic_fence.mode atom)
          ~mo:
            Mem_order.(
              try_change (Atomic_fence.mo atom) ~replacement:mo
                ~is_compatible:(Fn.const true) ~direction))

  let change_mo_atomic_fetch
      (atom : Act_c_mini.Expression.t Act_c_mini.Atomic_fetch.t)
      ~(mo : Act_c_mini.Mem_order.t) ~(direction : [< `Strengthen | `Any]) :
      Act_c_mini.Expression.t Act_c_mini.Atomic_fetch.t Or_error.t =
    Or_error.return
      Act_c_mini.(
        Atomic_fetch.make ~obj:(Atomic_fetch.obj atom)
          ~arg:(Atomic_fetch.arg atom) ~op:(Atomic_fetch.op atom)
          ~mo:
            Mem_order.(
              try_change (Atomic_fetch.mo atom) ~replacement:mo
                ~is_compatible:(Fn.const true) ~direction))

  let change_mo_atomic_store (atom : Act_c_mini.Atomic_store.t)
      ~(mo : Act_c_mini.Mem_order.t) ~(direction : [< `Strengthen | `Any]) :
      Act_c_mini.Atomic_store.t Or_error.t =
    Or_error.return
      Act_c_mini.(
        Atomic_store.make ~src:(Atomic_store.src atom)
          ~dst:(Atomic_store.dst atom)
          ~mo:
            Mem_order.(
              try_change (Atomic_store.mo atom) ~replacement:mo
                ~is_compatible:is_store_compatible ~direction))

  module Bm = Act_c_mini.Atomic_statement.Base_map (Or_error)

  let change_mo_atomic (atom : Act_c_mini.Atomic_statement.t)
      ~(mo : Act_c_mini.Mem_order.t) ~(direction : [< `Strengthen | `Any]) :
      Act_c_mini.Atomic_statement.t Or_error.t =
    Bm.bmap atom
      ~cmpxchg:(change_mo_atomic_cmpxchg ~mo ~direction)
      ~fence:(change_mo_atomic_fence ~mo ~direction)
      ~fetch:(change_mo_atomic_fetch ~mo ~direction)
      ~store:(change_mo_atomic_store ~mo ~direction)

  let change_mo (stm : Subject.Statement.t) ~(mo : Act_c_mini.Mem_order.t)
      ~(can_weaken : bool) : Subject.Statement.t Or_error.t =
    let direction = if can_weaken then `Any else `Strengthen in
    On_atomics.With_errors.map_m stm ~f:(change_mo_atomic ~mo ~direction)

  let run (subject : Subject.Test.t)
      ~payload:({path; mo; can_weaken} : Payload.t) :
      Subject.Test.t State.Monad.t =
    State.Monad.Monadic.return
      (Path_consumers.Test.transform_stm path ~target:subject
         ~f:(change_mo ~mo ~can_weaken))
end
