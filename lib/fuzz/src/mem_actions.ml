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
  module Cm = Act_fir
  module P = Payload
end

module Fence_payload = struct
  type t = {path: Path.Program.t; fence: Cm.Atomic_fence.t}
  [@@deriving sexp, make]
end

module Fence :
  Action_types.S with type Payload.t = Cm.Atomic_fence.t P.Insertion.t =
struct
  let name : Ac.Id.t = Ac.Id.of_string "mem.fence"

  let readme () : string =
    Act_utils.My_string.format_for_readme
      {| Inserts a randomly generated memory fence into the test. |}

  module Payload = P.Insertion.Make (struct
    type t = Cm.Atomic_fence.t [@@deriving sexp]

    let name = name

    let path_filter = State.Monad.return Path_filter.empty

    let gen (_ : Path.Program.t) (_ : Subject.Test.t)
        ~(random : Splittable_random.State.t) ~(param_map : Param_map.t) :
        Cm.Atomic_fence.t State.Monad.t =
      ignore param_map ;
      Payload.Helpers.lift_quickcheck Cm.Atomic_fence.quickcheck_generator
        ~random
  end)

  let available = Availability.has_threads

  let run (subject : Subject.Test.t)
      ~(payload : Cm.Atomic_fence.t P.Insertion.t) :
      Subject.Test.t State.Monad.t =
    let path = P.Insertion.where payload in
    let fence_stm =
      payload |> P.Insertion.to_insert |> Act_fir.Prim_statement.atomic_fence
      |> Act_fir.Statement.prim Metadata.generated
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
      empty
      |> require_end_check
           ~check:(End_check.Is_of_class Cm.Statement_class.[atomic ()]))

  module Payload = struct
    include Strengthen_payload

    let gen_path (o : Ac.Output.t) (subject : Subject.Test.t)
        ~(random : Splittable_random.State.t) : Path.Program.t State.Monad.t
        =
      log o "generating path" ;
      Payload.Helpers.lift_quickcheck_opt ~random ~action_id:name
        (Path_producers.try_gen_transform_stm subject ~filter)

    let gen_mo (o : Ac.Output.t) ~(random : Splittable_random.State.t) :
        Act_fir.Mem_order.t State.Monad.t =
      log o "generating memory order" ;
      Payload.Helpers.lift_quickcheck ~random
        Act_fir.Mem_order.quickcheck_generator

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

  let available : Availability.t =
    Availability.is_filter_constructible filter

  module On_atomics =
    Travesty.Traversable.Chain0
      (Subject.Statement.On_primitives)
      (Act_fir.Prim_statement.On_atomics)
  module On_mos =
    Travesty.Traversable.Chain0
      (On_atomics)
      (Act_fir.Atomic_statement.On_mem_orders)

  let change_mo (stm : Subject.Statement.t) ~(mo : Act_fir.Mem_order.t)
      ~(can_weaken : bool) : Subject.Statement.t Or_error.t =
    let direction = if can_weaken then `Any else `Strengthen in
    Ok
      (On_mos.map stm
         ~f:(Act_fir.Mem_order.try_change ~replacement:mo ~direction))

  let run (subject : Subject.Test.t)
      ~payload:({path; mo; can_weaken} : Payload.t) :
      Subject.Test.t State.Monad.t =
    State.Monad.Monadic.return
      (Path_consumers.Test.transform_stm path ~target:subject
         ~f:(change_mo ~mo ~can_weaken))
end
