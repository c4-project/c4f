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
end

module Fence_payload = struct
  type t = {path: F.Path.Test.t; fence: Fir.Atomic_fence.t}
  [@@deriving sexp, make]
end

module Fence :
  F.Action_types.S
    with type Payload.t = Fir.Atomic_fence.t F.Payload.Insertion.t = struct
  let name : Ac.Id.t = Ac.Id.of_string "mem.fence"

  let readme () : string =
    Act_utils.My_string.format_for_readme
      {| Inserts a randomly generated memory fence into the test. |}

  module Payload = F.Payload.Insertion.Make (struct
    type t = Fir.Atomic_fence.t [@@deriving sexp]

    let name = name

    let path_filter = F.State.Monad.return F.Path_filter.empty

    let gen (_ : F.Path.Test.t) (_ : F.Subject.Test.t)
        ~(random : Splittable_random.State.t) ~(param_map : F.Param_map.t) :
        Fir.Atomic_fence.t F.State.Monad.t =
      ignore param_map ;
      F.Payload.Helpers.lift_quickcheck Fir.Atomic_fence.quickcheck_generator
        ~random
  end)

  let available = F.Availability.has_threads

  let run (subject : F.Subject.Test.t)
      ~(payload : Fir.Atomic_fence.t F.Payload.Insertion.t) :
      F.Subject.Test.t F.State.Monad.t =
    let path = F.Payload.Insertion.where payload in
    let fence_stm =
      payload |> F.Payload.Insertion.to_insert
      |> Act_fir.Prim_statement.atomic_fence
      |> Act_fir.Statement.prim F.Metadata.generated
    in
    (* We don't need to do any bookkeeping on fences. *)
    F.State.Monad.Monadic.return
      (F.Path_consumers.consume subject ~path ~action:(Insert [fence_stm]))
end

module Strengthen_payload = struct
  type t = {path: F.Path.Test.t; mo: Fir.Mem_order.t; can_weaken: bool}
  [@@deriving sexp, make]
end

module Strengthen :
  F.Action_types.S with type Payload.t = Strengthen_payload.t = struct
  module Name = struct
    let name : Ac.Id.t = Ac.Id.("mem" @: "strengthen" @: empty)
  end

  include Name
  include F.Action.Make_log (Name)

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

  let filter : F.Path_filter.t =
    F.Path_filter.(
      empty
      |> require_end_check
           ~check:(End_check.Stm_class (Is, Fir.Statement_class.[atomic ()])))

  module Payload = struct
    include Strengthen_payload

    let gen_path (o : Ac.Output.t) (subject : F.Subject.Test.t)
        ~(random : Splittable_random.State.t) : F.Path.Test.t F.State.Monad.t
        =
      log o "generating path" ;
      F.Payload.Helpers.lift_quickcheck_opt ~random ~action_id:name
        (F.Path_producers.try_gen subject ~filter ~kind:Transform)

    let gen_mo (o : Ac.Output.t) ~(random : Splittable_random.State.t) :
        Fir.Mem_order.t F.State.Monad.t =
      log o "generating memory order" ;
      F.Payload.Helpers.lift_quickcheck ~random
        Fir.Mem_order.quickcheck_generator

    let gen (subject : F.Subject.Test.t)
        ~(random : Splittable_random.State.t) ~(param_map : F.Param_map.t) :
        t F.State.Monad.t =
      F.State.Monad.(
        Let_syntax.(
          let%bind weaken_flag =
            Monadic.return
              (F.Config_tables.unsafe_weaken_orders_flag param_map)
          in
          let can_weaken = F.Flag.eval weaken_flag ~random in
          let%bind o = output () in
          let%bind path = gen_path o subject ~random in
          let%map mo = gen_mo o ~random in
          {path; mo; can_weaken}))
  end

  let available : F.Availability.t =
    F.Availability.is_filter_constructible filter ~kind:Transform

  module On_atomics =
    Travesty.Traversable.Chain0
      (F.Subject.Statement.On_primitives)
      (Act_fir.Prim_statement.On_atomics)
  module On_mos =
    Travesty.Traversable.Chain0
      (On_atomics)
      (Fir.Atomic_statement.On_mem_orders)

  let change_mo (stm : F.Subject.Statement.t) ~(mo : Act_fir.Mem_order.t)
      ~(can_weaken : bool) : F.Subject.Statement.t Or_error.t =
    let direction = if can_weaken then `Any else `Strengthen in
    Ok
      (On_mos.map stm
         ~f:(Act_fir.Mem_order.try_change ~replacement:mo ~direction))

  let run (subject : F.Subject.Test.t)
      ~payload:({path; mo; can_weaken} : Payload.t) :
      F.Subject.Test.t F.State.Monad.t =
    F.State.Monad.Monadic.return
      (F.Path_consumers.consume subject ~path ~filter
         ~action:(Transform (change_mo ~mo ~can_weaken)))
end
