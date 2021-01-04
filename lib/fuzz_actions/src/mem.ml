(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Fence :
  Fuzz.Action_types.S
    with type Payload.t = Fir.Atomic_fence.t Fuzz.Payload_impl.Pathed.t =
struct
  let name : Common.Id.t = Common.Id.of_string "mem.fence"

  let readme : string Lazy.t =
    lazy {| Inserts a randomly generated memory fence into the test. |}

  module Payload = struct
    type t = Fir.Atomic_fence.t Fuzz.Payload_impl.Pathed.t [@@deriving sexp]

    let path_filter _ = Fuzz.Path_filter.zero

    let gen' (_ : Fuzz.Path.With_meta.t) :
        Fir.Atomic_fence.t Fuzz.Payload_gen.t =
      Fuzz.Payload_gen.lift_quickcheck Fir.Atomic_fence.quickcheck_generator

    let gen = Fuzz.Payload_impl.Pathed.gen Insert path_filter gen'
  end

  let recommendations (_ : Payload.t) : Common.Id.t list = []

  let available = Fuzz.Availability.has_threads

  let run (test : Fuzz.Subject.Test.t)
      ~(payload : Fir.Atomic_fence.t Fuzz.Payload_impl.Pathed.t) :
      Fuzz.Subject.Test.t Fuzz.State.Monad.t =
    (* We don't need to do any bookkeeping on fences. *)
    Fuzz.Payload_impl.Pathed.insert payload ~test ~f:(fun payload ->
        payload
        |> Accessor.construct
             Fir.(Prim_statement.atomic @> Atomic_statement.fence)
        |> Fuzz.Subject.Statement.make_generated_prim)
end

module Strengthen_payload = struct
  type t =
    {path: Fuzz.Path.With_meta.t; mo: Fir.Mem_order.t; can_weaken: bool}
  [@@deriving sexp, make]
end

module Strengthen :
  Fuzz.Action_types.S with type Payload.t = Strengthen_payload.t = struct
  module Name = struct
    let name : Common.Id.t = Common.Id.("mem" @: "strengthen" @: empty)
  end

  include Name
  include Fuzz.Action.Make_log (Name)

  let readme : string Lazy.t =
    lazy
      ( {| Replaces the memory order of a random atomic statement (not an atomic
         expression) with another memory order.

         Usually, this will only perform the replacement when the new memory
         order is compatible with the atomic action and also stronger than the
         old one.  If '|}
      ^ Common.Id.to_string Fuzz.Config_tables.unsafe_weaken_orders_flag
      ^ {|' is true, this action will
         permit weakening of memory orders, likely resulting in a loss of
         semantics preservation.
      |}
      )

  let filter : Fuzz.Path_filter.t =
    Fuzz.Path_filter.(
      require_end_check
        (End_check.Stm_class (Is, Fir.Statement_class.[atomic ()])))

  module Payload = struct
    include Strengthen_payload

    let gen : t Fuzz.Payload_gen.t =
      Fuzz.Payload_gen.(
        let* can_weaken =
          flag Fuzz.Config_tables.unsafe_weaken_orders_flag
        in
        let* path = path_with_flags Transform ~filter in
        let+ mo = lift_quickcheck Fir.Mem_order.quickcheck_generator in
        {path; mo; can_weaken})
  end

  let recommendations (_ : Payload.t) : Common.Id.t list = []

  let available : Fuzz.Availability.t =
    Fuzz.Availability.is_filter_constructible filter ~kind:Transform

  module On_atomics =
    Travesty.Traversable.Chain0
      (Fuzz.Subject.Statement.On_primitives)
      (C4f_fir.Prim_statement.On_atomics)
  module On_mos =
    Travesty.Traversable.Chain0
      (On_atomics)
      (Fir.Atomic_statement.On_mem_orders)

  let change_mo (stm : Fuzz.Subject.Statement.t) ~(mo : C4f_fir.Mem_order.t)
      ~(can_weaken : bool) : Fuzz.Subject.Statement.t Or_error.t =
    let direction = if can_weaken then `Any else `Strengthen in
    Ok
      (On_mos.map stm
         ~f:(C4f_fir.Mem_order.try_change ~replacement:mo ~direction))

  let run (subject : Fuzz.Subject.Test.t)
      ~payload:({path; mo; can_weaken} : Payload.t) :
      Fuzz.Subject.Test.t Fuzz.State.Monad.t =
    Fuzz.State.Monad.Monadic.return
      (Fuzz.Path_consumers.consume subject ~path ~filter
         ~action:(Transform (change_mo ~mo ~can_weaken)))
end
