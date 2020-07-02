(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Make_surround (Basic : sig
  val subname : string
  (** [subname] is the name that should appear at the end of the action ID. *)

  val lock_type : Act_fir.Flow_block.Lock.t
  (** [lock_type] is the type of lock block being generated. *)

  val lock_type_name : string
  (** [lock_type_name] is the name of the lock type, to interpolate into the
      readme. *)

  val path_filter : Path_filter.t
  (** [path_filter] is the path filter used on paths to statement spans being
      surrounded. *)
end) : Action_types.S with type Payload.t = Path.Test.t =
Action.Make_surround (struct
  let name =
    Act_common.Id.("flow" @: "lock" @: "surround" @: Basic.subname @: empty)

  let surround_with : string = Basic.lock_type_name ^ " block"

  module Payload = struct
    type t = Path.Test.t [@@deriving sexp]

    let where : t -> Path.Test.t = Fn.id

    let gen (test : Subject.Test.t) ~(random : Splittable_random.State.t)
        ~(param_map : Param_map.t) : t State.Monad.t =
      ignore param_map ;
      Payload.Helpers.lift_path_gen test ~action_id:name
        ~f:Path_producers.try_gen_transform_stm_list
        ~filter:(State.Monad.return Basic.path_filter)
        ~random
  end

  let wrap (statements : Metadata.t Act_fir.Statement.t list)
      ~(payload : Payload.t) : Metadata.t Act_fir.Statement.t =
    ignore (payload : Payload.t) ;
    Act_fir.Statement.flow
      (Act_fir.Flow_block.lock_block ~kind:Basic.lock_type
         ~body:(Subject.Block.make_generated ~statements ()))
end)

module Atomic_surround : Action_types.S with type Payload.t = Path.Test.t =
Make_surround (struct
  let subname = "atomic"

  let lock_type = Act_fir.Flow_block.Lock.Atomic

  let lock_type_name = "an atomic"

  let path_filter = Path_filter.(transaction_safe @@ empty)
end)

module Sync_surround : Action_types.S with type Payload.t = Path.Test.t =
Make_surround (struct
  let subname = "sync"

  let lock_type = Act_fir.Flow_block.Lock.Synchronized

  let lock_type_name = "a synchronised"

  let path_filter = Path_filter.empty
end)
