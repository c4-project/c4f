(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Surround = struct
  module type S =
    Fuzz.Action_types.S with type Payload.t = unit Fuzz.Payload_impl.Pathed.t

  module Make (Basic : sig
    val subname : string
    (** [subname] is the name that should appear at the end of the action ID. *)

    val lock_type : Fir.Flow_block.Lock.t
    (** [lock_type] is the type of lock block being generated. *)

    val lock_type_name : string
    (** [lock_type_name] is the name of the lock type, to interpolate into
        the readme. *)

    val path_filter : Fuzz.Path_filter.t
    (** [path_filter] is the path filter used on paths to statement spans
        being surrounded. *)
  end) : S = Fuzz.Action.Make_surround (struct
    let name =
      Act_common.Id.(
        "flow" @: "lock" @: "surround" @: Basic.subname @: empty)

    let surround_with : string = Basic.lock_type_name ^ " block"

    let readme_suffix = ""

    (* Lock surrounds are always available if there is at least one thread.
       This is because they can always surround a block of zero statements,
       which trivially fulfils any path filter used on surrounds at time of
       writing. *)
    let available : Fuzz.Availability.t = Fuzz.Availability.has_threads

    let path_filter _ = Basic.path_filter

    module Payload = struct
      type t = unit [@@deriving sexp]

      let src_exprs : t -> Fir.Expression.t list = Fn.const []

      let gen (_ : Act_fuzz.Path.Flagged.t) : t Fuzz.Payload_gen.t =
        Fuzz.Payload_gen.return ()
    end

    let recommendations (_ : unit Fuzz.Payload_impl.Pathed.t) :
        Common.Id.t list =
      []

    let run_pre (test : Fuzz.Subject.Test.t) ~(payload : Payload.t) :
        Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      ignore payload ;
      Fuzz.State.Monad.return test

    let wrap (statements : Fuzz.Metadata.t Fir.Statement.t list)
        ~(payload : Payload.t) : Fuzz.Metadata.t Fir.Statement.t =
      ignore (payload : Payload.t) ;
      Accessor.construct Fir.Statement.flow
        (Fir.Flow_block.lock_block ~kind:Basic.lock_type
           (Fuzz.Subject.Block.make_generated ~statements ()))
  end)

  module Atomic : S = Make (struct
    let subname = "atomic"

    let lock_type = Fir.Flow_block.Lock.Atomic

    let lock_type_name = "an atomic"

    let path_filter = Fuzz.Path_filter.transaction_safe
  end)

  module Sync : S = Make (struct
    let subname = "sync"

    let lock_type = Fir.Flow_block.Lock.Synchronized

    let lock_type_name = "a synchronised"

    let path_filter = Fuzz.Path_filter.zero
  end)
end
