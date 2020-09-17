(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module A = Accessor
  module Fir = Act_fir
  module F = Act_fuzz
end

module Surround = struct
  module type S = F.Action_types.S with type Payload.t = F.Path.Flagged.t

  module Make (Basic : sig
    val subname : string
    (** [subname] is the name that should appear at the end of the action ID. *)

    val lock_type : Fir.Flow_block.Lock.t
    (** [lock_type] is the type of lock block being generated. *)

    val lock_type_name : string
    (** [lock_type_name] is the name of the lock type, to interpolate into
        the readme. *)

    val path_filter : F.Path_filter.t
    (** [path_filter] is the path filter used on paths to statement spans
        being surrounded. *)
  end) : S = F.Action.Make_surround (struct
    let name =
      Act_common.Id.(
        "flow" @: "lock" @: "surround" @: Basic.subname @: empty)

    let surround_with : string = Basic.lock_type_name ^ " block"

    let readme_suffix = ""

    (* Lock surrounds are always available if there is at least one thread.
       This is because they can always surround a block of zero statements,
       which trivially fulfils any path filter used on surrounds at time of
       writing. *)
    let available : F.Availability.t = F.Availability.has_threads

    module Payload = struct
      type t = F.Path.Flagged.t [@@deriving sexp]

      let where : t -> F.Path.Flagged.t = Fn.id

      let src_exprs : t -> Fir.Expression.t list = Fn.const []

      let gen : t F.Payload_gen.t =
        F.Payload_gen.path_with_flags Transform_list
          ~filter:Basic.path_filter
    end

    let run_pre (test : F.Subject.Test.t) ~(payload : Payload.t) :
        F.Subject.Test.t F.State.Monad.t =
      ignore payload ; F.State.Monad.return test

    let wrap (statements : F.Metadata.t Fir.Statement.t list)
        ~(payload : Payload.t) : F.Metadata.t Fir.Statement.t =
      ignore (payload : Payload.t) ;
      A.construct Fir.Statement.flow
        (Fir.Flow_block.lock_block ~kind:Basic.lock_type
           (F.Subject.Block.make_generated ~statements ()))
  end)

  module Atomic : S = Make (struct
    let subname = "atomic"

    let lock_type = Fir.Flow_block.Lock.Atomic

    let lock_type_name = "an atomic"

    let path_filter = F.Path_filter.(transaction_safe @@ empty)
  end)

  module Sync : S = Make (struct
    let subname = "sync"

    let lock_type = Fir.Flow_block.Lock.Synchronized

    let lock_type_name = "a synchronised"

    let path_filter = F.Path_filter.empty
  end)
end
