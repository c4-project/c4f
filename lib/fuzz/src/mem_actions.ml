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
  type t =
    { path: Path.Program.t
    ; fence: Cm.Atomic_fence.t
    } [@@deriving sexp, make]
end

module Fence : Action_types.S with type Payload.t = Fence_payload.t = struct
	let name : Ac.Id.t = Ac.Id.of_string "mem.fence"

	let readme () : string =
	  Act_utils.My_string.format_for_readme
	    {| Inserts a randomly generated memory fence into the test. |}

	module Payload = struct
	  include Fence_payload

    module PP =
      Payload.Program_path (struct
        let action_id = name
        let gen = Path_producers.Test.try_gen_insert_stm
        let build_filter = Fn.id
      end)

    let gen (subject : Subject.Test.t) ~(random : Splittable_random.State.t)
        ~(param_map : Param_map.t) : t State.Monad.t =
      State.Monad.Let_syntax.(
      	let%map path = PP.gen subject ~random ~param_map
      	and fence = Payload.Helpers.lift_quickcheck Cm.Atomic_fence.quickcheck_generator ~random in
      	Fence_payload.make ~path ~fence
      )
	end

	let available = Action.always

	let run (subject : Subject.Test.t)
      ~payload:({path; fence} : Fence_payload.t) :
      Subject.Test.t State.Monad.t =
    let fence_stm =
      Act_c_mini.Statement.atomic_fence Metadata.generated fence
    in
    (* We don't need to do any bookkeeping on fences. *)
    State.Monad.Monadic.return
        (Path_consumers.Test.insert_stm path ~to_insert:fence_stm
           ~target:subject)
end
