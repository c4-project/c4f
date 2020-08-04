(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Fir = Act_fir
  module F = Act_fuzz
end

module Surround :
  F.Action_types.S with type Payload.t = F.Payload.Cond_surround.t = struct
  let name = Act_common.Id.of_string_list ["flow"; "loop"; "surround"]

  let readme () : string =
    Act_utils.My_string.format_for_readme
      {| Removes a sublist
       of statements from the program, replacing them with a `do... while`
       statement containing some transformation of the removed statements.

       The condition of the `do... while` loop is statically guaranteed to be
       false. |}

  let available (ctx : F.Availability.Context.t) : bool Or_error.t =
    Ok
      (ctx |> F.Availability.Context.subject |> F.Subject.Test.has_statements)

  module Surround = F.Payload.Cond_surround

  module Payload = Surround.Make (struct
    let name = name

    let cond_gen : Fir.Env.t -> Fir.Expression.t Base_quickcheck.Generator.t
        =
      Fir.Expression_gen.gen_falsehoods

    let path_filter : F.Path_filter.t F.State.Monad.t =
      F.State.Monad.return F.Path_filter.empty
  end)

  let wrap_in_loop (cond : Fir.Expression.t)
      (statements : F.Metadata.t Fir.Statement.t list) :
      F.Metadata.t Fir.Statement.t =
    Fir.Statement.flow
      (Fir.Flow_block.while_loop ~kind:Do_while ~cond
         ~body:(F.Subject.Block.make_generated ~statements ()))

  let run (test : F.Subject.Test.t) ~(payload : Payload.t) :
      F.Subject.Test.t F.State.Monad.t =
    Surround.apply payload ~test ~f:wrap_in_loop
end
