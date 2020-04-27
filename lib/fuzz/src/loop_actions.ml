(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Surround : Action_types.S with type Payload.t = Payload.Surround.t =
struct
  let name = Act_common.Id.of_string_list ["flow"; "loop"; "surround"]

  let readme () : string =
    Act_utils.My_string.format_for_readme
      {| Removes a sublist
       of statements from the program, replacing them with a `do... while`
       statement containing some transformation of the removed statements.

       The condition of the `do... while` loop is statically guaranteed to be
       false. |}

  let available (test : Subject.Test.t) ~(param_map : Param_map.t) :
      bool State.Monad.t =
    ignore (param_map : Param_map.t) ;
    test |> Subject.Test.has_statements |> State.Monad.return

  module Surround = Payload.Surround

  module Payload = Surround.Make (struct
    let action_id = name

    let cond_gen :
           Act_c_mini.Env.t
        -> Act_c_mini.Expression.t Base_quickcheck.Generator.t =
      Act_c_mini.Expression_gen.gen_falsehoods

    let path_filter : Path_filter.t = Path_filter.empty
  end)

  let wrap_in_loop (cond : Act_c_mini.Expression.t)
      (statements : Metadata.t Act_c_mini.Statement.t list) :
      Metadata.t Act_c_mini.Statement.t =
    Act_c_mini.Statement.while_loop
      (Act_c_mini.Statement.While.make ~kind:`Do_while ~cond
         ~body:(Subject.Block.make_generated ~statements ()))

  let run (test : Subject.Test.t) ~(payload : Payload.t) :
      Subject.Test.t State.Monad.t =
    Surround.apply payload ~test ~f:wrap_in_loop
end
