(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

type t = (module Action_types.S)

module With_default_weight = struct
  type t = {action: (module Action_types.S); default_weight: int}
  [@@deriving make, fields]

  let ( @-> ) (action : (module Action_types.S)) (default_weight : int) =
    {action; default_weight}

  let name ({action= (module M); _} : t) : Common.Id.t = M.name
end

module Make_surround (Basic : Action_types.Basic_surround) :
  Action_types.S with type Payload.t = Basic.Payload.t Payload_impl.Pathed.t =
struct
  let name = Basic.name

  let available = Basic.available

  let readme =
    lazy
      (String.concat
         [ {| This action removes a sublist of statements from the program, replacing
          them with |}
         ; Basic.surround_with
         ; {| containing those statements. |}
         ; "\n\n"
         ; Basic.readme_suffix ] )

  module Payload = struct
    type t = Basic.Payload.t Payload_impl.Pathed.t [@@deriving sexp]

    let gen =
      Payload_impl.Pathed.gen Transform_list Basic.path_filter
        Basic.Payload.gen
  end

  let recommendations = Basic.recommendations

  let add_dependencies ({where; payload} : Payload.t) : unit State.Monad.t =
    let src_exprs = Basic.Payload.src_exprs payload in
    State.Monad.add_expression_dependencies_at_path src_exprs ~path:where

  let run (test : Subject.Test.t) ~(payload : Payload.t) :
      Subject.Test.t State.Monad.t =
    State.Monad.(
      Let_syntax.(
        let%bind filter = peek Basic.path_filter in
        let%bind () = add_dependencies payload in
        let%bind test' = Basic.run_pre test ~payload:payload.payload in
        Payload_impl.Pathed.surround ~filter payload ~test:test'
          ~f:(fun payload -> Basic.wrap ~payload)))
end

module Make_log (B : sig
  val name : Common.Id.t
end) : sig
  val log : Common.Output.t -> ('a, Formatter.t, unit) format -> 'a
end = struct
  let log (o : Common.Output.t) (format : ('a, Formatter.t, unit) format) :
      'a =
    Fmt.(
      Common.Output.pv o
        Caml.("@[<h>%a:@ @[" ^^ format ^^ "@]@]@.")
        (styled (`Fg `Green) Common.Id.pp)
        B.name)
end

module Nop : Action_types.S with type Payload.t = unit = struct
  let name = Common.Id.("nop" @: empty)

  let readme =
    lazy
      {| Does nothing, but consumes an action step.

         This action is automatically executed if no other actions are available
         during a step.  If a weight is assigned to it in the action table, it
         will also occasionally substitute for a real action; this is one way
         to introduce variance into the action count.
      |}

  let available = Availability.always

  module Payload = Payload_impl.None

  let recommendations () = []

  let run (subject : Subject.Test.t) ~payload:() :
      Subject.Test.t State.Monad.t =
    State.Monad.return subject
end
