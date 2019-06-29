(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ac = Act_common
module Tx = Travesty_base_exts
module Initialiser = Mini_initialiser
module Statement = Mini_statement
module Type = Mini_type

type t =
  { parameters: (Ac.C_id.t, Type.t) List.Assoc.t
  ; body_decls: (Ac.C_id.t, Initialiser.t) List.Assoc.t
  ; body_stms: Statement.t list }
[@@deriving sexp, fields, make]

let with_body_stms (func : t) (new_stms : Statement.t list) : t =
  {func with body_stms= new_stms}

module On_monad (M : Monad.S) = struct
  module F = Travesty.Traversable.Helpers (M)

  let map_m (func : t)
      ~(parameters :
            (Ac.C_id.t, Type.t) List.Assoc.t
         -> (Ac.C_id.t, Type.t) List.Assoc.t M.t)
      ~(body_decls :
            (Ac.C_id.t, Initialiser.t) List.Assoc.t
         -> (Ac.C_id.t, Initialiser.t) List.Assoc.t M.t)
      ~(body_stms : Statement.t list -> Statement.t list M.t) : t M.t =
    Fields.fold ~init:(M.return func) ~parameters:(F.proc_field parameters)
      ~body_decls:(F.proc_field body_decls)
      ~body_stms:(F.proc_field body_stms)
end

let map =
  let module M = On_monad (Monad.Ident) in
  M.map_m

module On_decls :
  Travesty.Traversable_types.S0
    with type t = t
     and type Elt.t = Initialiser.Named.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Initialiser.Named

  module On_monad (M : Monad.S) = struct
    module B = On_monad (M)
    module L = Tx.List.On_monad (M)

    let map_m (func : t)
        ~(f : Initialiser.Named.t -> Initialiser.Named.t M.t) =
      B.map_m func ~parameters:M.return ~body_decls:(L.map_m ~f)
        ~body_stms:M.return
  end
end)

let cvars (func : t) : Ac.C_id.Set.t =
  func |> On_decls.to_list |> List.map ~f:fst |> Ac.C_id.Set.of_list
