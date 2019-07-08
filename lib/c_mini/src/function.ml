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

type t =
  { parameters: Type.t Named.Alist.t
  ; body_decls: Initialiser.t Named.Alist.t
  ; body_stms: Statement.t list }
[@@deriving sexp, fields, make, equal]

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
     and type Elt.t = Initialiser.t Named.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = struct
    type t = Initialiser.t Named.t [@@deriving equal]
  end

  module On_monad (M : Monad.S) = struct
    module B = On_monad (M)
    module Al = Named.Alist.As_named (Initialiser)
    module L = Al.On_monad (M)

    let map_m (func : t)
        ~(f : Initialiser.t Named.t -> Initialiser.t Named.t M.t) =
      B.map_m func ~parameters:M.return ~body_decls:(L.map_m ~f)
        ~body_stms:M.return
  end
end)

let cvars (func : t) : Ac.C_id.Set.t =
  func |> On_decls.to_list |> List.map ~f:Named.name |> Ac.C_id.Set.of_list
