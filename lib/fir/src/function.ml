(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Ac = C4f_common
  module Named = Ac.C_named
end

module Access = struct
  type 'meta t =
    { parameters: Type.t Named.Alist.t
    ; body_decls: Initialiser.t Named.Alist.t
    ; body_stms: 'meta Statement.t list }
  [@@deriving sexp, accessors, make, equal]
end

include Access

let parameters x = Accessor.get Access.parameters x

let body_decls x = Accessor.get Access.body_decls x

let body_stms x = Accessor.get Access.body_stms x

let with_body_stms (type m1 m2) (func : m1 t) (new_stms : m2 Statement.t list)
    : m2 t =
  {func with body_stms= new_stms}

module On (M : Applicative.S) = struct
  let map_m (type m1 m2) (func : m1 t)
      ~(parameters :
            (Ac.C_id.t, Type.t) List.Assoc.t
         -> (Ac.C_id.t, Type.t) List.Assoc.t M.t )
      ~(body_decls :
            (Ac.C_id.t, Initialiser.t) List.Assoc.t
         -> (Ac.C_id.t, Initialiser.t) List.Assoc.t M.t )
      ~(body_stms : m1 Statement.t list -> m2 Statement.t list M.t) :
      m2 t M.t =
    M.map3 (parameters func.parameters) (body_decls func.body_decls)
      (body_stms func.body_stms) ~f:(fun parameters body_decls body_stms ->
        {parameters; body_decls; body_stms} )
end

module Mid = On (Travesty.Monad_exts.App (Monad.Ident))

let map (type m1) (func : m1 t) = Mid.map_m func

module With_meta (Meta : Equal.S) = struct
  type nonrec t = Meta.t t

  let equal : t -> t -> bool = equal Meta.equal

  module On_decls :
    Travesty.Traversable_types.S0
      with type t = t
       and type Elt.t = Initialiser.t Named.t =
  Travesty.Traversable.Make0 (struct
    type nonrec t = t

    module Elt = struct
      type t = Initialiser.t Named.t [@@deriving equal]
    end

    module On (M : Applicative.S) = struct
      module B = On (M)
      module Al = Named.Alist.As_named (Initialiser)
      module L = Al.On (M)

      let map_m (func : t)
          ~(f : Initialiser.t Named.t -> Initialiser.t Named.t M.t) =
        B.map_m func ~parameters:M.return ~body_decls:(L.map_m ~f)
          ~body_stms:M.return
    end
  end)
end
