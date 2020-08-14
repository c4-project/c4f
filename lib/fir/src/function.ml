(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Ac = Act_common
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

let with_body_stms (type m1 m2) (func : m1 t)
    (new_stms : m2 Statement.t list) : m2 t =
  {func with body_stms= new_stms}

module On_monad (M : Monad.S) = struct
  let map_m (type m1 m2) (func : m1 t)
      ~(parameters :
            (Ac.C_id.t, Type.t) List.Assoc.t
         -> (Ac.C_id.t, Type.t) List.Assoc.t M.t)
      ~(body_decls :
            (Ac.C_id.t, Initialiser.t) List.Assoc.t
         -> (Ac.C_id.t, Initialiser.t) List.Assoc.t M.t)
      ~(body_stms : m1 Statement.t list -> m2 Statement.t list M.t) :
      m2 t M.t =
    M.Let_syntax.(
      let%map parameters' = parameters func.parameters
      and body_decls' = body_decls func.body_decls
      and body_stms' = body_stms func.body_stms in
      { parameters= parameters'
      ; body_decls= body_decls'
      ; body_stms= body_stms' })
end

module Mid = On_monad (Monad.Ident)

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
end
