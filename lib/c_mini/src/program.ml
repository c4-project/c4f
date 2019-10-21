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

type 'meta t =
  { globals: (Ac.C_id.t, Initialiser.t) List.Assoc.t
  ; functions: (Ac.C_id.t, 'meta Function.t) List.Assoc.t }
[@@deriving sexp, fields, make]

let with_functions (type m1 m2) (program : m1 t)
    (new_functions : (Ac.C_id.t, m2 Function.t) List.Assoc.t) : m2 t =
  {program with functions= new_functions}

module Base_map (M : Monad.S) = struct
  module F = Travesty.Traversable.Helpers (M)

  let bmap (type m1 m2) (program : m1 t)
      ~(globals :
            (Ac.C_id.t, Initialiser.t) List.Assoc.t
         -> (Ac.C_id.t, Initialiser.t) List.Assoc.t M.t)
      ~(functions :
            (Ac.C_id.t, m1 Function.t) List.Assoc.t
         -> (Ac.C_id.t, m2 Function.t) List.Assoc.t M.t) : m2 t M.t =
    M.Let_syntax.(
      let%map globals' = globals program.globals
      and functions' = functions program.functions in
      {globals= globals'; functions= functions'})
end

module With_meta (Meta : Equal.S) = struct
  type nonrec t = Meta.t t

  module Fun_meta = Function.With_meta (Meta)

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
      module B = Base_map (M)
      module L = Tx.List.On_monad (M)
      module G_alist = Named.Alist.As_named (Initialiser)
      module G = G_alist.On_monad (M)
      module F_alist = Named.Alist.As_named (Fun_meta)
      module F = F_alist.On_monad (M)
      module D = Fun_meta.On_decls.On_monad (M)
      module N = Named.On_monad (M)

      let map_m (program : t) ~(f : Elt.t -> Elt.t M.t) =
        B.bmap program ~globals:(G.map_m ~f)
          ~functions:(F.map_m ~f:(N.map_right_m ~f:(D.map_m ~f)))
    end
  end)
end
