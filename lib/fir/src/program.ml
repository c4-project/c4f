(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Ac = C4f_common
  module Tx = Travesty_base_exts
  module Named = Ac.C_named
end

type 'meta t =
  { globals: (Ac.C_id.t, Initialiser.t) List.Assoc.t
  ; functions: (Ac.C_id.t, 'meta Function.t) List.Assoc.t }
[@@deriving sexp, fields, make]

let with_functions (type m1 m2) (program : m1 t)
    (new_functions : (Ac.C_id.t, m2 Function.t) List.Assoc.t) : m2 t =
  {program with functions= new_functions}

module Base_map (M : Applicative.S) = struct
  let bmap (type m1 m2) (program : m1 t)
      ~(globals :
            (Ac.C_id.t, Initialiser.t) List.Assoc.t
         -> (Ac.C_id.t, Initialiser.t) List.Assoc.t M.t)
      ~(functions :
            (Ac.C_id.t, m1 Function.t) List.Assoc.t
         -> (Ac.C_id.t, m2 Function.t) List.Assoc.t M.t) : m2 t M.t =
    M.map2
      ~f:(fun globals functions -> {globals; functions})
      (globals program.globals)
      (functions program.functions)
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

    module On (M : Applicative.S) = struct
      module B = Base_map (M)
      module L = Tx.List.On (M)
      module G_alist = Named.Alist.As_named (Initialiser)
      module G = G_alist.On (M)
      module F_alist = Named.Alist.As_named (Fun_meta)
      module F = F_alist.On (M)
      module D = Fun_meta.On_decls.On (M)
      module N = Named.On (M)

      let map_m (program : t) ~(f : Elt.t -> Elt.t M.t) =
        B.bmap program ~globals:(G.map_m ~f)
          ~functions:(F.map_m ~f:(N.map_right_m ~f:(D.map_m ~f)))
    end
  end)
end
