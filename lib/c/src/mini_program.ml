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
module Function = Mini_function
module Initialiser = Mini_initialiser

type t =
  { globals: (Ac.C_id.t, Initialiser.t) List.Assoc.t
  ; functions: (Ac.C_id.t, Function.t) List.Assoc.t }
[@@deriving sexp, fields, make]

let with_functions (program : t)
    (new_functions : (Ac.C_id.t, Function.t) List.Assoc.t) : t =
  {program with functions= new_functions}

module Base_map (M : Monad.S) = struct
  module F = Travesty.Traversable.Helpers (M)

  let bmap (program : t)
      ~(globals :
            (Ac.C_id.t, Initialiser.t) List.Assoc.t
         -> (Ac.C_id.t, Initialiser.t) List.Assoc.t M.t)
      ~(functions :
            (Ac.C_id.t, Function.t) List.Assoc.t
         -> (Ac.C_id.t, Function.t) List.Assoc.t M.t) : t M.t =
    Fields.fold ~init:(M.return program) ~globals:(F.proc_field globals)
      ~functions:(F.proc_field functions)
end

module On_decls :
  Travesty.Traversable_types.S0
    with type t = t
     and type Elt.t = Initialiser.Named.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Initialiser.Named

  module On_monad (M : Monad.S) = struct
    module B = Base_map (M)
    module L = Tx.List.On_monad (M)
    module F = Function.On_decls.On_monad (M)

    let map_m (program : t) ~(f : Elt.t -> Elt.t M.t) =
      B.bmap program ~globals:(L.map_m ~f)
        ~functions:
          (L.map_m ~f:(fun (k, v) -> M.(F.map_m ~f v >>| fun v' -> (k, v'))))
  end
end)

let cvars (prog : t) : Set.M(Ac.C_id).t =
  prog |> On_decls.to_list |> List.map ~f:fst |> Ac.C_id.Set.of_list
