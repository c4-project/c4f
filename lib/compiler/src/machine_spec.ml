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

module M = struct
  type t =
    { enabled: bool [@default true] [@sexp_drop_default.equal]
    ; via: Via.t [@default Via.local] [@sexp_drop_default.equal]
    ; compilers: Spec.Set.t [@default Act_common.Spec.Set.empty]
    ; sims: Act_sim.Spec.Set.t [@default Act_common.Spec.Set.empty] }
  [@@deriving fields, equal, make]

  (* We use a different name for the getter than the one [@@deriving fields]
     infers. *)
  let is_enabled = enabled

  let sim (spec : t) ~(id : Ac.Id.t) : Act_sim.Spec.With_id.t Or_error.t =
    Act_sim.Spec.Set.get (sims spec) id

  let pp_enabled (f : Base.Formatter.t) : bool -> unit = function
    | true ->
        ()
    | false ->
        Fmt.unit "@ (DISABLED)" f ()

  let pp =
    Fmt.(
      hbox
        (using
           (fun {via; enabled; _} -> (via, enabled))
           (append Via.pp pp_enabled)))

  let pp_summary = pp (* for now *)

  let remoteness x = Via.remoteness (via x)

  let runner x = Via.to_runner (via x)
end

include M

module Forward_basic_spec
    (I : Act_utils.Inherit.S)
    (B : Machine_types.S_spec with type t := I.c) :
  Machine_types.S_spec with type t := I.t and type via := B.via = struct
  module H = Act_utils.Inherit.Helpers (I)

  let compilers = H.forward B.compilers

  (* TODO(@MattWindsor91): this is a bit suspect *)
  let pp f = H.forward (B.pp f)

  let runner = H.forward B.runner

  let sim = H.forward B.sim

  let sims = H.forward B.sims

  let via = H.forward B.via

  let remoteness = H.forward B.remoteness
end

module With_id = struct
  include Ac.Spec.With_id (M)

  include Forward_basic_spec (struct
              type nonrec t = t

              type c = M.t

              let component = spec
            end)
            (struct
              type via = Via.t

              include M
            end)

  let pp f t = Fmt.pf f "@[%a@ (@,%a@,)@]" Ac.Id.pp (id t) M.pp (spec t)
end

module MS = Ac.Spec.Make (struct
  include M

  let type_name = "machine"

  module With_id = With_id
end)

include (
  MS : module type of MS with type t := M.t and module With_id := With_id )

module On_compiler_set :
  Travesty.Traversable.S0 with type t = M.t and type Elt.t = Spec.Set.t =
Travesty.Traversable.Make0 (struct
  type t = M.t

  module Elt = Spec.Set

  module On_monad (M : Monad.S) = struct
    let map_m (spec : t) ~(f : Spec.Set.t -> Spec.Set.t M.t) : t M.t =
      M.Let_syntax.(
        let compilers = compilers spec in
        let%map compilers' = f compilers in
        {spec with compilers= compilers'})
  end
end)

module On_specs = Travesty.Traversable.Fix_elt (Ac.Spec.Set.On_specs) (Spec)

module On_compilers :
  Travesty.Traversable.S0 with type t = M.t and type Elt.t = Spec.t =
  Travesty.Traversable.Chain0 (On_compiler_set) (On_specs)

module Qualified_compiler = struct
  type t = {c_spec: Spec.With_id.t; m_spec: With_id.t}
  [@@deriving make, fields, equal]

  module H = Act_utils.Inherit.Helpers (struct
    type nonrec t = t

    type c = Spec.With_id.t

    let component = c_spec
  end)

  let style = H.forward Spec.With_id.style

  let emits = H.forward Spec.With_id.emits

  let cmd = H.forward Spec.With_id.cmd

  let argv = H.forward Spec.With_id.argv
end

module Qualified_sim = struct
  type t = {s_spec: Act_sim.Spec.With_id.t; m_spec: With_id.t}
  [@@deriving make, fields, equal]

  (* TODO(@MattWindsor91): properly chain here? *)
end
