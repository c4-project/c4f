(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Pb = Plumbing
module C_instance = Act_compiler.Instance
module C_instance_t = Act_compiler.Instance_types

module type Basic =
  Resolver_types.Basic with type spec := Act_compiler.Spec.With_id.t

module type S =
  Resolver_types.S
  with type 'a chain_input := 'a Act_compiler.Instance.Chain_input.t

module Sp = Qualified.Compiler

let from_resolver_and_spec
    (resolve :
      Act_compiler.Spec.With_id.t -> (module C_instance_t.Basic) Or_error.t)
    (spec : Sp.t) : (module C_instance_t.S) Or_error.t =
  let cspec = Sp.c_spec spec in
  Or_error.Let_syntax.(
    let%map (module B : C_instance_t.Basic) = resolve cspec in
    let (module Runner) = Spec.With_id.runner (Sp.m_spec spec) in
    ( module C_instance.Make (struct
      let cspec = cspec

      include B
      module Runner = Runner
    end)
    : C_instance_t.S ))

module Make (B : Basic) : S with type spec = Sp.t = struct
  type spec = Sp.t

  let from_spec : spec -> (module C_instance_t.S) Or_error.t =
    from_resolver_and_spec B.resolve

  let filter_from_spec (cspec : Sp.t) :
      (module Pb.Filter_types.S with type aux_i = unit
                                 and type aux_o = unit)
      Or_error.t =
    Or_error.Let_syntax.(
      let%map (module M) = from_spec cspec in
      (module C_instance.S_to_filter (M)
      : Pb.Filter_types.S
        with type aux_i = unit
         and type aux_o = unit ))

  let chained_filter_from_spec (type i o) (cspec : Sp.t)
      (module Onto : Pb.Filter_types.S
        with type aux_i = i
         and type aux_o = o) :
      (module Pb.Filter_types.S
         with type aux_i = i C_instance.Chain_input.t
          and type aux_o = o)
      Or_error.t =
    Or_error.Let_syntax.(
      let%map (module F) = filter_from_spec cspec in
      (module C_instance.Chain_with_compiler (F) (Onto)
      : Pb.Filter_types.S
        with type aux_i = i C_instance.Chain_input.t
         and type aux_o = o ))
end

module Make_on_target (B : Basic) : S with type spec = Target.t = struct
  type spec = Target.t

  let from_spec : spec -> _ Or_error.t = function
    | `Spec cspec ->
        from_resolver_and_spec B.resolve cspec
    | `Arch _ ->
        Or_error.return
          ( module C_instance.Fail (struct
            let error =
              Error.of_string
                "To run a compiler, you must supply a compiler ID."
          end)
          : C_instance_t.S )

  let filter_from_spec (tgt : Target.t) =
    Or_error.Let_syntax.(
      let%map (module M) = from_spec tgt in
      (module C_instance.S_to_filter (M)
      : Pb.Filter_types.S
        with type aux_i = unit
         and type aux_o = unit ))

  let chained_filter_from_spec (type i o) (tgt : Target.t)
      (module Onto : Pb.Filter_types.S
        with type aux_i = i
         and type aux_o = o) =
    let open Or_error.Let_syntax in
    let%map (module F) = filter_from_spec tgt in
    (module C_instance.Chain_with_compiler (F) (Onto)
    : Pb.Filter_types.S
      with type aux_i = i C_instance.Chain_input.t
       and type aux_o = o )
end
