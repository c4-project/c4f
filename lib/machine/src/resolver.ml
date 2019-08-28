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
module C_filter = Act_compiler.Filter

module type Basic =
  Resolver_types.Basic with type spec := Act_compiler.Spec.With_id.t

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
    end) : C_instance_t.S ))

module Make (B : Basic) : Resolver_types.S with type spec = Sp.t = struct
  type spec = Sp.t

  let from_spec : spec -> (module C_instance_t.S) Or_error.t =
    from_resolver_and_spec B.resolve

  let filter_from_spec (cspec : Sp.t) : (module C_filter.S) Or_error.t =
    Or_error.Let_syntax.(
      let%map (module M) = from_spec cspec in
      (module C_filter.Make (M) : C_filter.S))
end

module Make_on_target (B : Basic) :
  Resolver_types.S with type spec = Qualified.Compiler.t Target.t = struct
  type spec = Qualified.Compiler.t Target.t

  let from_spec : spec -> _ Or_error.t = function
    | Cc cspec ->
        from_resolver_and_spec B.resolve cspec
    | Arch _ ->
        Or_error.return
          ( module C_instance.Fail (struct
            let error =
              Error.of_string
                "To run a compiler, you must supply a compiler ID."
          end) : C_instance_t.S )

  let filter_from_spec (tgt : Qualified.Compiler.t Target.t) : (module C_filter.S) Or_error.t =
    Or_error.Let_syntax.(
      let%map (module M) = from_spec tgt in
      (module C_filter.Make (M) : C_filter.S))
end
