(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Tx = Travesty_base_exts

  type b_style_map =
    (Act_common.Id.t, (module Act_backend.Instance_types.Basic)) List.Assoc.t

  type c_style_map =
    ( Act_common.Id.t
    , (module Act_compiler.Instance_types.Basic) )
    List.Assoc.t
end

let probe_style (type m s) (instance : m) (style : Act_common.Id.t)
    ~(binary_names : m -> string list)
    ~(f : m -> Act_common.Id.t -> string -> s Or_error.t) :
    s Act_common.Spec.With_id.t list =
  (* TODO(@MattWindsor91): keep hold of errors? *)
  List.filter_map (binary_names instance) ~f:(fun cmd ->
      Option.Let_syntax.(
        let%map spec = Result.ok (f instance style cmd) in
        Act_common.Spec.With_id.make ~id:(Act_common.Id.of_string cmd) ~spec))

let probe_alist (type m s) (xs : (Act_common.Id.t, m) List.Assoc.t)
    ~(binary_names : m -> string list)
    ~(f : m -> Act_common.Id.t -> string -> s Or_error.t) :
    s Act_common.Spec.Set.t Or_error.t =
  xs
  |> List.concat_map ~f:(fun (style, instance) ->
         probe_style instance style ~binary_names ~f)
  |> Act_common.Spec.Set.of_list

module On_runner (R : Plumbing.Runner_types.S) = struct
  let probe_backend (instance : (module Act_backend.Instance_types.Basic))
      (style : Act_common.Id.t) (cmd : string) :
      Act_backend.Spec.t Or_error.t =
    Or_error.Let_syntax.(
      let%map () = Act_backend.Instance.probe (module R) instance cmd in
      (* TODO(@MattWindsor91): C model, asm models *)
      Act_backend.Spec.make ~cmd ~style ~argv:[] ())

  let probe_backends :
      b_style_map -> Act_backend.Spec.t Act_common.Spec.Set.t Or_error.t =
    probe_alist ~f:probe_backend
      ~binary_names:(fun (module I : Act_backend.Instance_types.Basic) ->
        I.binary_names)

  let probe_compiler (instance : (module Act_compiler.Instance_types.Basic))
      (style : Act_common.Id.t) (cmd : string) :
      Act_compiler.Spec.t Or_error.t =
    Or_error.Let_syntax.(
      let%map emits = Act_compiler.Instance.probe (module R) instance cmd in
      Act_compiler.Spec.make ~cmd ~enabled:true ~emits ~style ~argv:[] ())

  let probe_compilers :
      c_style_map -> Act_compiler.Spec.t Act_common.Spec.Set.t Or_error.t =
    probe_alist ~f:probe_compiler
      ~binary_names:(fun (module I : Act_compiler.Instance_types.Basic) ->
        I.binary_names)
end

let probe (via : Via.t) ~(compiler_styles : c_style_map)
    ~(backend_styles : b_style_map) : Spec.t Or_error.t =
  let (module R) = Via.to_runner via in
  let module P = On_runner (R) in
  Or_error.Let_syntax.(
    let%map backends = P.probe_backends backend_styles
    and compilers = P.probe_compilers compiler_styles in
    Spec.make ~enabled:true ~via ~compilers ~backends ())
