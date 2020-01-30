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

let probe_style (type m s i) (module Info : Comparable.S with type t = i)
    (instance : m) (style : Act_common.Id.t)
    ~(binary_names : m -> string Sequence.t)
    ~(f : m -> Act_common.Id.t -> string -> (i * s) Or_error.t) :
    (i * s Act_common.Spec.With_id.t) Sequence.t =
  (* TODO(@MattWindsor91): keep hold of errors? *)
  Sequence.filter_map (binary_names instance) ~f:(fun cmd ->
      Option.Let_syntax.(
        let%map i, spec = Result.ok (f instance style cmd) in
        ( i
        , Act_common.Spec.With_id.make
            ~id:(Act_common.Id.of_string cmd)
            ~spec )))

(* [info_specs_to_set] converts a list of pairs of probe-info hash and spec
   to a set of specs, keeping only one instance of each hash (and thereby
   making sure that we don't spec the same compiler twice). *)
let info_specs_to_set (type s i) (module Info : Comparable.S with type t = i)
    (specs : (i * s Act_common.Spec.With_id.t) list) :
    s Act_common.Spec.Set.t Or_error.t =
  specs
  |> List.dedup_and_sort ~compare:(Comparable.lift ~f:fst Info.compare)
  |> List.map ~f:snd |> Act_common.Spec.Set.of_list

let probe_alist (type m s i) (module Info : Comparable.S with type t = i)
    (xs : (Act_common.Id.t, m) List.Assoc.t)
    ~(binary_names : m -> string Sequence.t)
    ~(f : m -> Act_common.Id.t -> string -> (i * s) Or_error.t) :
    s Act_common.Spec.Set.t Or_error.t =
  xs |> Sequence.of_list
  |> Sequence.concat_map ~f:(fun (style, instance) ->
         probe_style (module Info) instance style ~binary_names ~f)
  |> Sequence.to_list
  |> info_specs_to_set (module Info)

module On_runner (R : Plumbing.Runner_types.S) = struct
  let probe_backend ?(c_model : string option)
      (instance : (module Act_backend.Instance_types.Basic))
      (style : Act_common.Id.t) (cmd : string) :
      (string * Act_backend.Spec.t) Or_error.t =
    (* TODO(@MattWindsor91): get information from the backend itself *)
    Or_error.Let_syntax.(
      let%map () = Act_backend.Instance.probe (module R) instance cmd in
      (* TODO(@MattWindsor91): asm models *)
      (cmd, Act_backend.Spec.make ?c_model ~cmd ~style ~argv:[] ()))

  let probe_backends ?(c_model : string option) :
      b_style_map -> Act_backend.Spec.t Act_common.Spec.Set.t Or_error.t =
    probe_alist
      (module String)
      ~f:(probe_backend ?c_model)
      ~binary_names:(fun (module I : Act_backend.Instance_types.Basic) ->
        I.binary_names)

  let probe_compiler (instance : (module Act_compiler.Instance_types.Basic))
      (style : Act_common.Id.t) (cmd : string) :
      (Act_compiler.Probe_info.t * Act_compiler.Spec.t) Or_error.t =
    Or_error.Let_syntax.(
      let%map info = Act_compiler.Instance.probe (module R) instance cmd in
      let emits = info.emits in
      ( info
      , Act_compiler.Spec.make ~cmd ~enabled:true ~emits ~style ~argv:[] ()
      ))

  let probe_compilers :
      c_style_map -> Act_compiler.Spec.t Act_common.Spec.Set.t Or_error.t =
    probe_alist
      (module Act_compiler.Probe_info)
      ~f:probe_compiler
      ~binary_names:(fun (module I : Act_compiler.Instance_types.Basic) ->
        I.binary_names)
end

let probe ?(c_model : string option) (via : Via.t)
    ~(compiler_styles : c_style_map) ~(backend_styles : b_style_map) :
    Spec.t Or_error.t =
  let (module R) = Via.to_runner via in
  let module P = On_runner (R) in
  Or_error.Let_syntax.(
    let%map backends = P.probe_backends ?c_model backend_styles
    and compilers = P.probe_compilers compiler_styles in
    Spec.make ~enabled:true ~via ~compilers ~backends ())
