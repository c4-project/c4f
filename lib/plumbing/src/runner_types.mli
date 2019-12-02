(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Module types for {!Runner}. *)

open Base
open Stdio

(** Signature that runner modules must implement to use {{!Run.Make} Make}. *)
module type Basic = sig
  val pre : Fpath.t Copy_spec.Pair.t -> string Copy_spec.Pair.t Or_error.t
  (** [pre p] does any preparation needed by the runner before it can run the
      main job. It takes a manifest [p.input] of the files that need
      transferring to the runner target, a manifest [p.output] of the files
      that are expected to need copying from the runner target, and returns a
      pair of altered input and output manifests specifying where the files
      are relative to the target. *)

  val post : Fpath.t Copy_spec.t -> unit Or_error.t
  (** [post in_manifest] does any cleanup required by the runner after it
      runs the main job, taking a manifest of items that it should transfer
      _from_ the runner target. *)

  val run_batch :
    ?oc:Out_channel.t -> string list list -> prog:string -> unit Or_error.t
  (** [run_batch ?oc argss ~prog] runs [prog] on each argument vector in
      [argss], waiting for each invocation to complete in sequence, and
      collecting any errors as [Error.t]s.

      If [oc] is given, each process's standard output will be copied
      line-by-line to it as it ends. *)
end

type ('m, 'a) argv_fun = input:'m -> output:'m -> 'a Or_error.t
(** Type of functions that return argument vectors. *)

(** Outward-facing interface of process runners. *)
module type S = sig
  include Basic

  val run_batch_with_copy :
       ?oc:Out_channel.t
    -> Fpath.t Copy_spec.Pair.t
    -> (string Copy_spec.t, string list list) argv_fun
    -> prog:string
    -> unit Or_error.t
  (** [run_batch_with_copy] behaves as [run_batch], but also takes copy specs
      of files that should be transferred to and from the target, and
      parametrises the argument vectors on the output copy spec. *)

  val run :
    ?oc:Out_channel.t -> string list -> prog:string -> unit Or_error.t
  (** [run ?oc args ~prog] runs the given program, waits for it to complete,
      and translates any errors to [Error.t].

      If [oc] is given, the process's standard output will be copied
      line-by-line to it at the end. *)

  val run_with_copy :
       ?oc:Out_channel.t
    -> Fpath.t Copy_spec.Pair.t
    -> (string Copy_spec.t, string list) argv_fun
    -> prog:string
    -> unit Or_error.t
  (** [run_with_copy] behaves as [run], but also takes copy specs of files
      that should be transferred to and from the target, and parametrises the
      argument vector on the output copy spec. *)
end
