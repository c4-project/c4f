(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Module types and type synonyms for {!Runner}. *)

open Base

(** {1 Function type synonyms} *)

type prog_fun =
  string -> input:Copy_projection.t Copy_spec.t -> string Or_error.t
(** Type of functions that transform programs according to an input copy
    specification. This lets runner users handle the possibility of the
    program itself being copied. *)

type 'a argv_fun = Copy_projection.t Copy_spec.Pair.t -> 'a Or_error.t
(** Type of functions that return argument vectors given manifests of the
    input and output being copied by the runner. *)

(** {1 Module types} *)

(** Signature that runner modules must implement to use {{!Run.Make} Make}. *)
module type Basic = sig
  val pre :
    Fpath.t Copy_spec.Pair.t -> Copy_projection.t Copy_spec.Pair.t Or_error.t
  (** [pre p] does any preparation needed by the runner before it can run the
      main job. It takes a manifest [p.input] of the files that need
      transferring to the runner target, a manifest [p.output] of the files
      that are expected to need copying from the runner target, and returns a
      pair of altered input and output manifests specifying where the files
      are relative to the target. *)

  val post : output:Copy_projection.t Copy_spec.t -> unit Or_error.t
  (** [post ~output] does any cleanup required by the runner after it runs
      the main job, taking a manifest [output] of items that it should
      transfer _from_ the runner target. *)

  val run_batch :
       ?out:Runner_output.t
    -> string list list
    -> prog:string
    -> unit Or_error.t
  (** [run_batch ?out argss ~prog] runs [prog] on each argument vector in
      [argss], waiting for each invocation to complete in sequence, and
      collecting any errors as [Error.t]s.

      If [out] is given, each process's standard output will be copied to it
      at some stage (not necessarily immediately). *)
end

(** Outward-facing interface of process runners. *)
module type S = sig
  include Basic

  val run_batch_with_copy :
       ?out:Runner_output.t
    -> ?prog_f:prog_fun
    -> Fpath.t Copy_spec.Pair.t
    -> argvs_f:string list list argv_fun
    -> prog:string
    -> unit Or_error.t
  (** [run_batch_with_copy ?out ?prog_f specs ~argvs_f ~prog] behaves like
      {!run_batch}, but also takes a pair [specs] of copy specs of files that
      should be transferred to and from the target, parametrises the argument
      vectors on the computed local and remote input and output copy specs
      through [argv_f], and optionally takes a callback [prog_f] to modify
      [prog] itself through the copy specs. *)

  val run :
    ?out:Runner_output.t -> string list -> prog:string -> unit Or_error.t
  (** [run ?out args ~prog] runs the given program, waits for it to complete,
      and translates any errors to [Error.t].

      If [out] is given, each process's standard output will be copied to it
      at some stage (not necessarily immediately). *)

  val run_with_copy :
       ?out:Runner_output.t
    -> ?prog_f:prog_fun
    -> Fpath.t Copy_spec.Pair.t
    -> argv_f:string list argv_fun
    -> prog:string
    -> unit Or_error.t
  (** [run_with_copy ?out ?prog_f specs ~argv_f ~prog] behaves like
      [run_batch_with_copy], but over a single invocation and therefore a
      single argument vector. *)
end
