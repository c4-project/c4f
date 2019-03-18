(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

(** Act's top-level configuration. *)

open Base

include module type of Act_intf

(** [Raw] represents act configuration loaded directly from a spec
    file, without any compiler testing or expansion. *)
module Raw : sig
  include S with module CSpec := Compiler.Cfg_spec

  val create
    :  ?cpp:Cpp.t
    -> ?herd:Herd.t
    -> compilers:Compiler.Cfg_spec.Set.t
    -> machines:Machine.Spec.Set.t
    -> t
  ;;

  include Utils.Loadable.S with type t := t
end

include S with module CSpec := Compiler.Spec

type 't hook = ('t -> 't option Or_error.t)
(** ['t hook] is the type of testing hooks sent to [from_raw]. *)

val disabled_compilers : t -> (Id.t * Error.t option) list
(** [disabled_compilers c] reports all disabled compiler IDs in
    the given config, along with any reason why. *)

val disabled_machines : t -> (Id.t * Error.t option) list
(** [disabled_machines c] reports all disabled machines in
    the given config, along with any reason why. *)

val require_herd : t -> Herd.t Or_error.t
(** [require_herd c] behaves as [herd c], but raises a descriptive
    error if [c] has no Herd configuration. *)

(** [from_raw c ?chook ?mhook ?phook] takes a raw config [t] and
    processes it by:

    - applying the given testing hooks onto the compilers and machines, and
      disabling any that fail;
    - resolving machine references, and disabling any
      broken ones;
    - installing [phook] as the sanitiser pass selector.

    Testing hooks are optional (and default to passing the compiler
    or machine through unaltered), and should return
    [Ok (Some x)] when the element is enabled and passing;
    [Ok None] when the element is disabled; and
    [Error e] when the element is enabled and failing. *)
val from_raw
  :  ?chook:(Compiler.Spec.With_id.t hook)
  -> ?mhook:(Machine.Spec.With_id.t hook)
  -> ?phook:(default:Sanitiser_pass.Set.t -> Sanitiser_pass.Set.t)
  -> Raw.t
  -> t Or_error.t
;;
