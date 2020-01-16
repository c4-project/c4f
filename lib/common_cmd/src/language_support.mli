(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Language lookup and module building support

    The various top-level act commands need to invoke the
    language-independent bits of act using the appropriate language-dependent
    bits. This module works out which language is needed by looking at the
    'emits' clause of a compiler spec, and hooks up the correct
    language-dependent modules. *)

open Core_kernel

(* TODO(@MattWindsor91): find a way to avoid exposing this *)

val style_modules :
  (Act_common.Id.t, (module Act_compiler.Instance_types.Basic)) List.Assoc.t
(** [style_modules] is the raw set of mappings from compiler styles to
    compiler modules. *)

val resolve :
     Act_machine.Qualified.Compiler.t
  -> (module Act_compiler.Instance_types.S) Or_error.t
(** [resolve spec] resolves [spec] using this module's built-in compiler
    table to look up compiler styles. *)

(** [Lookup] permits look-up of compilers with presence testing supplied by
    using {!resolve} to resolve the compiler, and the compiler's [test]
    function. *)
module Lookup : sig
  include Act_machine.Lookup_types.S_compiler

  val lookup_in_cfg :
       Act_common.Id.t
    -> cfg:Act_config.Global.t
    -> Act_machine.Qualified.Compiler.t Or_error.t
  (** [lookup_in_cfg fqid ~cfg] looks up the fully qualified backend ID
      [fqid] in the specs, and using the defaults, given by [cfg]. *)
end

val lookup_and_resolve_in_cfg :
     Act_common.Id.t
  -> cfg:Act_config.Global.t
  -> (module Act_compiler.Instance_types.S) Or_error.t
(** [lookup_and_resolve_in_cfg fqid ~cfg] composes {!Lookup.lookup_in_cfg}
    and {!resolve}. *)
