(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Language lookup and module building support

    The various top-level act commands need to invoke the
    language-independent bits of act (in the [Lib] module) using the
    appropriate language-dependent bits. This module works out which language
    is needed by looking at the 'emits' clause of a compiler spec, and hooks
    up the correct language-dependent modules. *)

open Core_kernel
open Act_common

val asm_runner_of_arch :
  Id.t -> (module Act_asm.Runner_intf.Basic) Or_error.t
(** [asm_runner_of_arch arch] generates an assembly job runner from an
    architecture ID [arch]. *)

val asm_runner_of_target :
     Act_machine.Qualified.Compiler.t Act_machine.Target.t
  -> (module Act_asm.Runner_intf.Basic) Or_error.t
(** [asm_runner_of_target target] gets the runner dependency module
    associated with a target (either a compiler spec or emits clause). *)

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
