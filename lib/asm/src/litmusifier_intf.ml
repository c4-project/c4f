(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(** Signature of constructed litmusifier modules. *)
module type S = sig
  type config
  (** Type of config. *)

  type fmt
  (** Type of formatting directives. *)

  type program
  (** Type of incoming programs. *)

  module Litmus : Act_litmus.Test_types.S
  (** The AST module for the litmus test language we're targeting. *)

  module Redirect : Act_common.Redirect_map_intf.S
  (** The redirect map for this litmusifier's input symbols. *)

  val make :
       config:config
    -> redirects:Redirect.t
    -> threads:program list
    -> Litmus.t Or_error.t
  (** [make ~config ~redirects ~threads] litmusifies a list of
      programs [threads], with redirect map [redirects],
      and configuration [config]. *)

  val print_litmus : fmt -> Stdio.Out_channel.t -> Litmus.t -> unit
  (** [print_litmus fmt oc ast] is the litmus test printer matching the
      configuration [fmt]. *)

  module Filter : Runner_intf.S with type cfg = config
  (** [Filter] is the litmusifier packaged up as an assembly job runner, ie
      a filter accepting an assembly job and outputting a standard job
      output. *)
end
