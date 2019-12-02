(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Module types for {!Scp}. *)

open Base

(** Outward-facing interface of SCP modules. *)
module type S = sig
  val send :
       Ssh.t
    -> recurse:bool
    -> locals:Fpath.t list
    -> remote:string
    -> unit Or_error.t
  (** [send cfg ~recurse ~local ~remote] tries to copy the local paths
      [locals] to the remote host at path [remote] using SCP per [cfg].
      [recurse], if true, turns on the recursive copy flag. *)

  val receive :
       Ssh.t
    -> recurse:bool
    -> remotes:string list
    -> local:Fpath.t
    -> unit Or_error.t
  (** [receive cfg ~recurse ~remotes ~local] tries to copy the path [remotes]
      on the remote host to the local path [local] using SCP per [cfg].
      [recurse], if true, turns on the recursive copy flag. *)
end
