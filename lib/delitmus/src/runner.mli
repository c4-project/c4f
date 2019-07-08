(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** De-litmusification of memalloy-style C litmus tests *)

open Base

(** Enumeration of different styles of delitmusification. *)
module Style : sig
  type t = Vars_as_globals | Vars_as_parameters
  [@@deriving compare, equal, enumerate]

  include Act_utils.Enum.Extension_table with type t := t
end

val run :
  Act_c_mini.Litmus.Ast.Validated.t -> style:Style.t -> Output.t Or_error.t
(** [run litmus ~style] runs de-litmusification on [litmus] according to
    style [style]. *)
