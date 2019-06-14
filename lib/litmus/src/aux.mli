(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Records of auxiliary Litmus information.

    This module describes a container for the parts of a Litmus test that
    aren't the program itself: the locations stanza, initial value map, and
    postcondition. Since de-litmusifying C tests removes this information *)

open Base

(** Opaque type of auxiliary Litmus records. *)
type 'const t

(** {2 Constructors} *)

val make :
     ?locations:Act_common.C_id.t list
  -> ?init:(Act_common.C_id.t, 'const) List.Assoc.t
  -> ?postcondition:'const Ast_base.Postcondition.t
  -> unit
  -> 'const t
(** [make ?locations ?init ?postcondition ()] makes an auxiliary record with
    the given fields. *)

(** {2 Accessors} *)

val locations : _ t -> Act_common.C_id.t list option
(** [locations aux] gets the computed location stanza of [aux], if any. *)

val init : 'const t -> (Act_common.C_id.t, 'const) List.Assoc.t
(** [init aux] gets the computed init block of [aux]. *)

val postcondition : 'const t -> 'const Ast_base.Postcondition.t option
(** [postcondition aux] gets the postcondition given in [aux], if any. *)

(** {2 Serialisation} *)

module Json (Const : sig
  type t

  include Pretty_printer.S with type t := t

  include Plumbing.Loadable_types.Jsonable with type t := t

  val parse_post_string : string -> t Ast_base.Postcondition.t Or_error.t
end) : Plumbing.Loadable_types.Jsonable with type t = Const.t t
