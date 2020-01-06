(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** A specification of a fuzzer configurable parameter (or flag). *)

open Base

(** {1 The parameter spec record} *)

type 'a t
(** Opaque type of parameter specs, parametrised by the default value. *)

(** {2 Constructors} *)

val make : description:string -> default:'a -> 'a t
(** [make ~description ~default] makes a parameter spec with description
    [description] and default value [default]. *)

(** {2 Accessors} *)

val description : _ t -> string
(** [description spec] gets the human-readable description of [spec]. This
    description is not necessarily formatted for output. *)

val default : 'a t -> 'a
(** [default spec] gets the default value of [spec]. *)

(** {1 Specialised types} *)

(** Integer parameter specs. *)
module Int : sig
  type nonrec t = int t
  (** Specialised type of flag specs. *)

  include Pretty_printer.S with type t := t
  (** Flag specs can be pretty-printed. *)
end

(** Boolean parameter ('flag') specs. *)
module Bool : sig
  type nonrec t = Flag.t t
  (** Specialised type of flag specs. *)

  include Pretty_printer.S with type t := t
  (** Flag specs can be pretty-printed. *)
end
