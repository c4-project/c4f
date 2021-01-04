(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** A specification of a fuzzer configurable parameter (or flag). *)

open Base

(** {1 The parameter spec record} *)

(** Opaque type of parameter specs, parametrised by the default value. *)
type 'a t

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
  (** Specialised type of flag specs. *)
  type nonrec t = int t

  (** Flag specs can be pretty-printed. *)
  include Pretty_printer.S with type t := t
end

(** Boolean parameter ('flag') specs. *)
module Bool : sig
  (** Specialised type of flag specs. *)
  type nonrec t = Flag.t t

  (** Flag specs can be pretty-printed. *)
  include Pretty_printer.S with type t := t
end
