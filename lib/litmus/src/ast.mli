(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Top-level AST for Litmus tests *)

open Base

(** Initialisation blocks. *)
module Init : sig
  type 'const elt = {id: Act_common.C_id.t; value: 'const} [@@deriving sexp]

  type 'const t = 'const elt list [@@deriving sexp]
end

(** Freeform stanzas in a Litmus test. *)
module Decl : sig
  type ('const, 'prog) t =
    | Program of 'prog
    | Init of 'const Init.t
    | Post of 'const Postcondition.t
    | Locations of Act_common.C_id.t list
  [@@deriving sexp]

  val as_program : (_, 'prog) t -> 'prog option
  (** [as_program decl] returns [Some p] if [decl] is a program [p], and
      [None] otherwise. *)

  val as_init : ('const, _) t -> 'const Init.t option
  (** [as_init decl] returns [Some i] if [decl] is an init-block [i], and
      [None] otherwise. *)

  val as_post : ('const, _) t -> 'const Postcondition.t option
  (** [as_post decl] returns [Some c] if [decl] is a postcondition [c], and
      [None] otherwise. *)

  val as_locations : (_, _) t -> Act_common.C_id.t list option
  (** [as_post decl] returns [Some ls] if [decl] is a location list [ls],
      and [None] otherwise. *)
end

(** {2 Top-level AST} *)

type ('const, 'prog) t =
  { language: Act_common.C_id.t
  ; name: string
  ; decls: ('const, 'prog) Decl.t list }
[@@deriving sexp]
(** The type of litmus ASTs. *)

(** [M] allows AST type creation through referring to an existing language
    module. *)
module M (B : sig
  module Constant : T

  module Program : T
end) : sig
  type nonrec t = (B.Constant.t, B.Program.t) t
end

(** {3 Extracting parts of the AST} *)

val get_programs : (_, 'prog) Decl.t list -> 'prog list

val get_init :
     ('const, _) Decl.t list
  -> (Act_common.C_id.t, 'const) List.Assoc.t Or_error.t

val get_post :
  ('const, _) Decl.t list -> 'const Postcondition.t option Or_error.t

val get_locations :
  (_, _) Decl.t list -> Act_common.C_id.t list option Or_error.t

val get_header :
  string -> ('const, _) Decl.t list -> 'const Header.t Or_error.t
(** [get_header name decls] scrapes [decls] for all of the various parts of
    a Litmus test that make up a header, then builds a {!Header} with them
    and the test name [name]. *)

(* TODO(@MattWindsor91): expose constructors *)
