(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Id = Act_common.Litmus_id

(** {2 Signatures} *)

(** Signature containing just the parts of an act language needed for
    building Litmus file ASTs. *)
module type Basic = sig
  val name : string
  (** [name] is the Herd name of the language. *)

  (** Abstract type of constant syntax *)
  module Constant : sig
    type t [@@deriving compare, equal, sexp, quickcheck]
  end

  (** Abstract type of statements. *)
  module Statement : sig
    type t [@@deriving sexp]

    val make_uniform : t list list -> t list list
    (** [make_uniform listings] pads each listing in [listing] to the same
        length. *)
  end

  (** Abstract type of types. *)
  module Type : sig
    type t [@@deriving compare, equal, sexp]
  end

  (** Abstract type of programs. *)
  module Program : sig
    type t [@@deriving sexp]

    val name : t -> string option
    (** [name program] gets the declared name of [program], if it has one. *)

    val listing : t -> Statement.t list
    (** [listing program] gets [program]'s statement listing. *)

    val global_vars : t -> Type.t Map.M(Act_common.C_id).t option
    (** [global_vars program] gets the set of global variables referenced by
        [program], if this makes sense for this particular litmus language. *)
  end
end

(** {2 Test modules} *)

(** Signature of objects that contain frozen, 'valid' Litmus tests.

    A 'valid' litmus test is one that has a well-formed name, the correct
    language, exactly one init block, at most one postcondition block, and a
    set of appropriately named programs. *)
module type S = sig
  (** Type of unvalidated litmus tests. *)
  type raw

  (** The subset of the Litmus language used to define this module. *)
  module Lang : Basic

  (** The abstract type of a validated litmus AST. *)
  type t [@@deriving sexp_of]

  (** For pretty-printing, use one of the functors in [Pp]. *)

  (** {3 Constructing validated tests} *)

  val make :
       header:Lang.Constant.t Header.t
    -> threads:Lang.Program.t list
    -> t Or_error.t
  (** [make ~header ~threads] directly constructs a validated test with the
      given header and thread programs. It may fail if the result fails
      validation. *)

  val validate : raw -> t Or_error.t
  (** [validate lit] tries to validate an existing Litmus test. It may fail
      if the input isn't a valid Litmus program. *)

  val of_ast : Ast.M(Lang).t -> t Or_error.t
  (** [of_ast ast] tries to construct a validated Litmus test directly from
      an abstract syntax tree [ast]. *)

  (** {3 Accessing properties of validated tests} *)

  val raw : t -> raw
  (** [raw test] gets the raw, unvalidated form of [test]. *)

  val threads : t -> Lang.Program.t list
  (** [programs test] gets the program listings in [test], in left-right or
      top-bottom order. *)

  val header : t -> Lang.Constant.t Header.t
  (** [header test] gets the header for [test]. *)

  (** {4 Shortcuts for accessing individual header fields} *)

  val name : t -> string
  (** [name test] gets the name of [test]. *)

  val init : t -> (Act_common.C_id.t, Lang.Constant.t) List.Assoc.t
  (** [init test] gets the initialiser in [test]. *)

  val locations : t -> Act_common.C_id.t list option
  (** [locations test] gets the locations stanza for [test], if it exists. *)

  val postcondition : t -> Lang.Constant.t Postcondition.t option
  (** [postcondition test] gets the postcondition of [test], if one exists. *)

  (** {3 Modifying validated tests} *)

  val try_map_raw : t -> f:(raw -> raw Or_error.t) -> t Or_error.t
  (** [try_map_raw test ~f] maps [f] over the unvalidated form of [test],
      then, if successful, tries to re-validate it. *)

  val try_map_header :
       t
    -> f:(Lang.Constant.t Header.t -> Lang.Constant.t Header.t Or_error.t)
    -> t Or_error.t
  (** [try_map_header test ~f] maps [f] over the header of [test], then, if
      successful, tries to re-validate the resulting test. *)
end

(** {2 Conversion} *)

(** Signature of inputs to the [Convert] functor. *)
module type Basic_convert = sig
  (** The Litmus language from which we're converting. *)
  module From : Basic

  (** The Litmus language to which we're converting. *)
  module To : Basic

  val constant : From.Constant.t -> To.Constant.t Or_error.t
  (** [constant k] tries to convert [k] to the new language. *)

  val program : From.Program.t -> To.Program.t Or_error.t
  (** [constant k] tries to convert [k] to the new language. *)
end
