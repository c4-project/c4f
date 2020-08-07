(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Stock payloads.

    See {!Payload_types.S} for the shape of an payload. *)

(** Dummy payload module for actions that take no payload. *)
module None : Payload_types.S with type t = unit

(** Adapts payload generators that don't depend on a generation context. *)
module Pure (Basic : sig
  type t [@@deriving sexp]

  val quickcheck_generator : t Base_quickcheck.Generator.t
end) : Payload_types.S with type t = Basic.t

(** {1 Insertion} *)

(** Scaffolding for producing statement insertion payloads. *)
module Insertion : sig
  (** Opaque type of insertion payloads. *)
  type 'a t [@@deriving sexp]

  val make : to_insert:'a -> where:Path.Flagged.t -> 'a t
  (** [make ~to_insert ~where] makes a payload that expresses the desire to
      insert [to_insert] using path [where]. *)

  val to_insert : 'a t -> 'a
  (** [to_insert x] gets the insertion candidate of [x]. *)

  val where : _ t -> Path.Flagged.t
  (** [where x] gets the flagged program path to which [x] is inserting. *)

  (** Constructs a payload for inserting a statement. *)
  module Make (To_insert : sig
    type t [@@deriving sexp]

    val path_filter : Availability.Context.t -> Path_filter.t
    (** [path_filter ctx] should generate the path filter needed for the
        action, given the availability context [ctx]. *)

    val gen : Path.Flagged.t -> t Payload_gen.t
    (** [gen where] generates the inner payload given the generated path
        [where]. *)
  end) : Payload_types.S with type t = To_insert.t t
end

(** {1 Surround} *)

(** Payload for actions that surround a statement span with a flow block or
    if statement that requires a conditional.

    The payload for any surround action contains two elements:

    - the expression to insert into the condition of the if statement; - the
      path to the statements to remove, pass through the if statement block
      generators, and replace with the statement. *)
module Cond_surround : sig
  (** Opaque type of payloads. *)
  type t

  (** {3 Constructors} *)

  val make : cond:Act_fir.Expression.t -> where:Path.Flagged.t -> t
  (** [make ~cond ~where] makes a payload given a specific condition
      expression [cond] and statement-list selecting path [where]. *)

  (** {3 Accessors} *)

  val cond : t -> Act_fir.Expression.t
  (** [cond payload] retrieves the generated condition inside [payload]. *)

  val where : t -> Path.Flagged.t
  (** [where payload] retrieves the generated path inside [payload]. *)

  val apply :
       ?filter:Path_filter.t
    -> t
    -> test:Subject.Test.t
    -> f:
         (   Act_fir.Expression.t
          -> Subject.Statement.t list
          -> Subject.Statement.t)
    -> Subject.Test.t State.Monad.t
  (** [apply ?filter payload ~test ~f] lifts a surrounding function [f] over
      a payload [payload], supplying the test [test] to transform, and
      lifting the computation into the fuzzer state monad. If [filter] is
      given, the path in [payload] will be checked for compliance with it. *)

  (** {3 Using as a payload module} *)

  (** [Make] builds a fully-functional payload module with the surround
      payload and a generator that uses the given conditional generator. *)
  module Make (Basic : sig
    val cond_gen :
      Act_fir.Env.t -> Act_fir.Expression.t Base_quickcheck.Generator.t
    (** [cond_gen env] should, given an environment [env] capturing the
        variables in scope at the point where the if statement is appearing,
        return a Quickcheck generator generating expressions over those
        variables. *)

    val path_filter : Availability.Context.t -> Path_filter.t
    (** [path_filter ctx] should generate the path filter needed for the
        action, given the availability context [ctx]. *)
  end) : Payload_types.S with type t = t
end
