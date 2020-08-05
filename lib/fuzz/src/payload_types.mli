(** Module type of payload modules.

    Each action takes in a payload record that can be:

    - converted to and from S-expressions;
    - generated, in the context of a fuzzer state, subject, and RNG. *)
module type S = sig
  (** The type of the payload. *)
  type t [@@deriving sexp]

  val gen : t Payload_gen.t
  (** [gen] is a generator for this payload type. *)
end
