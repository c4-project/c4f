(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Unvalidated and validated Litmus tests.

    Not to be confused with the {{!Ast} Ast}, which contains a more
    low-level syntactic form of Litmus tests. *)

open Base

(** {2 Unvalidated tests} *)

module Raw : sig
  type ('const, 'prog) t [@@deriving sexp]
  (** Opaque type of unvalidated tests. *)

  (** Convenience functor for building raw test types based on a language
      module. *)
  module M (L : sig
    module Constant : T

    module Program : T
  end) : sig
    type nonrec t = (L.Constant.t, L.Program.t) t
  end

  (** {3 Constructing raw tests} *)

  val make :
       name:string
    -> header:'const Header.t
    -> threads:'prog list
    -> ('const, 'prog) t
  (** [make ~name ~header ~threads] directly constructs an unvalidated
      Litmus test with the given fields. *)

  (** {3 Accessing parts of raw tests} *)

  val name : (_, _) t -> string
  (** [name test] gets the name of [test]. *)

  val header : ('const, _) t -> 'const Header.t
  (** [header test] gets [test]'s header directly. *)

  val threads : (_, 'prog) t -> 'prog list
  (** [threads test] gets the program listings in [test], in left-right or
      top-bottom order. *)

  (** {3 Manipulating raw tests}

      Raw tests can be modified in various ways. These modifications needn't
      result in well-formed litmus tests, but may cause validation against a
      language to fail if not. *)

  val map_name :
    ('const, 'prog) t -> f:(string -> string) -> ('const, 'prog) t
  (** [map_name test ~f] maps [f] over the name of [test]. *)

  (** {4 Manipulating the header} *)

  val try_map_header :
       ('c1, 'prog) t
    -> f:('c1 Header.t -> 'c2 Header.t Or_error.t)
    -> ('c2, 'prog) t Or_error.t
  (** [try_map_header test ~f] tries to map [f] over the test header in
      [test]. *)

  (** {4 Manipulating threads} *)

  val try_map_threads :
       ('const, 'p1) t
    -> f:('p1 -> 'p2 Or_error.t)
    -> ('const, 'p2) t Or_error.t
  (** [try_map_threads test ~f] tries to map [f] over each thread program in
      [test]. *)

  val add_thread :
       ('const, 'prog) t
    -> thread:'prog
    -> index:int
    -> ('const, 'prog) t Or_error.t
  (** [add_thread test ~index ~thread] inserts [thread] into the thread slot
      in test [test] described by [index], adjusting any litmus IDs
      accordingly. It fails if [index] is out of bounds. *)

  val add_thread_at_end :
    ('const, 'prog) t -> thread:'prog -> ('const, 'prog) t
  (** [add_thread_at_end test ~thread] inserts [thread] onto the end of the
      thread set in test [test]. It cannot fail. *)

  val try_map_thread :
       ('const, 'prog) t
    -> index:int
    -> f:('prog -> 'prog Or_error.t)
    -> ('const, 'prog) t Or_error.t
  (** [try_map_thread test ~index ~f] maps the the thread in test [test]
      described by [index] through [f]. accordingly. It fails if [index] is
      out of bounds or [f] fails. *)

  val remove_thread :
    ('const, 'prog) t -> index:int -> ('const, 'prog) t Or_error.t
  (** [remove_thread test ~index] removes the thread in test [test]
      described by [index], adjusting any litmus IDs accordingly. It fails
      if [index] is out of bounds. *)
end

(** {2 Validated tests} *)

(** [Make] is a functor that, given a language described by [Basic],
    produces a module type for validated Litmus tests, well as operations
    for pretty-printing them. *)
module Make (Lang : Test_types.Basic) :
  Test_types.S with module Lang = Lang and type raw = Raw.M(Lang).t
