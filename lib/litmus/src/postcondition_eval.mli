(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Support for evaluating {{!Postcondition.t} postconditions}.

    To be able to check whether a state set obeys a Litmus postcondition, we
    need to be able to evaluate the postcondition in the context of the state
    set. This module provides support for doing so. *)

(** {1 Abstract data type of postcondition results} *)

module Result : sig
  (** Opaque type of postcondition evaluations. *)
  type 'a t [@@deriving yojson]

  val truth : 'a t -> bool
  (** [truth e] gets [e]'s truth value. *)

  val witnesses : 'a t -> 'a list
  (** [witnesses e] gets the witnesses of whichever postcondition was
      evaluated (the items for which the predicate was true). *)

  val counter_examples : 'a t -> 'a list
  (** [counter_examples e] gets the counter-examples of whichever
      postcondition was evaluated (the items for which the predicate was
      false). *)
end

(** {1 Evaluating parts of postconditions} *)

val eval_pred :
  'const Predicate.t -> elt:('const Predicate.Element.t -> bool) -> bool
(** [eval_pred pred ~elt] evaluates a Litmus postcondition predicate,
    applying [elt] to check the truth of each predicate element. *)

val eval_quantifier :
     Postcondition.Quantifier.t
  -> f:('a -> bool)
  -> subjects:'a list
  -> 'a Result.t
(** [on_list q ~f ~subject] returns the result of evaluating [f] over the
    subjects in [subjects], where the truth value is determined by the
    quantifier [q] (for example, it will behave as [List.exists] if [q] is
    [Exists], and [List.for_all] if [q] is [For_all]). *)

val eval :
     'const Postcondition.t
  -> elt:('a -> 'const Predicate.Element.t -> bool)
  -> subjects:'a list
  -> 'a Result.t
(** [eval post ~elt ~subjects] evaluates a Litmus postcondition, against a
    list of subjects [subjects], applying [elt] to check the truth of each
    predicate element against a particular element. It returns the result as
    an {!Evaluation.t}. *)
