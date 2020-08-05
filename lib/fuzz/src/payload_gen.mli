(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** The payload generator monad. *)

open Base

(** {1 Context}

    A payload generation context is similar to an {!Availability.Context.t},
    but contains a random number generator and action ID. *)
module Context : sig
  (** Opaque type of payload generation contexts. *)
  type t

  val make :
       action_id:Act_common.Id.t
    -> subject:Subject.Test.t
    -> param_map:Param_map.t
    -> state:State.t
    -> random:Splittable_random.State.t
    -> t
  (** [make ~action_id ~subject ~param_map ~state ~random] constructs a
      payload generation context from the given components. *)

  val action_id : t -> Act_common.Id.t

  (* [action_id ctx] gets the action ID inside [ctx]. *)

  val subject : t -> Subject.Test.t

  (* [subject ctx] gets the instantaneous subject inside [ctx]. *)

  val param_map : t -> Param_map.t

  (* [param_map ctx] gets the parameter map inside [ctx]. *)

  val state : t -> State.t

  (* [state ctx] gets the instantaneous fuzzer state inside [ctx]. *)

  val random : t -> Splittable_random.State.t

  (* [random ctx] gets the random number generator inside [ctx]. *)

  val to_availability : t -> Availability.Context.t
  (** [to_availability ctx] lowers [ctx] to an availability context. *)
end

(** {1 The generator monad}

    Think of the generator monad as a sort of reader monad over {!Context.t}. *)

(** Type of generators over ['a]. *)
type 'a t

(** The generator monad is, indeed, a monad. *)
include Monad.S with type 'a t := 'a t

(** {2 Let syntax}

    This will move to Travesty's monad extensions eventually, I imagine. *)

val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
(** Syntax for mapping. *)

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
(** Syntax for binding. *)

(** {2 Constructing generators}

    One can, of course, use [return] to lift a raw value to a generator. *)

val lift : (Context.t -> 'a) -> 'a t
(** [peek f] projects a value from the context using [f]. *)

val path : Path_kind.t -> filter:Path_filter.t -> Path.Test.t t
(** [path kind ~filter] generates a path of kind [kind] and respecting filter
    [filter]. *)

(** {3 Common queries} *)

val vars : Var.Map.t t
(** [vars] gets the current variable map at time of generation. *)

val flag : Act_common.Id.t -> bool t
(** [flag id] evaluates the (potentially stochastic) flag with ID [id]. *)

(** {3 Lifting generators} *)

val lift_opt_gen : 'a Opt_gen.t -> 'a t
(** [lift_opt_gen g] lifts the fallible generator [g] to the generator monad. *)

val lift_quickcheck : 'a Base_quickcheck.Generator.t -> 'a t
(** [lift_quickcheck g] lifts the quickcheck generator [g] to the generator
    monad. *)

(** {2 Running generators} *)

val run : 'a t -> ctx:Context.t -> 'a Or_error.t
(** [run g ~ctx] runs [g] in [ctx]. It can fail if, for instance, any of the
    fallible generators lifted into [g] fail. *)
