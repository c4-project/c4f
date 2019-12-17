(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Delitmus configuration.

    A delitmus configuration describes how to construct a {!Runner}. It
    specifies things like:

    - whether variables in a Litmus test map to global variables, parameters,
      and so on;
    - whether the delitmusified thread-body functions should have a suffix
      appended to their name;
    - various other alterations that the delitmusifier should make to the
      test.

    In practice, the user will specify this configuration through
    command-line flags. *)

open Base

(** {1 Styles of delitmusification} *)

(** Enumeration of different styles of delitmusification. *)
module Style : sig
  type t = Vars_as_globals | Vars_as_parameters
  [@@deriving compare, equal, enumerate]

  include Act_utils.Enum_types.Extension_table with type t := t
end

(** {1 Config records} *)

type t
(** Opaque type of config records. *)

(** {2 Constructors} *)

val make :
  ?impl_suffix:string -> ?qualify_locals:bool -> style:Style.t -> unit -> t
(** [make ?impl_suffix ?qualify_locals ~style ()] constructs a delitmus
    configuration using the style [style].

    - If [impl_suffix] is given, each function thread's name will receive it
      as a suffix. This is useful for creating 'stub' Litmus tests that call
      into delitmusified implementations.
    - If [qualify_locals] is explicitly set to [false], local variables will
      not have their names prefixed by their thread IDs. This can be useful
      This may also be useful for creating stub tests. *)

(** {2 Accessors} *)

val impl_suffix : t -> string option
(** [impl_suffix config] gets the configured thread suffix of [config]. *)

val qualify_locals : t -> bool
(** [qualify_locals config] gets whether [config] will qualify local
    variables. *)

val style : t -> Style.t
(** [style config] gets the overall delitmus style of [config]. *)

(** {2 Using a config record} *)

val to_runner : t -> (module Runner.S)
(** [to_runner config] produces a {!Runner.S} that runs a delitmusification
    campaign according to [config]. *)
