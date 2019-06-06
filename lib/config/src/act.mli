(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** A record containing processed common configuration used for most ACT
    tools. This includes the {{!Global} global configuration} from act.conf,
    as well as pre-filtered machines and sanitiser passes. *)

(* TODO(@MattWindsor91): eventually, this should be split into assembly and
   C configuration. *)

open Base
open Act_common

include Global_types.S

(** ['t hook] is the type of testing hooks sent to [from_raw]. *)
type 't hook = 't -> 't option Or_error.t

(** {2 Constructors} *)

val make :
     ?disabled_compilers:(Id.t, Error.t option) List.Assoc.t
  -> ?disabled_machines:(Id.t, Error.t option) List.Assoc.t
  -> ?machines:Act_machine.Spec.Set.t
  -> global:Global.t
  -> sanitiser_passes:(   default:Set.M(Act_sanitiser.Pass_group).t
                       -> Set.M(Act_sanitiser.Pass_group).t)
  -> unit
  -> t

val of_global :
     ?chook:Act_machine.Qualified.Compiler.t hook
  -> ?mhook:Act_machine.Spec.With_id.t hook
  -> ?phook:(   default:Set.M(Act_sanitiser.Pass_group).t
             -> Set.M(Act_sanitiser.Pass_group).t)
  -> Global.t
  -> t Or_error.t
(** [of_global ?chook ?mhook ?phook global] takes a global config [t] and
    processes it by:

    - applying the given testing hooks onto the compilers and machines, and
      disabling any that fail;
    - resolving machine references, and disabling any broken ones;
    - installing [phook] as the sanitiser pass selector.

    Testing hooks are optional (and default to passing the compiler or
    machine through unaltered), and should return [Ok (Some x)] when the
    element is enabled and passing; [Ok None] when the element is disabled;
    and [Error e] when the element is enabled and failing. *)

(** {2 Projections} *)

val disabled_compilers : t -> (Id.t * Error.t option) list
(** [disabled_compilers c] reports all (fully qualified) disabled compiler
    IDs in the given config, along with any reason why. *)

val disabled_machines : t -> (Id.t * Error.t option) list
(** [disabled_machines c] reports all disabled machines in the given config,
    along with any reason why. *)

(* TODO(@MattWindsor91): this shouldn't be in here? *)
val sanitiser_passes :
     t
  -> default:Set.M(Act_sanitiser.Pass_group).t
  -> Set.M(Act_sanitiser.Pass_group).t

val all_compilers : t -> Act_machine.Qualified.Compiler.t list
(** [all_compilers c] returns a list of qualified specifications for all
    compilers, across all machines. *)

val all_sims : t -> Act_machine.Qualified.Sim.t list
(** [all_sims c] returns a list of qualified specifications for all
    simulators, across all machines. *)

(** {2 Resolving components across machines by fully qualified ID}

    When a user asks for a certain compiler/simulator/etc on the command
    line, ACT expects a single 'fully qualified' ID: the identifier of the
    machine, followed by the identifier of the component relative to the
    machine. These functions resolve those IDs, checking every machine in
    the config in turn. *)

val compiler : t -> fqid:Id.t -> Act_machine.Qualified.Compiler.t Or_error.t
(** [compiler c ~fqid] looks up a compiler with the fully qualified ID
    [fqid] (that is, the concatenation of the machine ID and the compiler
    ID) in [c]'s specification sets. *)

val sim : t -> fqid:Id.t -> Act_machine.Qualified.Sim.t Or_error.t
(** [compiler c ~fqid] looks up a simulator with the fully qualified ID
    [fqid] (that is, the concatenation of the machine ID and the compiler
    ID) in [c]'s specification sets. *)
