open Core
open Utils

(** [Id] is a module for compiler identifiers. *)
module Id : sig
  (** [t] is the type of compiler IDs. *)
  type t [@@deriving compare, hash, sexp]

  (** [to_string_list cid] returns a list of each element in [cid]'s
     ID. *)
  val to_string_list : t -> string list

  include Core.Identifiable.S_plain with type t := t
end

(** [Spec] is a module for compiler specifications. *)
module Spec : sig
  (** [ssh] describes how to invoke a compiler remotely
      from ssh. *)
  type ssh =
    { host     : string        (* The host to run on. *)
    ; user     : string option (* The user to run as. *)
    ; copy_dir : string        (* The remote directory to use for temporary results. *)
    } [@@deriving sexp, fields]

  (** [t] describes how to invoke a compiler. *)
  type t =
    { enabled : bool           (* Whether the compiler is enabled. *)
    ; style   : string         (* The 'style' of compiler being described. *)
    ; emits   : string list    (* The architecture the compiler will emit. *)
    ; cmd     : string         (* The compiler command. *)
    ; argv    : string list    (* The arguments to the command. *)
    ; herd    : string option  (* If present, the 'herd' command to use. *)
    ; ssh     : ssh option     (* If present, details for executing the compiler over SSH. *)
    } [@@deriving sexp, fields]

  include Pretty_printer.S with type t := t

  (** [with_id] is a [t] bundled with its own compiler ID. *)
  type with_id =
    { cid   : Id.t
    ; cspec : t
    }
end

(** [WithSpec] is an interface for modules containing a compiler
    specification. *)
module type WithSpec = sig
  val cspec : Spec.with_id
end

(** [Set] is a module for dealing with sets of compiler specs. *)
module Set : sig
  type t

  include Pretty_printer.S with type t := t

  (** [pp_verbose verbose f specs] prints a [specs] with the level of
     verbosity implied by [verbose]. *)
  val pp_verbose : bool -> Format.formatter -> t -> unit

  include Sexpable.S with type t := t

  (** [get specs cid] tries to look up compiler ID [cid] in specs
      [specs], and emits an error if it can't. *)
  val get : t -> Id.t -> Spec.t Or_error.t

  (** [filter ~f specs] applies a filtering predicate to the
      specifications in [specs]. *)
  val filter
    :  f : (Spec.t -> bool)
    -> t
    -> t

  (** [test ~f specs] runs a testing predicate [f] over each enabled
      compiler in [specs].  It returns [specs] with all disabled
      and failed compilers removed, and the list of any
      testing errors discovered. *)
  val test
    :  f : (Spec.with_id -> unit Or_error.t)
    -> t
    -> t * Error.t list

  (** [load ~file] loads a spec set from [file]. *)
  val load : path:string -> t Or_error.t

  (** [map ~f specs] maps a function across each enabled
      compiler ID and spec pair in [specs]. *)
  val map : f:(Spec.with_id -> 'a) -> t -> 'a list
end

(** [S] is the basic interface compilers must implement. *)
module type S = sig
  (** [test_args] is the set of arguments sent to the compiler to test
      that it works.  Usually, this will be some form of version
      command. *)
  val test_args : string list

  (** [compile_args ~args ~emits ~infile ~outfile] takes the set of
      arguments [args] the user supplied, the name of the architecture
      [emits] the compiler is going to emit, the input file
      [infile], and the output file [outfile], and produces a final
      argument vector to send to the compiler. *)
  val compile_args
    :  args    : string list
    -> emits   : string list
    -> infile  : string
    -> outfile : string
    -> string list
end

(** [Hooks] is the signature of mechanisms used to tell compilers to
   do something before/after a compilation.

    The main use of this module is to set up file transfers when doing
   remote compilation. *)
module type Hooks = sig
  (** [pre ~infile ~outfile] is run before compiling, and, if the
      hook is successful, returns the new infile and outfile to use
      for the compilation. *)
  val pre : infile:string -> outfile:string -> (string * string) Or_error.t

  (** [post ~infile ~outfile] is run after a successful compile.
      It receives the *original* input and output file names---not the
      ones returned by [pre]. *)
  val post : infile:string -> outfile:string -> unit Or_error.t
end

(** [Intf] is the outward-facing interface of compiler modules. *)
module type Intf = sig
  include WithSpec

  (** [test ()] tests that the compiler is working. *)
  val test : unit -> unit Or_error.t

  (** [compile ~infile ~outfile] runs the compiler on [infile],
      emitting assembly to [outfile] and returning any errors that arise. *)
  val compile : infile:string -> outfile:string -> unit Or_error.t
end

(** [Make] produces a runnable compiler satisfying [Intf] from a
    triple of spec [P], compiler [C], hooks [H], and runner [R]. *)
module Make
  : functor (P : WithSpec)
    -> functor (C : S)
      -> functor (H : Hooks)
        -> functor (R : Run.Runner)
          -> Intf

(** [from_spec f spec] takes a function that generates a first-class
   [S] from a spec, and attempts to produce a first-class compiler
   module. *)
val from_spec
  :  (Spec.with_id -> (module S) Or_error.t)
  -> Spec.with_id
  -> (module Intf) Or_error.t
