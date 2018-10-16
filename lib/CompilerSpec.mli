open Core

(*
 * style
 *)

(** [CompilerSpec.style] enumerates the various flavours of compiler
   that act understands. *)
type style =
  | Gcc (* GCC *)

(** [CompilerSpec.show_style] returns a string representation of a
   [style]. *)
val show_style : style -> string

(** [CompilerSpec.pp_style] pretty-prints a [style]. *)
val pp_style : Format.formatter -> style -> unit

(*
 * ssh
 *)

(** [CompilerSpec.ssh] describes how to invoke a compiler remotely
   from ssh. *)
type ssh =
  { host     : string        (* The host to run on. *)
  ; user     : string option (* The user to run as. *)
  ; copy_dir : string        (* The remote directory to use for temporary results. *)
  } [@@deriving sexp, fields]

(*
 * t
 *)

(** [CompilerSpec.t] describes how to invoke a compiler. *)
type t =
  { enabled : bool           (* Whether the compiler is enabled. *)
  ; style   : style          (* The 'style' of compiler being described. *)
  ; emits   : string list    (* The architecture the compiler will emit. *)
  ; cmd     : string         (* The compiler command. *)
  ; argv    : string list    (* The arguments to the command. *)
  ; herd    : string option  (* If present, the 'herd' command to use. *)
  ; ssh     : ssh option     (* If present, details for executing the compiler over SSH. *)
  } [@@deriving sexp, fields]

include Pretty_printer.S with type t := t

module Id : sig
  (** [t] is the type of compiler IDs. *)
  type t = string list [@@deriving compare, hash, sexp]

  include Core.Identifiable.S_plain with type t := t
end

module Set : sig
  type elt = t

  type t

  include Pretty_printer.S with type t := t

  (** [pp_verbose verbose f specs] prints a [specs] with the level of
     verbosity implied by [verbose]. *)
  val pp_verbose : bool -> Format.formatter -> t -> unit

  include Sexpable.S with type t := t

  (** [get specs cid] tries to look up compiler ID [cid] in specs
      [specs], and emits an error if it can't. *)
  val get : t -> Id.t -> elt Or_error.t

  (** [filter ~f specs] applies a filtering predicate to the
      specifications in [specs]. *)
  val filter
    :  f : (elt -> bool)
    -> t
    -> t

  (** [test ~f specs] runs a testing predicate [f] over each enabled
      compiler in [specs].  It returns [specs] with all disabled
      and failed compilers removed, and the list of any
      testing errors discovered. *)
  val test
    :  f : (Id.t -> elt -> unit Or_error.t)
    -> t
    -> t * Error.t list

  (** [load ~file] loads a spec set from [file]. *)
  val load : path:string -> t Or_error.t

  (** [map ~f specs] maps a function across each enabled
      compiler ID and spec pair in [specs]. *)
  val map : f:(Id.t -> elt -> 'a) -> t -> 'a list
end
