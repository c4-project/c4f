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
  } [@@deriving sexp]

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
  } [@@deriving sexp]

include Pretty_printer.S with type t := t

module Id : sig
  (** [t] is the type of compiler IDs. *)
  type t = string list [@@deriving compare, hash, sexp]

  include Core.Identifiable.S_plain with type t := t
end

(** [CompilerSpec.set] is an associative list mapping compiler names
   to compiler specs. *)
type set = (Id.t, t) List.Assoc.t [@@deriving sexp]

val load_specs : path:string -> set Or_error.t
