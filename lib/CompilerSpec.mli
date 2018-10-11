open Core
open Lang
open Sexplib

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
 * t
 *)

(** [CompilerSpec.t] describes how to invoke a compiler. *)
type t =
  { style : style          (* The 'style' of compiler being described. *)
  ; emits : Language.name  (* The architecture the compiler will emit. *)
  ; cmd   : string         (* The compiler command. *)
  ; argv  : string list    (* The arguments to the command. *)
  }
val t_of_sexp : Sexp.t -> t
val sexp_of_t : t -> Sexp.t
val pp : Format.formatter -> t -> unit

module Id : sig
  (** [t] is the type of compiler IDs. *)
  type t = string list [@@deriving compare, hash, sexp]

  include Core.Identifiable.S_plain with type t := t
end

(** [CompilerSpec.set] is an associative list mapping compiler names
   to compiler specs. *)
type set = (Id.t, t) List.Assoc.t
val set_of_sexp : Sexp.t -> set
val sexp_of_set : set -> Sexp.t

val load_specs : path:string -> set Or_error.t
