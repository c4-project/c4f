open Core

(** [Pathset.t] is a record containing various paths used in the
    execution of [act]. *)
type t

(** [File] contains a file-specific path set. *)
module File : sig
  type ps = t
  type t

  (** [basename f] gets the basename of [f]. *)
  val basename : t -> string;;

  (** [c_path f] gets the path of the C input file for [f]. *)
  val c_path : t -> string;;

  (** [litc_path f] gets the path of the C/litmus input file for
     [f]. *)
  val litc_path : t -> string;;

  (** [asm_path f] gets the path of the assembly output file for
     [f]. *)
  val asm_path : t -> string;;

  (** [lita_path f] gets the path of the assembly litmus output file
     for [f]. *)
  val lita_path : t -> string;;

  (** [herd_path f] gets the path of the Herd run output file for
     [f]. *)
  val herd_path : t -> string;;

  (** [make ps basename] makes file-specific paths for
      basename [basename], according to pathset [ps]. *)
  val make : ps -> string -> t;;
end

(** [mkdirs] tries to make the output directories
   mentioned in a [Pathset.t]. *)
val mkdirs : t -> unit Or_error.t;;

(** [make] constructs a [Pathset.t] from a compiler spec set and
   various other paths. *)
val make
  :  Compiler.CSpec.WithId.t
  -> in_root  : string
  -> out_root : string
  -> t
;;

(** [make_and_mkdirs] constructs a [Pathset.t] per [make], then
    tries to make the directories through [mkdirs]. *)
val make_and_mkdirs
  :  Compiler.CSpec.WithId.t
  -> in_root  : string
  -> out_root : string
  -> t Or_error.t
;;

include Pretty_printer.S with type t := t
