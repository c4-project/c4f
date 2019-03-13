open Core

(** [Pathset.t] is a record containing various paths used in the
    execution of [act]. *)
type t

(** [File] contains a file-specific path set. *)
module File : sig
  type ps = t
  type t

  (** [name f] gets the basename of [f]. *)
  val name : t -> string

  (** [c_path f] gets the path of the C input file for [f]. *)
  val c_path : t -> Fpath.t

  (** [litc_path f] gets the path of the C/litmus input file for
     [f]. *)
  val litc_path : t -> Fpath.t

  (** [asm_path f] gets the path of the assembly output file for
     [f]. *)
  val asm_path : t -> Fpath.t

  (** [lita_path f] gets the path of the assembly litmus output file
     for [f]. *)
  val lita_path : t -> Fpath.t

  (** [herdc_path f] gets the path of the C/litmus Herd run output
     file for [f]. *)
  val herdc_path : t -> Fpath.t

  (** [herda_path f] gets the path of the assembly Herd run output
     file for [f]. *)
  val herda_path : t -> Fpath.t

  (** [make ps filename] makes file-specific paths for
      file [filename], according to pathset [ps]. *)
  val make : ps -> Fpath.t -> t
end

(** [mkdirs] tries to make the output directories
   mentioned in a [Pathset.t]. *)
val mkdirs : t -> unit Or_error.t

module Input_mode : sig
  type t

  val memalloy : input_root:Fpath.t -> t Or_error.t
  val litmus_only : files:Fpath.t list -> t Or_error.t

  val must_delitmusify : t -> bool
  (** [must_delitmusify imode] returns [true] if the tester must
      generate C files by de-litmusifying the C litmus tests, or
      [false] if they are already assumed to exist. *)
end

val make
  :  Id.t
  -> input_mode:Input_mode.t
  -> output_root:Fpath.t
  -> t Or_error.t
(** [make id ~input_mode ~output_root] constructs a pathset for
    compiler ID [id], with all output relative to [output_root]
    relative to [out_root], and the input determined by [input_mode]. *)

val make_and_mkdirs
  :  Id.t
  -> input_mode:Input_mode.t
  -> output_root:Fpath.t
  -> t Or_error.t
(** [make_and_mkdirs] behaves as {{!make}make}, then tries to make the
   directories through [mkdirs]. *)

val input_mode : t -> Input_mode.t
(** [input_mode ps] gets the input mode used to construct [ps]. *)

val to_files : t -> File.t list
(** [to_files ps] constructs a file pathset for each C litmus test in
   [ps]. *)

include Pretty_printer.S with type t := t
