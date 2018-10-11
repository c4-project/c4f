open Core

(** [Pathset.compiler] is a record containing compiler-specific paths. *)
type compiler =
  { a_path    : string (* Path to assembly output *)
  ; lita_path : string (* Path to litmus output *)
  }

(** [Pathset.t] is a record containing various paths used in the
    execution of [act]. *)
type t =
  { basename       : string
  ; c_path         : string
  ; litc_path      : string
  ; out_root       : string
  ; compiler_paths : (CompilerSpec.Id.t, compiler) List.Assoc.t
  }

(** [make_dir_structure] tries to make the output directories
   mentioned in a [Pathset.t]. *)
val make_dir_structure : t -> unit Or_error.t

(** [make] constructs a [Pathset.t] from a compiler spec set and
   various other paths. *)
val make
  :  CompilerSpec.set
  -> root_path    : string
  -> results_path : string
  -> c_fname      : string
  -> t

include Pretty_printer.S with type t := t
