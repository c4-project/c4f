open Core

(** [Pathset.t] is a record containing various paths used in the
   execution of [act]. *)
type t =
  { basename   : string                        (* Inferred base name *)
  ; c_path     : string                        (* Path to the executable C file     *)
  ; litc_path  : string                        (* Path to the C litmus test         *)
  ; out_root   : string                        (* Root for the output dir structure *)
  ; a_paths    : (string, string) List.Assoc.t (* Paths to output for each compiler *)
  ; lita_paths : (string, string) List.Assoc.t (* Paths to litmus for each compiler *)
  }

(** [make_dir_structure] tries to make the output directories
   mentioned in a [Pathset.t]. *)
val make_dir_structure : t -> unit Or_error.t

(** [make] constructs a [Pathset.t] from a compiler spec set and
   various other paths. *)
val make : CompilerSpec.set
           -> root_path    : string
           -> results_path : string
           -> c_fname      : string
           -> t

(** [pp f ps] pretty-prints a pathset [p] onto formatter [f]. *)
val pp : Format.formatter -> t -> unit
