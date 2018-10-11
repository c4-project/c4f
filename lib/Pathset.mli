open Core

(** [Pathset.compiler] is a record containing compiler-specific paths. *)
type compiler =
  { asm_path  : string (* Path to assembly output *)
  ; lita_path : string (* Path to litmus output *)
  ; herd_path : string (* Path to herd output *)
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

(** [compiler_paths_of ps cid] gets the compiler paths for compiler ID
    [cid], throwing an exception if [cid] isn't known to [ps]. *)
val compiler_paths_of : t -> CompilerSpec.Id.t -> compiler

(** [compiler_asm_path ps cid] gets the assembly path for compiler ID
   [cid], throwing an exception if [cid] isn't known to [ps]. *)
val compiler_asm_path : t -> CompilerSpec.Id.t -> string

(** [compiler_lita_path ps cid] gets the assembly litmus path for
   compiler ID [cid], throwing an exception if [cid] isn't known to
   [ps]. *)
val compiler_lita_path : t -> CompilerSpec.Id.t -> string

(** [compiler_herd_path ps cid] gets the herd output path for
   compiler ID [cid], throwing an exception if [cid] isn't known to
   [ps]. *)
val compiler_herd_path : t -> CompilerSpec.Id.t -> string


(** [mkdirs] tries to make the output directories
   mentioned in a [Pathset.t]. *)
val mkdirs : t -> unit Or_error.t

(** [make] constructs a [Pathset.t] from a compiler spec set and
   various other paths. *)
val make
  :  CompilerSpec.set
  -> in_root  : string
  -> out_root : string
  -> c_fname  : string
  -> t

(** [make_and_mkdirs] constructs a [Pathset.t] per [make], then
    tries to make the directories through [mkdirs]. *)
val make_and_mkdirs
  :  CompilerSpec.set
  -> in_root  : string
  -> out_root : string
  -> c_fname  : string
  -> t Or_error.t

include Pretty_printer.S with type t := t
