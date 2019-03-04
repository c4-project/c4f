(** [has_extension] asks whether a given filename has the extension [ext]. *)
val has_extension : ext:string -> string -> bool

(** [concat_list xs] concatenates a list [xs] of directory fragments. *)
val concat_list : string list -> string
