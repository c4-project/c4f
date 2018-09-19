open Core

(** [null_formatter ()] is a formatter that silently discards anything
    printed to it. *)

val null_formatter : unit -> Format.formatter

(** [pp_kv f k pv v] prints a key-value pair, whose key is the string
   [k] and value is the value [v] printable by [pv], onto formatter
   [f]. *)
val pp_kv : Format.formatter
            -> string
            -> (Format.formatter -> 'v -> unit)
            -> 'v
            -> unit

(** [pp_sr f sr] prints a string reference [sr] onto formatter [f]. *)
val pp_sr : Format.formatter
            -> string ref
            -> unit

(** [pp_sq f q] prints a string queue [q] onto formatter [f]. *)
val pp_sq : Format.formatter
            -> string Queue.t
            -> unit
