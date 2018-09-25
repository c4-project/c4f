open Core

(** [null_formatter ()] is a formatter that silently discards anything
    printed to it. *)

val null_formatter : unit -> Format.formatter

(** [format_to_string pp v] runs formatter [pp] on value [v], and
    dumps the results into a string. *)
val format_to_string : (Format.formatter -> 'v -> unit)
                       -> 'v
                       -> string

(** [pp_option f ~pp o] pretty-prints the value inside [o] using [pp]
   onto formatter [f], if there is one.  Otherwise, it is a no-op. *)
val pp_option : Format.formatter ->
                pp:(Format.formatter -> 'a -> unit) ->
                'a option ->
                unit

(** [pp_c_braces f pi] wraps a vertical pretty-printer [pi] inside a
   C-style brace pair. *)
val pp_c_braces : Format.formatter
                  -> (Format.formatter -> unit)
                  -> unit

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
