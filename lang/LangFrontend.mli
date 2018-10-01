open Core

module type S = sig
  (** [ast] is the type of the syntax tree outputted by the frontend. *)
  type ast
  (** [perr] is the type of parse errors. *)
  type perr
  (** [lerr] is the type of lex errors. *)
  type lerr

  (** [pp_perr f perr] pretty-prints a parse error [perr] onto formatter [f]. *)
  val pp_perr : Format.formatter -> perr -> unit

  (** [pp_perr f perr] pretty-prints a lex error [lerr] onto formatter [f]. *)
  val pp_lerr : Format.formatter -> lerr -> unit

  (** [run_ic file ic] runs the language frontend on input channel
     [ic].  [file] can be set to pass in a filename, for error
     reporting purposes. *)
  val run_ic : ?file:string -> In_channel.t -> (ast, (perr, lerr) LangParser.error) result

  (** [run_file file] runs the language frontend on filename [file]. *)
  val run_file : file:string -> (ast, (perr, lerr) LangParser.error) result

  (** [run_stdin ()] runs the language frontend on standard input. *)
  val run_stdin : unit -> (ast, (perr, lerr) LangParser.error) result
end

(** [Make] lifts an instance of [LangParser.S] into a frontend. *)
module Make : functor (SI : LangParser.S) -> (S with type ast = SI.ast)
