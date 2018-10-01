open Core

module type S = sig
  type ast
  type perr
  type lerr

  val pp_perr : Format.formatter -> perr -> unit
  val pp_lerr : Format.formatter -> lerr -> unit

  val run_ic : ?file:string -> In_channel.t -> (ast, (perr, lerr) LangParser.error) result

  val run_file : file:string -> (ast, (perr, lerr) LangParser.error) result
  val run_stdin : unit -> (ast, (perr, lerr) LangParser.error) result
end

module Make (SI : LangParser.S) : (S with type ast = SI.ast) = struct
  type lerr = SI.lerr
  type perr = SI.perr
  type ast = SI.ast

  let pp_perr = SI.pp_perr
  let pp_lerr = SI.pp_lerr

  let run_ic ?file ic =
    let lexbuf = Lexing.from_channel ic in
    Option.iter
      ~f:(fun f ->
        lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = f })
      file;
    SI.parse SI.lex lexbuf

  let run_file ~file =
    In_channel.with_file file ~f:(run_ic ~file)

  let run_stdin () =
    run_ic ~file:"(stdin)" In_channel.stdin
end
