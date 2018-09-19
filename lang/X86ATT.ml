open Core

type perror =
  | Range of (Lexing.position * Lexing.position * X86Base.parse_error)
  | Uncaught of Lexing.position

module Frontend : LangFrontend.S =
  LangFrontend.Make (
      struct
        type perr = perror
        type lerr = string * Lexing.position
        type token = X86ATTParser.token
        type ast = X86Ast.statement list

        let pp_perr f =
          function
          | Range (p1, p2, err) ->
             Format.fprintf f "@[parse@ error@ at@ %a:@ %a@]"
                            LexMisc.pp_pos2 (p1, p2)
                            X86Base.pp_error err
          | Uncaught pos ->
             Format.fprintf f "@[uncaught@ parse@ error@ near@ %a]"
                            LexMisc.pp_pos pos
        let pp_lerr f ((err, pos) : lerr) =
          Format.fprintf f "@[lexing@ error@ at@ %a:@ %s@]"
                         LexMisc.pp_pos pos
                         err

        let lex =
          let module T = X86ATTLexer.Make(LexUtils.Default) in
          T.token

        let parse lex lexbuf =
          try
            Ok (X86ATTParser.main lex lexbuf)
          with
          | LexMisc.Error (e, _) ->
             Error (LangParser.Lex (e, lexbuf.lex_curr_p))
          | X86Base.ParseError ((p1, p2), ty) ->
             Error (LangParser.Parse (Range (p1, p2, ty)))
          | X86ATTParser.Error ->
             Error (LangParser.Parse (Uncaught lexbuf.lex_curr_p))
      end)
