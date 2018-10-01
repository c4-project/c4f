open Core

type perror =
  | Range of (Lexing.position * Lexing.position * X86Base.parse_error)
  | Uncaught of Lexing.position

module Frontend =
  LangFrontend.Make (
      struct
        type perr = perror
        type lerr = string * Lexing.position
        type token = X86ATTParser.token
        type ast = X86Ast.t

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
            Ok ({ syntax = X86Dialect.Att
                ; program = X86ATTParser.main lex lexbuf
                } : X86Ast.t)
          with
          | LexMisc.Error (e, _) ->
             Error (LangParser.Lex (e, lexbuf.lex_curr_p))
          | X86Base.ParseError ((p1, p2), ty) ->
             Error (LangParser.Parse (Range (p1, p2, ty)))
          | X86ATTParser.Error ->
             Error (LangParser.Parse (Uncaught lexbuf.lex_curr_p))
      end)

(* TODO(@MattWindsor91): break this out into a separate module. *)
module Make (T : X86Dialect.Traits) (P : X86PP.S) =
  Language.Make
    (struct
      let name = (Language.X86 (T.dialect))

      let is_program_label = X86Base.is_program_label

      module Statement = struct
        open X86Ast

        type t = X86Ast.statement

        let pp = P.pp_statement

        let nop () = X86Ast.StmNop

        let basic_instruction_type
            : [< X86Ast.basic_opcode] -> Language.abs_instruction =
          function
          | `Add    -> Language.AIArith
          | `Leave  -> Language.AICall
          | `Mfence -> Language.AIFence
          | `Mov    -> Language.AIMove
          | `Nop    -> Language.AINop
          | `Pop    -> Language.AIStack
          | `Push   -> Language.AIStack
          | `Ret    -> Language.AICall
          | `Sub    -> Language.AIArith

        let instruction_type ({opcode; _} : X86Ast.instruction) =
          match opcode with
          | X86Ast.OpDirective _ ->
             (* handled by statement_type below. *)
             assert false
          | X86Ast.OpJump _ -> Language.AIJump
          | X86Ast.OpBasic b -> basic_instruction_type b
          | X86Ast.OpSized (b, _) -> basic_instruction_type b
          | X86Ast.OpUnknown _ -> Language.AIOther


        let statement_type =
          function
          | StmInstruction { opcode = OpDirective s; _ } ->
             Language.ASDirective s
          | StmInstruction i -> Language.ASInstruction (instruction_type i)
          | StmLabel l -> Language.ASLabel l
          | StmNop -> Language.ASBlank

        let fold_map_symbols = X86Ast.fold_map_statement_symbols
      end

      module Constant = struct
        type t = X86Ast.operand (* TODO: this is too weak *)
        let pp = P.pp_operand
      end

      module Location = struct
        type t = X86Ast.indirect (* TODO: as is this *)
        let pp = P.pp_indirect
      end
    end)

module Lang = Make (X86Dialect.ATTTraits) (X86PP.ATT)

