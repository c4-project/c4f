open Core

type perror =
  | Range of (Lexing.position * Lexing.position * X86Base.parse_error)
  | Uncaught of Lexing.position

module Frontend : (LangFrontend.S with type ast = X86Ast.statement list) =
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

module Lang : (Language.S with type statement = X86Ast.statement) = struct
  let name = (Language.X86 (X86Ast.SynAtt))

  type statement = X86Ast.statement
  type constant = X86Ast.operand (* TODO: this is too weak *)
  type location = X86Ast.indirect (* TODO: as is this *)

  let pp_statement = X86Ast.pp_statement X86Ast.SynAtt
  let pp_constant = X86Ast.pp_operand X86Ast.SynAtt
  let pp_location = X86Ast.pp_indirect X86Ast.SynAtt

  let nop () = X86Ast.StmNop

  let is_nop =
    function
    | X86Ast.StmNop -> true
    | _ -> false

  let instruction_type_inner _ (* { prefix; opcode; operands } *) =
    Language.AbsOther

  open X86Ast

  let map_disp_ids ~f =
    function
    | DispSymbolic s -> DispSymbolic (f s)
    | DispNumeric  k -> DispNumeric k

  let map_location_ids ~f { in_seg; in_disp; in_base; in_index } =
    { in_seg = in_seg (* register *)
    ; in_disp = Option.map ~f:(map_disp_ids ~f:f) in_disp
    ; in_base = in_base (* register *)
    ; in_index = in_index (* register and possible number *)
    }

  let rec map_operand_ids ~f =
    function
    | OperandIndirect i -> OperandIndirect (map_location_ids ~f:f i)
    | OperandReg r -> OperandReg r
    | OperandImmediate d -> OperandImmediate (map_disp_ids ~f:f d)
    | OperandString s -> OperandString s
    | OperandBop (l, b, r) ->
       OperandBop ( map_operand_ids ~f:f l
                  , b
                  , map_operand_ids ~f:f r
                  )

  let map_instr_ids ~f { prefix; opcode; operands } : X86Ast.instruction =
    { prefix = prefix
    ; opcode = opcode
    ; operands = (List.map ~f:(map_operand_ids ~f:f) operands)
    }

  let map_statement_ids ~f =
    function
    | X86Ast.StmInstruction i ->
       X86Ast.StmInstruction (map_instr_ids ~f:f i)
    | X86Ast.StmLabel l -> X86Ast.StmLabel (f l)
    | X86Ast.StmNop -> X86Ast.StmNop

  let instruction_type =
    function
    | X86Ast.StmInstruction i -> Some (instruction_type_inner i)
    | _ -> None

  let is_directive =
    function
    | X86Ast.StmInstruction {opcode = X86OpDirective _; _} -> true
    | _ -> false

  let is_program_boundary =
    function
    | X86Ast.StmLabel l -> X86Base.is_program_label l
    | _ -> false
end
