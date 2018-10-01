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

      module Location = struct
        type t = X86Ast.location
        let pp = P.pp_location

        let indirect_abs_type ( { in_seg; in_disp; in_base; in_index } : X86Ast.indirect) =
          let open Language.AbsLocation in
          match in_seg, in_disp, in_base, in_index with
          (* In the sort of code act is going to analyse, ESP is almost
             always pointing to the top of the stack. *)
          | None, None, Some ESP, None ->
             StackPointer
          (* This might be incorrect. *)
          | None, None, Some EBP, None ->
             StackPointer
          (* Typically, [ EBP - i ] is a stack location: EBP is the
             frame pointer, and the x86 stack grows downwards. *)
          | None, Some (DispNumeric i), Some EBP, None ->
             StackOffset i
          | _, _, _, _ -> Unknown

        let abs_type =
          let open Language.AbsLocation in
          function
          | X86Ast.LocReg ESP
            | LocReg EBP -> StackPointer
          | X86Ast.LocReg _ -> GeneralRegister
          | X86Ast.LocIndirect i -> indirect_abs_type i
      end

      module Statement = struct
        open X86Ast

        type t = X86Ast.statement

        let pp = P.pp_statement

        let zero_operands (operands : operand list)
            : Language.AbsOperands.t =
          let open Language.AbsOperands in
          if List.is_empty operands
          then None
          else Erroneous

        let src_dst_operands (operands : operand list)
          : Language.AbsOperands.t =
          let open Language.AbsOperands in
          let open X86Dialect.ATTTraits in
          to_src_dst operands
          |> Option.value_map
               ~f:(function
                   | { src = OperandLocation s
                     ; dst = OperandLocation d
                     } ->
                      LocTransfer
                        { src = Location.abs_type s
                        ; dst = Location.abs_type d
                        }
                   | _ -> None (* TODO(@MattWindsor91): flag erroneous *)
                  )
               ~default:None

        let basic_operands (o : [< X86Ast.basic_opcode])
            (operands : X86Ast.operand list) =
          let open Language.AbsOperands in
          match o with
          | `Leave
            | `Mfence
            | `Nop
            | `Ret -> zero_operands operands
          | `Mov -> src_dst_operands operands
          (* TODO(@MattWindsor91): analyse other opcodes! *)
          | `Add
            | `Pop
            | `Push
            | `Sub -> Other

        let instruction_operands_inner {opcode; operands; _} =
          match opcode with
          | X86Ast.OpBasic b -> basic_operands b operands
          | X86Ast.OpSized (b, _) -> basic_operands b operands
          | _ -> Language.AbsOperands.Other

        let%expect_test "instruction_operands_inner: nop -> none" =
          Format.printf "%a@."
                        Language.AbsOperands.pp
                        (instruction_operands_inner
                           { opcode = X86Ast.OpBasic `Nop
                           ; operands = []
                           ; prefix = None
                           });
          [%expect {| none |}]

        let%expect_test "instruction_operands_inner: nop $42 -> error" =
          Format.printf "%a@."
                        Language.AbsOperands.pp
                        (instruction_operands_inner
                           { opcode = X86Ast.OpBasic `Nop
                           ; operands = [ X86Ast.OperandImmediate
                                            (X86Ast.DispNumeric 42) ]
                           ; prefix = None
                           });
          [%expect {| <invalid operands> |}]

        let%expect_test "instruction_operands_inner: mov %ESP, %EBP" =
          Format.printf "%a@."
                        Language.AbsOperands.pp
                        (instruction_operands_inner
                           { opcode = X86Ast.OpBasic `Mov
                           ; operands = [ X86Ast.OperandLocation (X86Ast.LocReg ESP)
                                        ; X86Ast.OperandLocation (X86Ast.LocReg EBP)
                                        ]
                           ; prefix = None
                           });
          [%expect {| &stack -> &stack |}]


        let%expect_test "instruction_operands_inner: movl %ESP, %EBP" =
          Format.printf "%a@."
                        Language.AbsOperands.pp
                        (instruction_operands_inner
                           { opcode = X86Ast.OpSized (`Mov, X86SLong)
                           ; operands = [ X86Ast.OperandLocation (X86Ast.LocReg ESP)
                                        ; X86Ast.OperandLocation (X86Ast.LocReg EBP)
                                        ]
                           ; prefix = None
                           });
          [%expect {| &stack -> &stack |}]


        let instruction_operands =
          function
          | StmInstruction i -> Some (instruction_operands_inner i)
          | _ -> None


        let nop () = X86Ast.StmNop

        let basic_instruction_type
            : [< X86Ast.basic_opcode] -> Language.AbsInstruction.t =
          let open Language.AbsInstruction in
          function
          | `Add    -> Arith
          | `Leave  -> Call
          | `Mfence -> Fence
          | `Mov    -> Move
          | `Nop    -> Nop
          | `Pop    -> Stack
          | `Push   -> Stack
          | `Ret    -> Call
          | `Sub    -> Arith

        let instruction_type ({opcode; _} : X86Ast.instruction) =
          let open Language.AbsInstruction in
          match opcode with
          | X86Ast.OpDirective _ ->
             (* handled by abs_type below. *)
             assert false
          | X86Ast.OpJump _ -> Jump
          | X86Ast.OpBasic b -> basic_instruction_type b
          | X86Ast.OpSized (b, _) -> basic_instruction_type b
          | X86Ast.OpUnknown _ -> Other


        let abs_type =
          let open Language.AbsStatement in
          function
          | StmInstruction { opcode = OpDirective s; _ } ->
             Directive s
          | StmInstruction i -> Instruction (instruction_type i)
          | StmLabel l -> Label l
          | StmNop -> Blank

        let fold_map_symbols = X86Ast.fold_map_statement_symbols
      end

      module Constant = struct
        type t = X86Ast.operand (* TODO: this is too weak *)
        let pp = P.pp_operand
      end
    end)

module Lang = Make (X86Dialect.ATTTraits) (X86PP.ATT)

