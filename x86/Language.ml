open Core
open Lib

module AttFrontend =
  LangFrontend.Make (
      struct
        type ast = Ast.t

        module I = ATTParser.MenhirInterpreter

        let lex =
          let module T = ATTLexer.Make(LexUtils.Default) in
          T.token

        let parse = ATTParser.Incremental.main;;

        let message = ATTMessages.message;;
      end)

module type Intf = sig
  include Dialect.Intf
  include PP.Printer
  include
    Language.Intf
    with type Constant.t = Ast.Operand.t
     and type Location.t = Ast.location
     and type Instruction.t = Ast.instruction
     and type Statement.t = Ast.statement

  val make_jump_operand : string -> Ast.Operand.t
end

module Make (T : Dialect.Intf) (P : PP.Printer) = struct
  include T
  include P

  let make_jump_operand jsym =
    Ast.(
      let disp = DispSymbolic jsym in
      match T.symbolic_jump_type with
      | `Indirect ->
        Operand.Location (LocIndirect (Indirect.make ~disp ()))
      | `Immediate ->
        Operand.Immediate disp
    )

  include
    Language.Make
      (struct
        let name = "X86"
        let pp_comment = P.pp_comment

        module Symbol = struct
          include String

          let abstract = Fn.id
          let abstract_demangle str =
            (* These are the types of manglings we've seen in practice: *)
            List.filter_opt
              [ Some str  (* GNU/Linux ELF *)
              ; String.chop_prefix ~prefix:"_" str (* Darwin Mach-O *)
              ]
          ;;

          module OnStringsS = struct
            type t = string
            type cont = string

            let fold_map ~f ~init sym = f init sym
          end
        end

        module Location = struct
          type t = Ast.location
          let sexp_of_t = [%sexp_of: Ast.location]
          let t_of_sexp = [%of_sexp: Ast.location]

          let pp = P.pp_location

          let make_heap_loc l =
            Ast.(LocIndirect (Indirect.make ~disp:(DispSymbolic l) ()))
          ;;

          let indirect_abs_type (i : Ast.Indirect.t) =
            let open Abstract.Location in
            let open Ast.Indirect in
            match (seg i), (disp i), (base i), (index i) with
            (* Typically, [ EBP - i ] is a stack location: EBP is the
               frame pointer, and the x86 stack grows downwards. *)
            | None, Some (DispNumeric i), Some EBP, None ->
              StackOffset i
            (* This is the same as [ EBP - 0 ]. *)
            | None, None, Some ESP, None ->
              StackOffset 0
            (* This may be over-optimistic. *)
            | None, Some (DispSymbolic s), None, None ->
              Heap s
            | _, _, _, _ -> Unknown

          let abs_type =
            let open Abstract.Location in
            function
            | Ast.LocReg ESP
            | LocReg EBP -> StackPointer
            | Ast.LocReg _ -> GeneralRegister
            | Ast.LocIndirect i -> indirect_abs_type i
        end

        module Instruction = struct
          open Ast

          type t = Ast.instruction
          let sexp_of_t = [%sexp_of: Ast.instruction]
          let t_of_sexp = [%of_sexp: Ast.instruction]

          type sym = Symbol.t
          type loc = Location.t

          let pp = P.pp_instruction

          let jump l =
            { prefix = None
            ; opcode = OpJump None
            ; operands = [ make_jump_operand l ]
            }

          (** [basic_instruction_type o] assigns an act classification to a
              primitive opcode [o]. *)
          let basic_instruction_type
            : [< Ast.basic_opcode] -> Abstract.Instruction.t = function
            | `Add    -> Arith
            | `Call   -> Call
            | `Cmp    -> Compare
            | `Leave  -> Call
            | `Mfence -> Fence
            | `Mov    -> Move
            | `Nop    -> Nop
            | `Pop    -> Stack
            | `Push   -> Stack
            | `Ret    -> Return
            | `Sub    -> Arith
            | `Xchg   -> Rmw
            | `Xor    -> Logical

          let zero_operands (operands : Operand.t list)
            : Abstract.Operands.t =
            let open Abstract.Operands in
            if List.is_empty operands
            then None
            else Erroneous

          let src_dst_operands (operands : Operand.t list)
            : Abstract.Operands.t =
            let open Abstract.Operands in
            let open Operand in
            let open T in
            to_src_dst operands
            |> Option.value_map
              ~f:(function
                  | { src = Location s
                    ; dst = Location d
                    } ->
                    LocTransfer
                      { src = Location.abs_type s
                      ; dst = Location.abs_type d
                      }
                  | { src = Immediate (DispNumeric k)
                    ; dst = Location d
                    } ->
                    IntImmediate
                      { src = k
                      ; dst = Location.abs_type d
                      }
                  | _ -> None (* TODO(@MattWindsor91): flag erroneous *)
                )
              ~default:None

          let basic_operands (o : [< Ast.basic_opcode])
              (operands : Ast.Operand.t list) =
            let open Abstract.Operands in
            match o with
            | `Leave
            | `Mfence
            | `Nop
            | `Ret -> zero_operands operands
            | `Add
            | `Sub
            | `Mov
            | `Xor -> src_dst_operands operands
            (* TODO(@MattWindsor91): analyse other opcodes! *)
            | `Call
            | `Cmp
            | `Pop
            | `Xchg
            | `Push -> Unknown

          let jump_operands =
            Abstract.Operands.(
              function
              | [o] ->
                begin
                  match o with
                  | Operand.Location (LocIndirect i) ->
                    begin
                      match Indirect.disp i with
                      | Some (DispSymbolic s) -> SymbolicJump s
                      | _ -> Unknown
                    end
                  | Operand.Immediate (Ast.DispSymbolic s) -> SymbolicJump s
                  | _ -> Unknown
                end
              | _ -> Erroneous
            )

          let abs_operands {opcode; operands; _} =
            match opcode with
            | Ast.OpBasic b -> basic_operands b operands
            | Ast.OpSized (b, _) -> basic_operands b operands
            | Ast.OpJump _ -> jump_operands operands
            | Ast.OpDirective _ -> Abstract.Operands.Other
            | Ast.OpUnknown _ -> Abstract.Operands.Unknown

          let%expect_test "abs_operands: nop -> none" =
            Format.printf "%a@."
              Abstract.Operands.pp
              (abs_operands
                 { opcode = Ast.OpBasic `Nop
                 ; operands = []
                 ; prefix = None
                 });
            [%expect {| none |}]

          let%expect_test "abs_operands: jmp, AT&T style" =
            Format.printf "%a@."
              Abstract.Operands.pp
              (abs_operands
                 { opcode = Ast.OpJump None
                 ; operands =
                     [ Operand.Location
                         (Ast.LocIndirect
                            (Ast.Indirect.make
                               ~disp:(Ast.DispSymbolic "L1") ()))
                     ]
                 ; prefix = None
                 });
            [%expect {| jump->L1 |}]

          let%expect_test "abs_operands: nop $42 -> error" =
            Format.printf "%a@."
              Abstract.Operands.pp
              (abs_operands
                 { opcode = Ast.OpBasic `Nop
                 ; operands = [ Operand.Immediate
                                  (Ast.DispNumeric 42) ]
                 ; prefix = None
                 });
            [%expect {| <invalid operands> |}]

          let%expect_test "abs_operands: mov %ESP, %EBP" =
            Format.printf "%a@."
              Abstract.Operands.pp
              (abs_operands
                 { opcode = Ast.OpBasic `Mov
                 ; operands = [ Operand.Location (Ast.LocReg ESP)
                              ; Operand.Location (Ast.LocReg EBP)
                              ]
                 ; prefix = None
                 });
            [%expect {| &stack -> &stack |}]

          let%expect_test "abs_operands: movl %ESP, %EBP" =
            Format.printf "%a@."
              Abstract.Operands.pp
              (abs_operands
                 { opcode = Ast.OpSized (`Mov, SLong)
                 ; operands = [ Operand.Location (Ast.LocReg ESP)
                              ; Operand.Location (Ast.LocReg EBP)
                              ]
                 ; prefix = None
                 });
            [%expect {| &stack -> &stack |}]

          let abs_type ({opcode; _} : Ast.instruction) =
            let open Abstract.Instruction in
            match opcode with
            | Ast.OpDirective _ ->
              (* handled by abs_type below. *)
              Other
            | Ast.OpJump _ -> Jump
            | Ast.OpBasic b -> basic_instruction_type b
            | Ast.OpSized (b, _) -> basic_instruction_type b
            | Ast.OpUnknown _ -> Unknown

          module OnSymbolsS = struct
            type t = string
            type cont = Ast.instruction
            let fold_map = Ast.fold_map_instruction_symbols
          end

          module OnLocationsS = struct
            type t = Location.t
            type cont = Ast.instruction
            let fold_map = Ast.fold_map_instruction_locations
          end
        end

        module Statement = struct
          open Ast

          type sym = Symbol.t
          type t = Ast.statement
          let sexp_of_t = [%sexp_of: Ast.statement]
          let t_of_sexp = [%of_sexp: Ast.statement]
          let pp = P.pp_statement

          type ins = Ast.instruction

          let empty () = Ast.StmNop
          let label s = Ast.StmLabel s
          let instruction i = Ast.StmInstruction i

          let abs_type =
            let open Abstract.Statement in
            function
            | StmInstruction { opcode = OpDirective s; _ } ->
              Directive s
            | StmInstruction i -> Instruction (Instruction.abs_type i)
            | StmLabel l -> Label l
            | StmNop -> Blank

          module OnSymbolsS = struct
            type t = string
            type cont = Ast.statement
            let fold_map = Ast.fold_map_statement_symbols
          end

          module OnInstructionsS = struct
            type t = Instruction.t
            type cont = Ast.statement
            let fold_map = Ast.fold_map_statement_instructions
          end
        end

        module Constant = struct
          (* TODO: this is too weak *)
          include Ast.Operand

          let pp = P.pp_operand

          let zero = Ast.Operand.Immediate (Ast.DispNumeric 0)
        end
      end)
end

module ATT = Make (Dialect.ATT) (PP.ATT)

let%expect_test "is_program_label: positive Mach-O example, AT&T" =
  printf "%b" (ATT.Symbol.is_program_label "_P0");
  [%expect {| true |}]

let%expect_test "is_program_label: positive ELF example, AT&T" =
  printf "%b" (ATT.Symbol.is_program_label "P0");
  [%expect {| true |}]

let%expect_test "is_program_label: wrong suffix, Mach-O, AT&T" =
  printf "%b" (ATT.Symbol.is_program_label "_P0P");
  [%expect {| false |}]

let%expect_test "is_program_label: wrong suffix, ELF, AT&T" =
  printf "%b" (ATT.Symbol.is_program_label "P0P");
  [%expect {| false |}]

let%expect_test "is_program_label: negative, AT&T" =
  printf "%b" (ATT.Symbol.is_program_label "_P-1");
  [%expect {| false |}]

let%expect_test "abs_operands: add $-16, %ESP, AT&T" =
  Format.printf "%a@."
    Abstract.Operands.pp
    (ATT.Instruction.abs_operands
       { opcode = Ast.OpBasic `Add
       ; operands = [ Ast.Operand.Immediate (Ast.DispNumeric (-16))
                    ; Ast.Operand.Location (Ast.LocReg ESP)
                    ]
       ; prefix = None
       });
  [%expect {| $-16 -> &stack |}]

module Intel = Make (Dialect.Intel) (PP.Intel)

let%expect_test "abs_operands: add ESP, -16, Intel" =
  Format.printf "%a@."
    Abstract.Operands.pp
    (Intel.Instruction.abs_operands
       { opcode = Ast.OpBasic `Add
       ; operands = [ Ast.Operand.Location (Ast.LocReg ESP)
                    ; Ast.Operand.Immediate (Ast.DispNumeric (-16))
                    ]
       ; prefix = None
       });
  [%expect {| $-16 -> &stack |}]

module Herd7 = Make (Dialect.Herd7) (PP.Herd7)

let lang_of_dialect =
  function
  | Dialect.Att   -> (module ATT : Intf)
  | Dialect.Intel -> (module Intel : Intf)
  | Dialect.Herd7 -> (module Herd7 : Intf)

let frontend_of_dialect =
  function
  | Dialect.Att
    -> Or_error.return (module AttFrontend : LangFrontend.Intf with type ast = Ast.t)
  | d ->
    Or_error.error "x86 dialect unsupported" d [%sexp_of: Dialect.t]
