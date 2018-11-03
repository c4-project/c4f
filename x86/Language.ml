open Core
open Lib
open Utils

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
    Language.S
    with type Constant.t = Ast.Operand.t
     and type Location.t = Ast.Location.t
     and type Instruction.t = Ast.Instruction.t
     and type Statement.t = Ast.Statement.t
     and type Symbol.t = string

  val make_jump_operand : string -> Ast.Operand.t
end

module Make (T : Dialect.Intf) (P : PP.Printer) = struct
  include T
  include P

  let make_jump_operand jsym =
    Ast.(
      let disp = Disp.Symbolic jsym in
      match T.symbolic_jump_type with
      | `Indirect ->
        Operand.Location (Location.Indirect (Indirect.make ~disp ()))
      | `Immediate ->
        Operand.Immediate disp
    )

  include
    Language.Make (struct
        let name = "X86"
        let pp_comment = P.pp_comment

        module Symbol = struct
          include String

          let of_string_opt = Option.some

          let abstract = Fn.id
          let abstract_demangle str =
            (* These are the types of manglings we've seen in practice: *)
            List.filter_opt
              [ Some str  (* GNU/Linux ELF *)
              ; String.chop_prefix ~prefix:"_" str (* Darwin Mach-O *)
              ]
          ;;

          module OnStrings = Fold_map.Make0 (struct
            type t = string
            module Elt = String

            module On_monad (M : Monad.S) = struct
              let fold_map ~f ~init sym = f init sym
            end
          end)
        end

        module Location = struct
          type t = Ast.Location.t
          let sexp_of_t = [%sexp_of: Ast.Location.t]
          let t_of_sexp = [%of_sexp: Ast.Location.t]

          let pp = P.pp_location

          let make_heap_loc l =
            Ast.(Location.Indirect (Indirect.make ~disp:(Disp.Symbolic l) ()))
          ;;

          let indirect_abs_type (i : Ast.Indirect.t) =
            let open Abstract.Location in
            let open Ast.Indirect in
            match (seg i), (disp i), (base i), (index i) with
            (* Typically, [ EBP - i ] is a stack location: EBP is the
               frame pointer, and the x86 stack grows downwards. *)
            | None, Some (Ast.Disp.Numeric i), Some EBP, None ->
              StackOffset i
            (* This is the same as [ EBP - 0 ]. *)
            | None, None, Some ESP, None ->
              StackOffset 0
            (* This may be over-optimistic. *)
            | None, Some (Symbolic s), None, None ->
              Heap s
            | _, _, _, _ -> Unknown

          include Abstractable.Make (struct
              type nonrec t = t
              module Abs = Abstract.Location
              open Abs

              let abs_type = function
                | Ast.Location.Reg ESP
                | Reg EBP -> StackPointer
                | Reg _ -> GeneralRegister
                | Indirect i -> indirect_abs_type i
            end)
        end

        module Instruction = struct
          type t = Ast.Instruction.t
          let sexp_of_t = [%sexp_of: Ast.Instruction.t]
          let t_of_sexp = [%of_sexp: Ast.Instruction.t]

          type sym = Symbol.t
          type loc = Location.t

          let pp = P.pp_instruction

          let jump l =
            Ast.Instruction.make
              ~opcode:(Ast.OpJump None)
              ~operands:[ make_jump_operand l ]
              ()
          ;;

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

          let zero_operands (operands : Ast.Operand.t list)
            : Abstract.Operands.t =
            let open Abstract.Operands in
            if List.is_empty operands
            then None
            else Erroneous

          let src_dst_operands (operands : Ast.Operand.t list)
            : Abstract.Operands.t =
            let open Abstract.Operands in
            let open T in
            to_src_dst operands
            |> Option.value_map
              ~f:(function
                  | { src = Ast.Operand.Location s
                    ; dst = Location d
                    } ->
                    LocTransfer
                      { src = Location.abs_type s
                      ; dst = Location.abs_type d
                      }
                  | { src = Immediate (Ast.Disp.Numeric k)
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
                  | Ast.Operand.Location (Ast.Location.Indirect i) ->
                    begin
                      match Ast.Indirect.disp i with
                      | Some (Ast.Disp.Symbolic s) -> SymbolicJump s
                      | _ -> Unknown
                    end
                  | Ast.Operand.Immediate (Ast.Disp.Symbolic s) -> SymbolicJump s
                  | _ -> Unknown
                end
              | _ -> Erroneous
            )

          let abs_operands {Ast.Instruction.opcode; operands; _} =
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
                 (Ast.Instruction.make
                    ~opcode:(Ast.OpBasic `Nop)
                    ()
                 ));
            [%expect {| none |}]

          let%expect_test "abs_operands: jmp, AT&T style" =
            Format.printf "%a@."
              Abstract.Operands.pp
              (abs_operands
                 (Ast.Instruction.make
                    ~opcode:(Ast.OpJump None)
                    ~operands:
                      [ Ast.Operand.Location
                          (Ast.Location.Indirect
                             (Ast.Indirect.make
                                ~disp:(Ast.Disp.Symbolic "L1") ()))
                      ]
                    ()
                 ));
            [%expect {| jump->L1 |}]

          let%expect_test "abs_operands: nop $42 -> error" =
            Format.printf "%a@."
              Abstract.Operands.pp
              (abs_operands
                (Ast.Instruction.make
                   ~opcode:(Ast.OpBasic `Nop)
                   ~operands:[ Ast.Operand.Immediate
                                 (Ast.Disp.Numeric 42) ]
                   ()
                ));
            [%expect {| <invalid operands> |}]

          let%expect_test "abs_operands: mov %ESP, %EBP" =
            Format.printf "%a@."
              Abstract.Operands.pp
              (abs_operands
                 (Ast.Instruction.make
                    ~opcode:(Ast.OpBasic `Mov)
                    ~operands:[ Ast.Operand.Location (Ast.Location.Reg ESP)
                              ; Ast.Operand.Location (Ast.Location.Reg EBP)
                              ]
                    ()
              ));
            [%expect {| &stack -> &stack |}]

          let%expect_test "abs_operands: movl %ESP, %EBP" =
            Format.printf "%a@."
              Abstract.Operands.pp
              (abs_operands
                 (Ast.Instruction.make
                    ~opcode:(Ast.OpSized (`Mov, Ast.SLong))
                    ~operands:[ Ast.Operand.Location (Ast.Location.Reg ESP)
                              ; Ast.Operand.Location (Ast.Location.Reg EBP)
                              ]
                    ()
                 ));
            [%expect {| &stack -> &stack |}]

          include Abstractable.Make_enum (struct
              type nonrec t = t
              module Abs = Abstract.Instruction
              open Abs

              let abs_type ({opcode; _} : Ast.Instruction.t) =
                match opcode with
                | Ast.OpDirective _ ->
                  (* handled by abs_type below. *)
                  Other
                | OpJump _ -> Jump
                | OpBasic b -> basic_instruction_type b
                | OpSized (b, _) -> basic_instruction_type b
                | OpUnknown _ -> Unknown
            end)

          module OnSymbols = struct
            include Ast.Instruction.On_symbols
            module Elt = Symbol
          end
          module OnLocations = struct
            include Ast.Instruction.On_locations
            module Elt = Ast.Location
          end
        end

        module Statement = struct
          type sym = Symbol.t
          type t = Ast.Statement.t
          let sexp_of_t = [%sexp_of: Ast.Statement.t]
          let t_of_sexp = [%of_sexp: Ast.Statement.t]
          let pp = P.pp_statement

          type ins = Ast.Instruction.t

          let empty () = Ast.Statement.Nop
          let label s = Ast.Statement.Label s
          let instruction = Ast.Statement.instruction

          let abs_type =
            let open Abstract.Statement in
            function
            | Ast.Statement.Instruction { opcode = Ast.OpDirective s; _ } ->
              Directive s
            | Instruction i -> Instruction (Instruction.abs_type i)
            | Label l -> Label l
            | Nop -> Blank

          module OnSymbols = struct
            include Ast.Statement.On_symbols
            module Elt = Symbol
          end
          module OnInstructions = struct
            include Ast.Statement.On_instructions
            module Elt = Ast.Instruction
          end
        end

        module Constant = struct
          (* TODO: this is too weak *)
          include Ast.Operand

          let pp = P.pp_operand

          let zero = Ast.Operand.Immediate (Ast.Disp.Numeric 0)
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
       (Ast.Instruction.make
          ~opcode:(Ast.OpBasic `Add)
          ~operands:[ Ast.Operand.Immediate (Ast.Disp.Numeric (-16))
                    ; Ast.Operand.Location (Ast.Location.Reg ESP)
                    ]
          ()
       ));
  [%expect {| $-16 -> &stack |}]

module Intel = Make (Dialect.Intel) (PP.Intel)

let%expect_test "abs_operands: add ESP, -16, Intel" =
  Format.printf "%a@."
    Abstract.Operands.pp
    (Intel.Instruction.abs_operands
       (Ast.Instruction.make
          ~opcode:(Ast.OpBasic `Add)
          ~operands:[ Ast.Operand.Location (Ast.Location.Reg ESP)
                    ; Ast.Operand.Immediate (Ast.Disp.Numeric (-16))
                    ]
          ()
       ));
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
