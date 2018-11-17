(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Core
open Lib
open Utils

module type S = sig
  include Dialect.S
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

module Make (T : Dialect.S) (P : PP.Printer) = struct
  include T
  include P

  module Basic = struct
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

      module On_strings = struct
        type t = string
        type elt = string
        include Singleton.With_elt (String)
      end
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
          ;;
        end)
    end

    module Instruction = Language_instruction.Make (struct
        module Dialect = T
        module Pretty = P
        module Symbol = Symbol
        module Location = Location
      end)

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

      module Abs = Abstract.Statement

      let abs_type = function
        | Ast.Statement.Instruction { opcode = Opcode.Directive s; _ } ->
          Abs.Directive s
        | Instruction i -> Instruction (Instruction.abs_type i)
        | Label l -> Label l
        | Nop -> Blank

      module On_symbols = struct
        include Ast.Statement.On_symbols
        module Elt = Symbol
      end
      module On_instructions = struct
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
  end

  include Language.Make (Basic)

  let make_jump_operand = Basic.Instruction.make_jump_operand
end

module Att = Make (Dialect.Att) (PP.Att)

let%expect_test "is_program_label: positive Mach-O example, AT&T" =
  printf "%b" (Att.Symbol.is_program_label "_P0");
  [%expect {| true |}]

let%expect_test "is_program_label: positive ELF example, AT&T" =
  printf "%b" (Att.Symbol.is_program_label "P0");
  [%expect {| true |}]

let%expect_test "is_program_label: wrong suffix, Mach-O, AT&T" =
  printf "%b" (Att.Symbol.is_program_label "_P0P");
  [%expect {| false |}]

let%expect_test "is_program_label: wrong suffix, ELF, AT&T" =
  printf "%b" (Att.Symbol.is_program_label "P0P");
  [%expect {| false |}]

let%expect_test "is_program_label: negative, AT&T" =
  printf "%b" (Att.Symbol.is_program_label "_P-1");
  [%expect {| false |}]

let%expect_test "abs_operands: add $-16, %ESP, AT&T" =
  Format.printf "%a@."
    Abstract.Operands.pp
    (Att.Instruction.abs_operands
       (Ast.Instruction.make
          ~opcode:(Opcode.Basic `Add)
          ~operands:[ Ast.Operand.Immediate (Ast.Disp.Numeric (-16))
                    ; Ast.Operand.Location (Ast.Location.Reg ESP)
                    ]
          ()
       ));
  [%expect {| $-16 -> &stack |}]

  let%expect_test "abs_operands: nop -> none" =
    Format.printf "%a@."
      Abstract.Operands.pp
      (Att.Instruction.abs_operands
         (Ast.Instruction.make
            ~opcode:(Opcode.Basic `Nop)
            ()
         ));
    [%expect {| none |}]

  let%expect_test "abs_operands: jmp, AT&T style" =
    Format.printf "%a@."
      Abstract.Operands.pp
      (Att.Instruction.abs_operands
         (Ast.Instruction.make
            ~opcode:(Opcode.Jump `Unconditional)
            ~operands:
              [ Ast.Operand.Location
                  (Ast.Location.Indirect
                     (Ast.Indirect.make
                        ~disp:(Ast.Disp.Symbolic "L1") ()))
              ]
            ()
         ));
    [%expect {| sym:L1 |}]

  let%expect_test "abs_operands: pop $42 -> error" =
    Format.printf "%a@."
      Abstract.Operands.pp
      (Att.Instruction.abs_operands
         (Ast.Instruction.make
            ~opcode:(Opcode.Basic `Pop)
            ~operands:
              [ Ast.Operand.Immediate (Ast.Disp.Numeric 42)
              ]
            ()
         ));
    [%expect {| <invalid operands> |}]

  let%expect_test "abs_operands: nop $42 -> error" =
    Format.printf "%a@."
      Abstract.Operands.pp
      (Att.Instruction.abs_operands
         (Ast.Instruction.make
            ~opcode:(Opcode.Basic `Nop)
            ~operands:[ Ast.Operand.Immediate
                          (Ast.Disp.Numeric 42) ]
            ()
         ));
    [%expect {| <invalid operands> |}]

  let%expect_test "abs_operands: mov %ESP, %EBP" =
    Format.printf "%a@."
      Abstract.Operands.pp
      (Att.Instruction.abs_operands
         (Ast.Instruction.make
            ~opcode:(Opcode.Basic `Mov)
            ~operands:[ Ast.Operand.Location (Ast.Location.Reg ESP)
                      ; Ast.Operand.Location (Ast.Location.Reg EBP)
                      ]
            ()
         ));
    [%expect {| &stack -> &stack |}]

  let%expect_test "abs_operands: movl %ESP, %EBP" =
    Format.printf "%a@."
      Abstract.Operands.pp
      (Att.Instruction.abs_operands
         (Ast.Instruction.make
            ~opcode:(Opcode.Sized (`Mov, Opcode.Size.Long))
            ~operands:[ Ast.Operand.Location (Ast.Location.Reg ESP)
                      ; Ast.Operand.Location (Ast.Location.Reg EBP)
                      ]
            ()
         ));
    [%expect {| &stack -> &stack |}]

module Intel = Make (Dialect.Intel) (PP.Intel)

let%expect_test "abs_operands: add ESP, -16, Intel" =
  Format.printf "%a@."
    Abstract.Operands.pp
    (Intel.Instruction.abs_operands
       (Ast.Instruction.make
          ~opcode:(Opcode.Basic `Add)
          ~operands:[ Ast.Operand.Location (Ast.Location.Reg ESP)
                    ; Ast.Operand.Immediate (Ast.Disp.Numeric (-16))
                    ]
          ()
       ));
  [%expect {| $-16 -> &stack |}]

let%expect_test "abs_operands: mov %ESP, $1, AT&T, should be error" =
  Format.printf "%a@."
    Abstract.Operands.pp
    (Att.Instruction.abs_operands
       (Ast.Instruction.make
          ~opcode:(Opcode.Basic `Mov)
          ~operands:[ Ast.Operand.Location (Ast.Location.Reg ESP)
                    ; Ast.Operand.Immediate (Ast.Disp.Numeric 1)
                    ]
          ()
       ));
  [%expect {| <invalid operands> |}]

module Herd7 = Make (Dialect.Herd7) (PP.Herd7)

let of_dialect = function
  | Dialect.Att   -> (module Att : S)
  | Dialect.Intel -> (module Intel : S)
  | Dialect.Herd7 -> (module Herd7 : S)
;;
