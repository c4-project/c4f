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
              ~opcode:(Opcode.Jump `Unconditional)
              ~operands:[ make_jump_operand l ]
              ()
          ;;

          let zero_operands (operands : Ast.Operand.t list)
            : Abstract.Operands.t =
            if List.is_empty operands
            then `None
            else `Erroneous
                (Error.create_s
                   [%message "Expected zero operands"
                       ~got:(operands : Ast.Operand.t list)]
                )
          ;;

          let src_operand
            : Ast.Operand.t -> Abstract.Operands.src Or_error.t = function
            | Ast.Operand.Location s ->
              Ok (`Location (Location.abs_type s))
            | Immediate (Ast.Disp.Numeric k) -> Ok (`Int k)
            | Immediate (Ast.Disp.Symbolic s) -> Ok (`Symbol s)
            | String _ | Typ _ | Bop _ -> Ok `Other
          ;;

          let dst_operand
            : Ast.Operand.t -> Abstract.Operands.dst Or_error.t = function
            | Ast.Operand.Location s ->
              Ok (`Location (Location.abs_type s))
            | Immediate k ->
              Or_error.error_s
                [%message "Immediate values can't be destinations"
                    ~immediate:(k : Ast.Disp.t)
                ]
            | String _ | Typ _ | Bop _ -> Ok `Other
          ;;

          let error_to_erroneous = function
            | Result.Ok x -> x
            | Error e -> `Erroneous e
          ;;

          let src_dst_operands (operands : Ast.Operand.t list)
            : Abstract.Operands.t =
            error_to_erroneous (
              let open Or_error.Let_syntax in
              let%bind { src; dst } = T.to_src_dst_or_error operands in
              let%map src' = src_operand src
              and     dst' = dst_operand dst
              in
              Abstract.Operands.src_dst ~src:src' ~dst:dst'
            )

          let classify_single operand classifiers =
            classifiers
            |> List.find_map ~f:(fun c -> c operand)
            |> Result.of_option
                 ~error:(Error.of_string "Operand type not allowed here")
          ;;

          let classify_double op1 op2 classifiers =
            classifiers
            |> List.find_map ~f:(fun c -> c op1 op2)
            |> Result.of_option
              ~error:(Error.of_string "Operand types not allowed here")
          ;;

          let single_operand operands ~allowed : Abstract.Operands.t =
            error_to_erroneous
              (let open Or_error.Let_syntax in
               let%bind operand = My_list.one operands in
               let%map abs_operand = classify_single operand allowed in
               Abstract.Operands.single abs_operand
              )
          ;;

          let double_operands operands ~allowed =
            error_to_erroneous
              (let open Or_error.Let_syntax in
               let%bind (op1, op2) = My_list.two operands in
               let%map (abs1, abs2) = classify_double op1 op2 allowed in
               Abstract.Operands.double abs1 abs2
              )
          ;;

          (** [pairwise_symmetric classifier_pairs] builds a list of
             operand classifiers that permits pairs of operands ([op1],
             [op2]) for which some [(c1, c2)] exists in
             [classifier_pairs] where either [op1] satisfies [c1] and
             [op2] satisfies [c2], or [op1] satisfies [c2] and [op2]
             satisfies [c1]. *)
          let pairwise_symmetric =
            List.concat_map
              ~f:(fun (c1, c2) ->
                  [ (fun op1 op2 -> Option.both (c1 op1) (c2 op2))
                  ; (fun op1 op2 -> Option.both (c2 op1) (c1 op2))
                  ])
          ;;

          let immediate_operand = function
            | Ast.Operand.Bop _ -> Some `Unknown
            | Immediate (Ast.Disp.Numeric k) -> Some (`Int k)
            | Immediate (Ast.Disp.Symbolic s) -> Some (`Symbol s)
            | Location _ | String _ | Typ _ -> None
          ;;

          let memory_operand = function
            | Ast.Operand.Bop _ -> Some `Unknown
            | Location (Ast.Location.Indirect _ as l)
              -> Some (`Location (Location.abs_type l))
            | Location (Reg _)
            | Immediate _ | String _ | Typ _  -> None
          ;;

          let register_operand = function
            | Ast.Operand.Bop _ -> Some `Unknown
            | Location (Ast.Location.Reg _ as l)
              -> Some (`Location (Location.abs_type l))
            | Location (Indirect _)
            | Immediate _ | String _ | Typ _  -> None
          ;;

          let jump_target_operand = function
            | Ast.Operand.Location (Ast.Location.Indirect i) ->
              begin
                match Ast.Indirect.disp i with
                | Some (Ast.Disp.Symbolic s) -> Some (`Symbol s)
                | _ -> Some `Unknown
              end
            | Immediate (Ast.Disp.Symbolic s) -> Some (`Symbol s)
            | Immediate (Numeric _) | Bop _ -> Some `Unknown
            | String _ | Typ _ | Location (Reg _) -> None
          ;;

          let basic_operands (operands : Ast.Operand.t list)
            : [< Opcode.Basic.t] -> Abstract.Operands.t = function
            | `Leave
            | `Mfence
            | `Nop
            | `Ret -> zero_operands operands
            | `Add
            | `Cmp
              (* Though the reference manual describes CMP as
                 having two source operands, the encoding only
                 allows immediate values in destination
                 position, so we return it as a src/dst. *)
            | `Sub
            | `Mov
            | `Xor -> src_dst_operands operands
            | `Push ->
              single_operand operands
                ~allowed:[ immediate_operand
                         ; memory_operand
                         ; register_operand
                         ]
            | `Pop ->
              single_operand operands
                ~allowed:[ memory_operand
                         ; register_operand
                         ]
            | `Xchg ->
              (* Though the reference manual describes XCHG as
                 having a source and destination operand, the two
                 operands are symmetrical in the encoding, and
                 both only accept destinations. *)
              double_operands operands
                ~allowed:(pairwise_symmetric
                            [ (memory_operand  , register_operand)
                            ; (register_operand, register_operand)
                            ])
            (* TODO(@MattWindsor91): analyse other opcodes! *)
            | `Call
            | `Cmpxchg -> `Unknown
          ;;

          let abs_operands {Ast.Instruction.opcode; operands; _} =
            match opcode with
            | Opcode.Basic b -> basic_operands operands b
            | Opcode.Sized (b, _) -> basic_operands operands b
            | Opcode.Jump _ ->
              single_operand operands ~allowed:[ jump_target_operand ]
            | Opcode.Directive _ -> `Other
            | Opcode.Unknown _ -> `Unknown
          ;;

          let%expect_test "abs_operands: nop -> none" =
            Format.printf "%a@."
              Abstract.Operands.pp
              (abs_operands
                 (Ast.Instruction.make
                    ~opcode:(Opcode.Basic `Nop)
                    ()
                 ));
            [%expect {| none |}]

          let%expect_test "abs_operands: jmp, AT&T style" =
            Format.printf "%a@."
              Abstract.Operands.pp
              (abs_operands
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
              (abs_operands
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
              (abs_operands
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
              (abs_operands
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
              (abs_operands
                 (Ast.Instruction.make
                    ~opcode:(Opcode.Sized (`Mov, Opcode.Size.Long))
                    ~operands:[ Ast.Operand.Location (Ast.Location.Reg ESP)
                              ; Ast.Operand.Location (Ast.Location.Reg EBP)
                              ]
                    ()
                 ));
            [%expect {| &stack -> &stack |}]

          include Abstractable.Make_enum (struct
              type nonrec t = t
              module Abs = Abstract.Instruction
              let abs_type ({opcode; _} : Ast.Instruction.t) =
                Opcode.abs_type opcode
            end)

          module On_symbols = struct
            include Ast.Instruction.On_symbols
            module Elt = Symbol
          end
          module On_locations = struct
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
            | Ast.Statement.Instruction { opcode = Opcode.Directive s; _ } ->
              Directive s
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
      end)
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
