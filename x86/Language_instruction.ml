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

(* All expects tests for this module are attached to the concrete
   implementations of each x86 dialect's language module. *)

module type Basic = sig
  module Dialect : Dialect.S
  module Pretty : PP.Printer
  module Symbol : Lib.Language_symbol.Basic with type t := string
  module Location : Lib.Language.Basic_location with type t := Ast.Location.t
end

module type S = sig
  include Lib.Language.Basic_instruction with type t = Ast.Instruction.t
                                          and type sym = string
                                          and type loc = Ast.Location.t

  val make_jump_operand : string -> Ast.Operand.t
end

module Make (B : Basic) : S = struct
  include B
  include (Ast.Instruction : Sexpable.S with type t = Ast.Instruction.t)

  type sym = string
  type loc = Ast.Location.t

  let make_jump_operand jsym =
    Ast.(
      let disp = Disp.Symbolic jsym in
      match Dialect.symbolic_jump_type with
      | `Indirect ->
        Operand.Location (Location.Indirect (Indirect.make ~disp ()))
      | `Immediate ->
        Operand.Immediate disp
    )
  ;;

  let pp = B.Pretty.pp_instruction

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
      let%bind { src; dst } = Dialect.to_src_dst_or_error operands in
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
