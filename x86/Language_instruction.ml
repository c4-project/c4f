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
    : Abstract.Operand.Bundle.t =
    if List.is_empty operands
    then Abstract.Operand.Bundle.None
    else
      Single
        (Erroneous
          (Error.create_s
             [%message "Expected zero operands"
                 ~got:(operands : Ast.Operand.t list)]))
  ;;

  let error_to_erroneous = function
    | Result.Ok x -> x
    | Error e -> Abstract.Operand.(Bundle.Single (Erroneous e))
  ;;

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

  let classify_src_dst src dst classifiers =
    let open Or_error.Let_syntax in
    let%map (src', dst') = classify_double src dst classifiers in
    { Src_dst.src = src'; dst = dst' }
  ;;

  let single_operand operands ~allowed : Abstract.Operand.Bundle.t =
    error_to_erroneous
      (let open Or_error.Let_syntax in
       let%bind operand = My_list.one operands in
       let%map abs_operand = classify_single operand allowed in
       Abstract.Operand.Bundle.single abs_operand
      )
  ;;

  let src_dst_operands operands ~allowed =
    error_to_erroneous (
      let open Or_error.Let_syntax in
      let%bind { src ; dst } = Dialect.to_src_dst_or_error operands in
      let%map { src = src'; dst = dst' } =
        classify_src_dst src dst allowed
      in Abstract.Operand.Bundle.src_dst ~src:src' ~dst:dst'
    )

  let double_operands operands ~allowed =
    error_to_erroneous (
      let open Or_error.Let_syntax in
      let%bind (op1, op2) = My_list.two operands in
      let%map (abs1, abs2) = classify_double op1 op2 allowed in
      Abstract.Operand.Bundle.double abs1 abs2
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
    | Ast.Operand.Bop _ -> Some Abstract.Operand.Unknown
    | Immediate (Ast.Disp.Numeric k) -> Some (Int k)
    | Immediate (Ast.Disp.Symbolic s) -> Some (Symbol s)
    | Location _ | String _ | Typ _ -> None
  ;;

  let memory_operand = function
    | Ast.Operand.Bop _ -> Some Abstract.Operand.Unknown
    | Location (Ast.Location.Indirect _ as l)
      -> Some (Location (Location.abs_type l))
    | Location (Reg _)
    | Immediate _ | String _ | Typ _  -> None
  ;;

  let register_operand = function
    | Ast.Operand.Bop _ -> Some Abstract.Operand.Unknown
    | Location (Ast.Location.Reg _ as l)
      -> Some (Location (Location.abs_type l))
    | Location (Indirect _)
    | Immediate _ | String _ | Typ _  -> None
  ;;

  let jump_target_displacement = function
    | Some (Ast.Disp.Symbolic s) -> Abstract.Operand.Symbol s
    | _ -> Unknown
  ;;

  let jump_target_operand = function
    | Ast.Operand.Location (Ast.Location.Indirect i) ->
      Some (jump_target_displacement (Ast.Indirect.disp i))
    | Immediate (Ast.Disp.Symbolic s) -> Some (Symbol s)
    | Immediate (Numeric _) | Bop _ -> Some Unknown
    | String _ | Typ _ | Location (Reg _) -> None
  ;;

  let single_spec_to_classifier = function
    | Opcode.Operand_spec.Immediate -> immediate_operand
    | Memory -> memory_operand
    | Register -> register_operand
  ;;

  let symmetric_specs_to_classifiers specs =
    specs
    |> List.map ~f:(fun (s1, s2) ->
        ( single_spec_to_classifier s1
        , single_spec_to_classifier s2
        ))
    |> pairwise_symmetric
  ;;

  let src_dst_spec_to_classifier { Src_dst.src; dst } s d =
    Option.both
      (single_spec_to_classifier src s)
      (single_spec_to_classifier dst d)
  ;;

  let rec spec_to_classifier = function
    | Opcode.Operand_spec.Zero -> zero_operands
    | One specs ->
      let allowed = List.map ~f:single_spec_to_classifier specs in
      single_operand ~allowed
    | Symmetric specs ->
      let allowed = symmetric_specs_to_classifiers specs in
      double_operands ~allowed
    | Src_dst specs ->
      let allowed = List.map ~f:src_dst_spec_to_classifier specs in
      src_dst_operands ~allowed
    | Or (spec1, spec2) -> fun operands ->
      match spec_to_classifier spec1 operands with
      | Abstract.Operand.(Bundle.Single (Erroneous err1)) -> begin
          match spec_to_classifier spec2 operands with
          | Single (Erroneous err2) ->
            Single (Erroneous
              (Error.create_s
                 [%message
                   "Neither of the allowed operand types matched"
                     ~first_error:(err1 : Error.t)
                     ~second_error:(err2 : Error.t)
                 ]))
          | x -> x
        end
      | x -> x
  ;;

  let basic_operands_table =
    List.map Opcode.Basic.all
      ~f:(fun opcode ->
          let spec =
            opcode
            |> Opcode.Basic.get_operand_spec
            |> Option.value_map
              ~f:spec_to_classifier
              ~default:(Fn.const
                          Abstract.Operand.(Bundle.Single Unknown)
                       )
          in ( opcode, spec )
        )
  ;;

  let basic_operands
      (operands : Ast.Operand.t list)
      (opcode : [< Opcode.Basic.t]) =
    let classify =
      List.Assoc.find_exn basic_operands_table
        (opcode :> Opcode.Basic.t)
        ~equal:(Opcode.Basic.equal)
    in classify operands
  ;;

  let abs_operands {Ast.Instruction.opcode; operands; _} =
    match opcode with
    | Opcode.Basic b -> basic_operands operands b
    | Sized (b, _) -> basic_operands operands b
    | Jump _ ->
      single_operand operands ~allowed:[ jump_target_operand ]
    | Directive _ -> Abstract.Operand.(Bundle.Single Other)
    | Unknown _ -> Abstract.Operand.(Bundle.Single Unknown)
  ;;

  include Abstractable.Make (struct
      type nonrec t = t
      module Abs = Abstract.Instruction
      let abs_type ins =
        Abs.make
          ~opcode:(Opcode.abs_type ins.Ast.Instruction.opcode)
          ~operands:(abs_operands ins)
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
