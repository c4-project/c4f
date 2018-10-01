(* This file is part of 'act'.

Copyright (c) 2018 by Matt Windsor

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Core
open Lang
open Utils.MyContainers

module type Intf = sig
  type statement

  val sanitise : statement list -> statement list list
end

module type LangHook = sig
  type statement

  val on_program : statement list -> statement list
  val on_statement : statement -> statement
end

module NullLangHook (LS : Language.Intf) =
  struct
    type statement = LS.Statement.t

    let on_program = Fn.id
    let on_statement = Fn.id
  end

let mangler =
  (* We could always just use something like Base36 here, but this
     seems a bit more human-readable. *)
  String.Escaping.escape_gen_exn
    ~escape_char:'Z'
    ~escapeworthy_map:[ '_', 'U'
                      ; '$', 'D'
                      ; '.', 'P'
                      ; 'Z', 'Z'
                      ]
let mangle ident =
  Staged.unstage mangler ident

let%expect_test "mangle: sample" =
  print_string (mangle "_foo$bar.BAZ");
  [%expect {| ZUfooZDbarZPBAZZ |}]

module T (LS : Language.Intf) (LH : LangHook with type statement = LS.Statement.t) =
  struct
    let remove_nops = MyList.exclude ~f:LS.Statement.is_nop
    let remove_directives = MyList.exclude ~f:LS.Statement.is_directive

    let warn (fmt : Format.formatter -> unit) =
      let f = Format.err_formatter in
      Format.pp_open_hbox f ();
      Format.fprintf f "Warning:@ ";
      Format.pp_open_hovbox f 0;
      fmt f;
      Format.pp_close_box f ();
      Format.pp_close_box f ();
      Format.pp_print_newline f ()

    let split_programs stms =
      (* Adding a nop to the start forces there to be some
         instructions before the first program, meaning we can
         simplify discarding such instructions. *)
      let progs =
        (LS.Statement.nop() :: stms)
        |> List.group ~break:(Fn.const LS.Statement.is_program_boundary)
      in
      List.drop progs 1
    (* TODO(MattWindsor91): divine the end of the program. *)

    let make_programs_uniform nop ps =
      let maxlen =
        ps
        |> (List.max_elt ~compare:(fun x y -> Int.compare (List.length x) (List.length y)))
        |> Option.value_map ~f:(List.length) ~default:0
      in
      List.map ~f:(fun p -> p @ List.init (maxlen - List.length p)
                                          ~f:(Fn.const nop))
               ps

    (** [mangle_identifiers] reduces identifiers into a form that herd
       can parse. *)
    let mangle_identifiers stm =
      LS.Statement.map_symbols ~f:mangle stm

    let make_unknown_warning stm stype f =
      Format.fprintf f
                     "act didn't understand@ %s@ %a.@,The litmus translation may be wrong."
                     stype
                     LS.Statement.pp stm

    (** [warn_unknown_statements stm] emits warnings for each statement in
        [stm] without a high-level analysis. *)
    let warn_unknown_statements stm =
      (match LS.Statement.abs_type stm with
       | Language.AbsStatement.Other ->
          warn (make_unknown_warning stm "statement")
       | _ -> ());
         stm

    (** [warn_unknown_instructions stm] emits warnings for each
       instruction in [stm] without a high-level analysis. *)
    let warn_unknown_instructions stm =
      (match LS.Statement.instruction_type stm with
       | Some Language.AbsInstruction.Other ->
          warn (make_unknown_warning stm "instruction")
       | _ -> ());
         stm


    (** [sanitise_stm] performs sanitisation at the single statement
       level. *)
    let sanitise_stm stm =
      stm
      |> LH.on_statement
      (* Do warnings after the language-specific hook has done any
         reduction necessary, but before we start making broad-brush
         changes to the statements. *)
      |> warn_unknown_statements
      |> warn_unknown_instructions
      |> mangle_identifiers

    (** [irrelevant_instruction_types] lists the high-level types of
       instruction that can be thrown out when converting to a litmus
       test. *)
    let irrelevant_instruction_types =
      let open Language.AbsInstruction in
      Set.of_list
        [ Call
        ; Stack
        ]

    let remove_irrelevant_instructions =
      MyList.exclude ~f:(LS.Statement.instruction_mem irrelevant_instruction_types)

    (** [remove_dead_labels prog] removes all labels in [prog] whose symbols
        aren't mentioned in jump instructions. *)
    let remove_dead_labels prog =
      let jsyms = LS.jump_symbols prog in
      MyList.exclude ~f:(LS.Statement.is_unused_label ~jsyms) prog

    (** [sanitise_program] performs sanitisation on a single program. *)
    let sanitise_program prog =
      prog
      |> LH.on_program
      |> remove_nops
      |> remove_directives
      |> remove_irrelevant_instructions
      |> remove_dead_labels
      |> List.map ~f:sanitise_stm

    let sanitise_programs progs =
      progs
      |> List.map ~f:sanitise_program
      |> make_programs_uniform (LS.Statement.nop ())

    let sanitise stms = sanitise_programs (split_programs stms)
  end

(* TODO(@MattWindsor91): should this move someplace else? *)

module X86 (DT : X86Dialect.Traits) =
  struct
    type statement = X86Ast.statement

    open X86Ast

    let negate = function
      | DispNumeric k -> OperandImmediate (DispNumeric (-k))
      | DispSymbolic s -> OperandBop ( OperandImmediate (DispNumeric 0)
                                     , BopMinus
                                     , OperandImmediate (DispSymbolic s)
                                     )

    let sub_to_add_ops : operand list -> operand list option =
      DT.bind_src_dst
        ~f:(function
            | {src = OperandImmediate s; dst} -> Some {src = negate s; dst}
            | _ -> None)

    let sub_to_add =
      function
      | StmInstruction si ->
         StmInstruction
           (match si with
            | { prefix; opcode = OpBasic `Sub; operands} as op ->
               Option.value_map
                 ~default:op
                 ~f:(fun ops' -> { prefix ; opcode = OpBasic `Add; operands = ops' })
                 (sub_to_add_ops operands)
            | { prefix; opcode = OpSized (`Sub, s); operands} as op ->
               Option.value_map
                 ~default:op
                 ~f:(fun ops' -> { prefix ; opcode = OpSized (`Add, s); operands = ops' })
                 (sub_to_add_ops operands)
            | _ -> si)
      | x -> x

    let on_statement stm =
      stm
      |> sub_to_add

    let on_program = Fn.id
  end
