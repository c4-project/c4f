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

type ctx =
  { progname : string option
  ; jsyms    : Language.SymSet.t
  }

let initial_ctx =
  { progname = None
  ; jsyms    = Language.SymSet.empty
  }

module WithCtx =
struct
  type 'a t = ctx -> (ctx * 'a)

  let run = Fn.id
  let run' f ctx = f ctx |> snd
  let make = Fn.id
  let peek f ctx = ctx, f ctx

  module Monad = Monad.Make (
    struct
      type nonrec 'a t = 'a t

      let map' wc ~f =
        fun ctx ->
          let (ctx', a) = wc ctx in
          (ctx', f a)
      let map = `Custom map'
      let bind wc ~f =
        fun ctx ->
          let (ctx', a) = wc ctx in
          (f a) ctx'
      let return a = fun initial_ctx -> (initial_ctx, a)
    end
    )
end


module type Intf = sig
  type statement

  val sanitise : statement list -> statement list list
end

module type LangHook = sig
  type statement
  type location

  val on_program :
    (statement list) ->
    (statement list) WithCtx.t

  val on_statement :
    statement ->
    statement WithCtx.t

  val on_location :
    location ->
    location WithCtx.t
end

module NullLangHook (LS : Language.Intf) =
struct
  type statement = LS.Statement.t
  type location = LS.Location.t

  let on_program = WithCtx.Monad.return
  let on_statement = WithCtx.Monad.return
  let on_location = WithCtx.Monad.return
end

let mangler =
  (* We could always just use something like Base36 here, but this
     seems a bit more human-readable. *)
  String.Escaping.escape_gen_exn
    ~escape_char:'Z'
    (* We escape some things that couldn't possibly appear in legal
       x86 assembler, but _might_ be generated during sanitisation. *)
    ~escapeworthy_map:[ '+', 'A' (* Add *)
                      ; ',', 'C' (* Comma *)
                      ; '$', 'D' (* Dollar *)
                      ; '.', 'F' (* Full stop *)
                      ; '-', 'M' (* Minus *)
                      ; '%', 'P' (* Percent *)
                      ; '_', 'U' (* Underscore *)
                      ; 'Z', 'Z' (* Z *)
                      ]
let mangle ident =
  Staged.unstage mangler ident

let%expect_test "mangle: sample" =
  print_string (mangle "_foo$bar.BAZ");
  [%expect {| ZUfooZDbarZPBAZZ |}]

module T (LS : Language.Intf) (LH : LangHook with type statement = LS.Statement.t) =
  struct
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
      MyList.right_pad ~padding:nop ps

    let change_stack_to_heap stm =
      WithCtx.peek
        (fun { progname; _ } ->
           let f ln =
             match LS.Location.abs_type ln with
             | Language.AbsLocation.StackOffset i ->
               LS.Location.make_heap_loc
                 (sprintf "t%ss%d"
                    (Option.value ~default:"?" progname)
                    i)
                 | _ -> ln
           in
           LS.Statement.map_locations ~f stm)

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
      let open WithCtx.Monad in
      LH.on_statement stm
      >>= change_stack_to_heap
      (* Do warnings after the language-specific hook has done any
         reduction necessary, but before we start making broad-brush
         changes to the statements. *)
      >>| warn_unknown_statements
      >>| warn_unknown_instructions
      >>| mangle_identifiers

    let any (fs : ('a -> bool) list) (a : 'a) : bool =
      List.exists ~f:(fun f -> f a) fs

    (** [irrelevant_instruction_types] lists the high-level types of
       instruction that can be thrown out when converting to a litmus
       test. *)
    let irrelevant_instruction_types =
      let open Language.AbsInstruction in
      Set.of_list
        [ Call
        ; Stack
        ]

    let instruction_is_irrelevant =
      LS.Statement.instruction_mem irrelevant_instruction_types

    (** [remove_irrelevant_statements prog] completely removes
       statements in [prog] that have no use in Litmus and cannot be
       rewritten. *)
    let remove_irrelevant_statements prog =
      WithCtx.peek
        (fun { jsyms; _ } ->
           let matchers =
             [ instruction_is_irrelevant
             ; LS.Statement.is_nop
             ; LS.Statement.is_directive
             ; LS.Statement.is_stack_manipulation
             ; LS.Statement.is_unused_label ~jsyms
             ]
           in
           MyList.exclude ~f:(any matchers) prog)

    let update_ctx
        (prog : LS.Statement.t list)
        ?(progname : string option) =
      WithCtx.run
        (fun ctx ->
           ( { jsyms    = LS.jump_symbols prog
             ; progname = ( Option.value_map
                              ~f:Option.some
                              ~default:ctx.progname
                              progname )
             }
           , prog
           ))

    let bindL
      ~f
      (xs : ('a list) WithCtx.t) :
      ('b list) WithCtx.t =
      let open WithCtx.Monad.Let_syntax in
      xs
      >>= (List.fold_right
            ~init:(return [])
            ~f:(fun x m ->
                let%bind x' = f x in
                let%bind m' = m in
                return (x'::m')))


    (** [sanitise_program] performs sanitisation on a single program. *)
    let sanitise_program (i : int) (prog : LS.Statement.t list)
      : LS.Statement.t list =
      let open WithCtx.Monad in
      WithCtx.run'
        ((return prog)
         >>= update_ctx ~progname:(sprintf "%d" i)
         >>= LH.on_program
         >>= remove_irrelevant_statements
         |>  bindL ~f:sanitise_stm
        )
        initial_ctx


    let sanitise_programs progs =
      progs
      |> List.mapi ~f:sanitise_program
      |> make_programs_uniform (LS.Statement.nop ())

    let sanitise stms = sanitise_programs (split_programs stms)
  end

(* TODO(@MattWindsor91): should this move someplace else? *)

module X86 (DT : X86Dialect.Traits) =
struct
  open X86Ast

  type nonrec statement = statement
  type nonrec location = location

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
    let open WithCtx.Monad in
    return stm
    >>| sub_to_add

  let on_program = WithCtx.Monad.return

  let on_location = WithCtx.Monad.return
end
