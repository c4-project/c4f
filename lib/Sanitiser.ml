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
open Lang
open Utils
open Utils.MyContainers

module type WarnIntf = sig
  module L : Language.Intf

  type elt =
    | Instruction of L.Instruction.t
    | Statement of L.Statement.t
    | Location of L.Location.t

  type t =
    | UnknownElt of elt

  type entry =
    { body     : t
    ; progname : string option
    }
end

module Warn (LS : Language.Intf) : WarnIntf with module L = LS =
struct
  module L = LS

  type elt =
    | Instruction of L.Instruction.t
    | Statement of L.Statement.t
    | Location of L.Location.t

  type t =
    | UnknownElt of elt

  type entry =
    { body     : t
    ; progname : string option
    }
end

module type CtxIntf =
sig
  module Warn : WarnIntf

  type ctx =
    { progname : string option
    ; jsyms    : Language.SymSet.t
    ; warnings : Warn.entry list
    }

  type 'a t

  val initial : ctx

  val make : (ctx -> (ctx * 'a)) -> 'a t
  val peek : (ctx -> 'a) -> 'a t
  val modify : (ctx -> ctx) -> 'a -> 'a t
  val run : 'a t -> ctx -> (ctx * 'a)
  val run' : 'a t -> ctx -> 'a

  val warn : Warn.t -> 'a -> 'a t

  module Monad : Monad.S with type 'a t := 'a t
end

module Ctx (LS : Language.Intf) =
struct
  module Warn = Warn(LS)

  type ctx =
    { progname : string option
    ; jsyms    : Language.SymSet.t
    ; warnings : Warn.entry list
    }

  let initial =
    { progname = None
    ; jsyms    = Language.SymSet.empty
    ; warnings = []
    }

  type 'a t = ctx -> (ctx * 'a)

  let run = Fn.id
  let run' f ctx = f ctx |> snd
  let make = Fn.id
  let peek f ctx = ctx, f ctx
  let modify f a ctx = f ctx, a

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

  let warn (w : Warn.t) =
    modify
      (fun ctx ->
         let ent = { Warn.progname = ctx.progname; body = w } in
         { ctx with warnings = ent::ctx.warnings })
end

module type Intf = sig
  type statement

  val sanitise : statement list -> statement list list
end

module type LangHook =
sig
  module L : Language.Intf
  module C : CtxIntf with module Warn.L = L

  val on_program : L.Statement.t list -> (L.Statement.t list) C.t
  val on_statement : L.Statement.t -> L.Statement.t C.t
  val on_instruction : L.Instruction.t -> L.Instruction.t C.t
  val on_location : L.Location.t -> L.Location.t C.t
end

module NullLangHook (LS : Language.Intf) =
struct
  module L = LS
  module C = Ctx(LS)

  let on_program = C.Monad.return
  let on_statement = C.Monad.return
  let on_instruction = C.Monad.return
  let on_location = C.Monad.return
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

module T
    (LS   : Language.Intf)
    (LH   : LangHook with module L = LS) =
struct
  module Ctx = LH.C
  module Warn = LH.C.Warn

  let split_programs stms =
    (* Adding a nop to the start forces there to be some
       instructions before the first program, meaning we can
       simplify discarding such instructions. *)
    let progs =
      (LS.Statement.empty() :: stms)
      |> List.group ~break:(Fn.const LS.Statement.is_program_boundary)
    in
    List.drop progs 1
  (* TODO(MattWindsor91): divine the end of the program. *)

  let make_programs_uniform nop ps =
    MyList.right_pad ~padding:nop ps

  let bindL
      ~f
      (xs : ('a list) Ctx.t) :
    ('b list) Ctx.t =
    Ctx.Monad.Let_syntax.(
      xs
      >>= (List.fold_right
             ~init:(return [])
             ~f:(fun x m ->
                 let%bind x' = f x in
                 let%bind m' = m in
                 return (x'::m'))))

  let change_stack_to_heap ins =
    Ctx.peek
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
         LS.Instruction.OnLocations.map ~f ins)

  (** [warn_unknown_instructions stm] emits warnings for each
      instruction in [stm] without a high-level analysis. *)
  let warn_unknown_instructions ins =
    let open Ctx.Monad in
    match LS.Instruction.abs_type ins with
     | Language.AbsInstruction.Other ->
       Ctx.warn (Warn.UnknownElt (Warn.Instruction ins)) ins
     | _ -> return ins

  (** [sanitise_ins] performs sanitisation at the single instruction
      level. *)
  let sanitise_ins ins =
    let open Ctx.Monad in
    LH.on_instruction ins
    >>= warn_unknown_instructions
    >>= change_stack_to_heap

  (** [mangle_identifiers] reduces identifiers into a form that herd
      can parse. *)
  let mangle_identifiers stm =
    LS.Statement.OnSymbols.map ~f:mangle stm

  (** [warn_unknown_statements stm] emits warnings for each statement in
      [stm] without a high-level analysis. *)
  let warn_unknown_statements stm =
    let open Ctx.Monad in
    match LS.Statement.abs_type stm with
    | Language.AbsStatement.Other ->
      Ctx.warn (Warn.UnknownElt (Warn.Statement stm)) stm
    | _ -> return stm

  (** [sanitise_all_ins stm] iterates instruction sanitisation over
     every instruction in [stm], threading the context through
     monadically. *)
  let sanitise_all_ins stm =
    Ctx.make
      (fun ctx ->
         LS.Statement.OnInstructions.fold_map
           ~f:(fun ctx' ins ->
               Ctx.run (sanitise_ins ins)
                 ctx')
           ~init:ctx
           stm
      )

  (** [sanitise_stm] performs sanitisation at the single statement
      level. *)
  let sanitise_stm stm =
    let open Ctx.Monad in
    LH.on_statement stm
    (* Do warnings after the language-specific hook has done any
       reduction necessary, but before we start making broad-brush
       changes to the statements. *)
    >>= warn_unknown_statements
    >>= sanitise_all_ins
    (* Do this last, in case the instruction sanitisers have
       introduced invalid identifiers. *)
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
    Ctx.peek
      (fun { jsyms; _ } ->
         let matchers =
           LS.Statement.(
             [ instruction_is_irrelevant
             ; is_nop
             ; is_directive
             ; is_stack_manipulation
             ; is_unused_label ~jsyms
             ])
         in
         MyList.exclude ~f:(any matchers) prog)

  let update_ctx
      ?(progname : string option)
      (prog : LS.Statement.t list) =
    Ctx.modify
      (fun ctx ->
         ( { ctx with jsyms    = LS.jump_symbols prog
                    ; progname = ( Option.value_map
                                     ~f:Option.some
                                     ~default:ctx.progname
                                     progname )
           }
         ))
      prog

  let emit_unknown_warning f elt =
    let elt_type_name =
      Warn.(
        match elt with
        | Statement _ -> "statement"
        | Instruction _ -> "instruction"
        | Location _ -> "location"
      )
    in
    let pp_elt f =
      Warn.(
        function
        | Statement s -> LS.Statement.pp f s
        | Instruction i -> LS.Instruction.pp f i
        | Location l -> LS.Location.pp f l
      )
    in
    Format.fprintf f
      "act didn't understand@ %s@ %a.@,The litmus translation may be wrong."
      elt_type_name
      pp_elt elt

  let emit_warning ent =
    let f = Format.err_formatter in
    Format.pp_open_hbox f ();
    Format.fprintf f "Warning:@ ";
    Format.pp_open_hovbox f 0;
    Warn.(
      MyFormat.pp_option f
        ~pp:(fun f -> Format.fprintf f "In program %a:@ " String.pp)
        ent.progname;
      match ent.body with
      | UnknownElt elt -> emit_unknown_warning f elt
    );
    Format.pp_close_box f ();
    Format.pp_close_box f ();
    Format.pp_print_newline f ()

  let emit_warnings =
    (* TODO(@MattWindsor91): push these all the way through the program *)
    Ctx.modify
      (fun ctx ->
         List.iter ~f:emit_warning ctx.warnings;
         ctx)

  (** [sanitise_program] performs sanitisation on a single program. *)
  let sanitise_program (i : int) (prog : LS.Statement.t list)
    : LS.Statement.t list =
    let open Ctx.Monad in
    Ctx.run'
      ((return prog)
       >>= update_ctx ~progname:(sprintf "%d" i)
       >>= LH.on_program
       >>= remove_irrelevant_statements
       |>  bindL ~f:sanitise_stm
       >>= emit_warnings
      )
      Ctx.initial


  let sanitise_programs progs =
    progs
    |> List.mapi ~f:sanitise_program
    |> make_programs_uniform (LS.Statement.empty ())

  let sanitise stms = sanitise_programs (split_programs stms)
end

(* TODO(@MattWindsor91): should this move someplace else? *)

module X86 (DT : X86Dialect.Traits) =
struct
  open X86Ast

  module L = X86ATT.Lang
  module C = Ctx (X86ATT.Lang)

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
    | x -> x

  let on_statement = C.Monad.return

  let on_program = C.Monad.return

  let on_location = C.Monad.return

  let on_instruction stm =
    let open C.Monad in
    return stm
    >>| sub_to_add
end
