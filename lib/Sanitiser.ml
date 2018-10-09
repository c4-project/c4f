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

(*
 * Warnings
 *)

module type CustomWarnS =
sig
  include Pretty_printer.S
end

module NoCustomWarn =
struct
  (* No warnings possible *)
  type t
  let pp _ _ = ()
end

module type WarnIntf = sig
  module L : Language.Intf
  module C : CustomWarnS

  type elt =
    | Instruction of L.Instruction.t
    | Statement of L.Statement.t
    | Location of L.Location.t

  type body =
    | UnknownElt of elt
    | Custom of C.t

  type t =
    { body     : body
    ; progname : string option
    }

  include Pretty_printer.S with type t := t
end

module Warn (L : Language.Intf) (C : CustomWarnS) =
struct
  module L = L
  module C = C

  type elt =
    | Instruction of L.Instruction.t
    | Statement of L.Statement.t
    | Location of L.Location.t

  type body =
    | UnknownElt of elt
    | Custom of C.t

  type t =
    { body     : body
    ; progname : string option
    }

  let pp_elt f =
    function
    | Statement s -> L.Statement.pp f s
    | Instruction i -> L.Instruction.pp f i
    | Location l -> L.Location.pp f l

  let pp_unknown_warning f elt =
    let elt_type_name =
      match elt with
      | Statement _ -> "statement"
      | Instruction _ -> "instruction"
      | Location _ -> "location"
    in
    Format.fprintf f
      "act didn't understand@ %s@ %a.@,The litmus translation may be wrong."
      elt_type_name
      pp_elt elt

  let pp_body f =
    function
    | UnknownElt elt -> pp_unknown_warning f elt
    | Custom c -> C.pp f c

  let pp f ent =
    MyFormat.pp_option f
      ~pp:(fun f -> Format.fprintf f "In program %a:@ " String.pp)
      ent.progname;
    pp_body f ent.body
end

(*
 * Context
 *)

module type CtxIntf =
sig
  module Warn : WarnIntf

  type ctx =
    { progname : string option
    ; hsyms    : Language.SymSet.t
    ; jsyms    : Language.SymSet.t
    ; warnings : Warn.t list
    }

  type 'a t

  val initial : ctx

  val make : (ctx -> (ctx * 'a)) -> 'a t
  val peek : (ctx -> 'a) -> 'a t
  val modify : (ctx -> ctx) -> 'a -> 'a t
  val run : 'a t -> ctx -> (ctx * 'a)
  val run' : 'a t -> ctx -> 'a

  val warn : Warn.body -> 'a -> 'a t

  module Monad : Monad.S with type 'a t := 'a t
end

(*
 * Language-dependent parts
 *)

module CtxMake (L : Language.Intf) (C : CustomWarnS) =
struct
  module Warn = Warn (L) (C)

  type ctx =
    { progname : string option
    ; hsyms    : Language.SymSet.t
    ; jsyms    : Language.SymSet.t
    ; warnings : Warn.t list
    }

  let initial =
    { progname = None
    ; hsyms    = Language.SymSet.empty
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

  let warn (w : Warn.body) =
    modify
      (fun ctx ->
         let ent = { Warn.progname = ctx.progname; body = w } in
         { ctx with warnings = ent::ctx.warnings })
end

(*
 * Main sanitiser modules
 *)

module type Intf = sig
  module Warn : WarnIntf

  type statement

  type output =
    { programs : statement list list
    ; warnings : Warn.t list
    }

  val sanitise : statement list -> output
end

module type LangHookS =
sig
  module L : Language.Intf
  module Ctx : CtxIntf with module Warn.L = L

  val on_program : L.Statement.t list -> (L.Statement.t list) Ctx.t
  val on_statement : L.Statement.t -> L.Statement.t Ctx.t
  val on_instruction : L.Instruction.t -> L.Instruction.t Ctx.t
  val on_location : L.Location.t -> L.Location.t Ctx.t
end

module NullLangHook (LS : Language.Intf) =
struct
  module L = LS
  module Ctx = CtxMake (LS) (NoCustomWarn)

  let on_program = Ctx.Monad.return
  let on_statement = Ctx.Monad.return
  let on_instruction = Ctx.Monad.return
  let on_location = Ctx.Monad.return
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
                      ; '@', 'T' (* aT *)
                      ; '_', 'U' (* Underscore *)
                      ; 'Z', 'Z' (* Z *)
                      ]
let mangle ident =
  Staged.unstage mangler ident

let%expect_test "mangle: sample" =
  print_string (mangle "_foo$bar.BAZ@lorem-ipsum+dolor,sit%amet");
  [%expect {| ZUfooZDbarZFBAZZZTloremZMipsumZAdolorZCsitZPamet |}]

module Make (LH : LangHookS) =
struct
  module Ctx = LH.Ctx
  module Warn = LH.Ctx.Warn

  type statement = LH.L.Statement.t

  type output =
    { programs : statement list list
    ; warnings : Warn.t list
    }

  let split_programs stms =
    (* Adding a nop to the start forces there to be some
       instructions before the first program, meaning we can
       simplify discarding such instructions. *)
    let progs =
      (LH.L.Statement.empty() :: stms)
      |> List.group ~break:(Fn.const LH.L.Statement.is_program_boundary)
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
           match LH.L.Location.abs_type ln with
           | Language.AbsLocation.StackOffset i ->
             LH.L.Location.make_heap_loc
               (sprintf "t%ss%d"
                  (Option.value ~default:"?" progname)
                  i)
           | _ -> ln
         in
         LH.L.Instruction.OnLocations.map ~f ins)

  (** [warn_unknown_instructions stm] emits warnings for each
      instruction in [stm] without a high-level analysis. *)
  let warn_unknown_instructions ins =
    let open Ctx.Monad in
    match LH.L.Instruction.abs_type ins with
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
    LH.L.Statement.OnSymbols.map ~f:mangle stm

  (** [warn_unknown_statements stm] emits warnings for each statement in
      [stm] without a high-level analysis. *)
  let warn_unknown_statements stm =
    let open Ctx.Monad in
    match LH.L.Statement.abs_type stm with
    | Language.AbsStatement.Other ->
      Ctx.warn (Warn.UnknownElt (Warn.Statement stm)) stm
    | _ -> return stm

  (** [sanitise_all_ins stm] iterates instruction sanitisation over
     every instruction in [stm], threading the context through
     monadically. *)
  let sanitise_all_ins stm =
    Ctx.make
      (fun ctx ->
         LH.L.Statement.OnInstructions.fold_map
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
    LH.L.Statement.instruction_mem irrelevant_instruction_types

  (** [remove_irrelevant_statements prog] completely removes
      statements in [prog] that have no use in Litmus and cannot be
      rewritten. *)
  let remove_irrelevant_statements prog =
    Ctx.peek
      (fun { jsyms; _ } ->
         let matchers =
           LH.L.Statement.(
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
      (prog : LH.L.Statement.t list) =
    Ctx.modify
      (fun ctx ->
         ( { ctx with
             hsyms    = LH.L.heap_symbols prog
           ; jsyms    = LH.L.jump_symbols prog
           ; progname = ( Option.value_map
                            ~f:Option.some
                            ~default:ctx.progname
                            progname )
           }
         ))
      prog

  (** [sanitise_program] performs sanitisation on a single program. *)
  let sanitise_program (i : int) (prog : LH.L.Statement.t list)
    : (LH.L.Statement.t list * Warn.t list) =
    let open Ctx.Monad in
    let ({ Ctx.warnings; _ }, program) =
      Ctx.run
        ((return prog)
         >>= update_ctx ~progname:(sprintf "%d" i)
         >>= LH.on_program
         >>= remove_irrelevant_statements
         |>  bindL ~f:sanitise_stm
        )
        Ctx.initial
    in (program, warnings)

  let sanitise_programs progs =
    let progs', warnlists =
      progs
      |> List.mapi ~f:sanitise_program
      |> List.unzip
    in
    { programs = make_programs_uniform (LH.L.Statement.empty ()) progs'
    ; warnings = List.concat warnlists
    }

  let sanitise stms = sanitise_programs (split_programs stms)
end
