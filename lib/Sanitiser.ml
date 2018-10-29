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
open Utils
open Utils.MyContainers

(*
 * Warnings
 *)

module type CustomWarnS = sig
  include Pretty_printer.S
end

module NoCustomWarn = struct
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
    | Operands of L.Instruction.t

  type body =
    | MissingEndLabel
    | UnknownElt of elt
    | ErroneousElt of elt
    | Custom of C.t

  type t =
    { body     : body
    ; progname : string
    }

  include Pretty_printer.S with type t := t
end

module Warn (L : Language.Intf) (C : CustomWarnS) = struct
  module L = L
  module C = C

  type elt =
    | Instruction of L.Instruction.t
    | Statement of L.Statement.t
    | Location of L.Location.t
    | Operands of L.Instruction.t

  type body =
    | MissingEndLabel
    | UnknownElt of elt
    | ErroneousElt of elt
    | Custom of C.t

  type t =
    { body     : body
    ; progname : string
    }

  let pp_elt f =
    function
    | Statement s -> L.Statement.pp f s
    | Instruction i | Operands i -> L.Instruction.pp f i
    | Location l -> L.Location.pp f l
  ;;

    let elt_type_name = function
      | Statement _ -> "statement"
      | Instruction _ -> "instruction"
      | Operands _ -> "the operands of instruction"
      | Location _ -> "location"
    ;;

  let pp_unknown_warning f elt =
    Format.fprintf f
      "act didn't understand@ %s@ %a.@ The litmus translation may be wrong."
      (elt_type_name elt)
      pp_elt elt

  let pp_erroneous_warning f elt =
    Format.fprintf f
      "act thinks@ %s@ %a@ is@ erroneous.@ The litmus translation may be wrong."
      (elt_type_name elt)
      pp_elt elt

  let pp_body f =
    function
    | MissingEndLabel ->
      String.pp f
        "act needed an end-of-program label here, but there wasn't one."
    | UnknownElt elt -> pp_unknown_warning f elt
    | ErroneousElt elt -> pp_erroneous_warning f elt
    | Custom c -> C.pp f c

  let pp f ent =
    Format.fprintf f "In program %s:@ " ent.progname;
    pp_body f ent.body
end

(*
 * Pass
 *)

module Pass = struct
  module M = struct
    type t =
      | LangHooks
      | MangleSymbols
      | RemoveBoundaries
      | RemoveLitmus
      | RemoveUseless
      | SimplifyLitmus
      | Warn
    [@@deriving enum, sexp]

    let table =
      [ LangHooks       , "lang-hooks"
      ; MangleSymbols   , "mangle-symbols"
      ; RemoveBoundaries, "remove-boundaries"
      ; RemoveLitmus    , "remove-litmus"
      ; RemoveUseless   , "remove-useless"
      ; SimplifyLitmus  , "simplify-litmus"
      ; Warn            , "warn"
      ]
  end

  include M
  include Enum.ExtendTable (M)

  (** [explain] collects passes that are useful for explaining an
     assembly file without modifying its semantics too much. *)
  let explain = Set.of_list [ RemoveUseless ]
end

let%expect_test "all passes accounted for" =
  Format.printf "@[<v>%a@]@."
    (Format.pp_print_list ~pp_sep:Format.pp_print_cut Pass.pp)
    (Pass.all_list ());
  [%expect {|
    lang-hooks
    mangle-symbols
    remove-boundaries
    remove-litmus
    remove-useless
    simplify-litmus
    warn |}]

(*
 * Context
 *)

module type CtxIntf = sig
  module Lang : Language.Intf
  module Warn : WarnIntf with module L = Lang

  type ctx =
    { progname : string
    ; proglen  : int
    ; endlabel : string option
    ; syms     : Abstract.Symbol.Table.t
    ; passes   : Pass.Set.t
    ; warnings : Warn.t list
    }

  include State.Intf with type state := ctx

  val initial : passes:Pass.Set.t -> progname:string -> proglen:int -> ctx

  val pass_enabled : Pass.t -> bool t
  val (|->) : Pass.t -> ('a -> 'a t) -> ('a -> 'a t)

  val warn_in_ctx : ctx -> Warn.body -> ctx
  val warn : Warn.body -> 'a -> 'a t

  val add_sym
    :  Abstract.Symbol.t
    -> Abstract.Symbol.Sort.t
    -> Abstract.Symbol.t t
  ;;

  val syms_with_sorts
    :  Abstract.Symbol.Sort.t list
    -> Abstract.Symbol.Set.t t

  val make_fresh_label : string -> string t
  val make_fresh_heap_loc : string -> string t
end

let freshen_label (syms : Abstract.Symbol.Set.t) (prefix : string) : string =
  let rec mu prefix count =
    let str = sprintf "%s%d" prefix count in
    if Abstract.Symbol.Set.mem syms str
    then mu prefix (count + 1)
    else str
  in
  mu prefix 0
;;

module CtxMake (L : Language.Intf) (C : CustomWarnS)
 : CtxIntf with module Lang = L and module Warn.C = C = struct
  module Lang = L
  module Warn = Warn (L) (C)

  type ctx =
    { progname : string
    ; proglen  : int
    ; endlabel : string option
    ; syms     : Abstract.Symbol.Table.t
    ; passes   : Pass.Set.t
    ; warnings : Warn.t list
    }[@@deriving fields]

  let initial ~passes ~progname ~proglen =
    { progname
    ; proglen
    ; endlabel = None
    ; syms     = Abstract.Symbol.Table.empty
    ; passes
    ; warnings = []
    }

  include State.Make (struct type state = ctx end)

  let pass_enabled pass =
    peek (fun ctx -> Pass.Set.mem ctx.passes pass)
  ;;

  let (|->) pass f a =
    make
      (fun ctx ->
         if Pass.Set.mem ctx.passes pass
         then run (f a) ctx
         else (ctx, a))
  ;;

  let warn_in_ctx ctx w =
    let ent = { Warn.progname = ctx.progname; body = w } in
    { ctx with warnings = ent::ctx.warnings }

  let warn (w : Warn.body) =
    modify (Fn.flip warn_in_ctx w)

  let add_sym sym sort =
    make
      (fun ctx ->
         { ctx with syms = Abstract.Symbol.Table.add ctx.syms sym sort }
         , sym
      )
  ;;

  let syms_with_sorts sorts =
    Abstract.Symbol.(
      let open Let_syntax in
      let%bind all_syms = peek (Field.get Fields_of_ctx.syms) in
      return (Table.set_of_sorts all_syms (Sort.Set.of_list sorts))
    )
  ;;

  let make_fresh_label prefix =
    Abstract.Symbol.(
      let open Let_syntax in
      let%bind syms = syms_with_sorts [ Sort.Jump; Sort.Label ] in
      let l = freshen_label syms prefix in
      add_sym l Sort.Label
    )
  ;;

  let make_fresh_heap_loc prefix =
    Abstract.Symbol.(
      let open Let_syntax in
      let%bind syms = syms_with_sorts [ Sort.Heap ] in
      let l = freshen_label syms prefix in
      add_sym l Sort.Heap
    )
  ;;
end

(*
 * Main sanitiser modules
 *)

module type Intf = sig
  module Warn : WarnIntf

  type statement

  module Output : sig
    type 'a t

    val result : 'a t -> 'a
    val warnings : 'a t -> Warn.t list
  end

  val sanitise
    :  ?passes:Pass.Set.t
    -> statement list
    -> statement list Output.t

  val split_and_sanitise
    :  ?passes:Pass.Set.t
    -> statement list
    -> statement list list Output.t
end

module type LangHookS = sig
  module L : Language.Intf
  module Ctx : CtxIntf with module Lang = L

  val on_program : L.Statement.t list -> (L.Statement.t list) Ctx.t
  val on_statement : L.Statement.t -> L.Statement.t Ctx.t
  val on_instruction : L.Instruction.t -> L.Instruction.t Ctx.t
  val on_location : L.Location.t -> L.Location.t Ctx.t
end

module NullLangHook (LS : Language.Intf) = struct
  module L = LS
  module Ctx = CtxMake (LS) (NoCustomWarn)

  let on_program = Ctx.return
  let on_statement = Ctx.return
  let on_instruction = Ctx.return
  let on_location = Ctx.return
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

module Make (LH : LangHookS)
  : Intf with type statement = LH.L.Statement.t = struct
  module Ctx = LH.Ctx
  module Warn = LH.Ctx.Warn

  type statement = LH.L.Statement.t

  module Output = struct
    type 'a t =
      { result   : 'a
      ; warnings : Warn.t list
      } [@@deriving fields]
  end

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

  let change_stack_to_heap ins =
    Ctx.peek
      (fun { progname; _ } ->
         let f ln =
           match LH.L.Location.abs_type ln with
           | Abstract.Location.StackOffset i ->
             LH.L.Location.make_heap_loc
               (sprintf "t%ss%d" progname i)
           | _ -> ln
         in
         LH.L.Instruction.OnLocations.map ~f ins)

  (** [warn_unknown_instructions stm] emits warnings for each
      instruction in [stm] without a high-level analysis. *)
  let warn_unknown_instructions ins =
    match LH.L.Instruction.abs_type ins with
     | Abstract.Instruction.Unknown ->
       Ctx.warn (Warn.UnknownElt (Warn.Instruction ins)) ins
     | _ -> Ctx.return ins

  (** [warn_operands stm] emits warnings for each instruction
     in [stm] whose operands don't have a high-level analysis,
      or are erroneous. *)
  let warn_operands ins =
    (* Don't emit warnings for unknown instructions---the
       upper warning should be enough. *)
    match LH.L.Instruction.abs_type ins with
    | Abstract.Instruction.Unknown ->
      Ctx.return ins
    | _ ->
      begin
        match LH.L.Instruction.abs_operands ins with
        | Abstract.Operands.Unknown ->
          Ctx.warn (Warn.UnknownElt (Warn.Operands ins)) ins
        | Abstract.Operands.Erroneous ->
          Ctx.warn (Warn.ErroneousElt (Warn.Operands ins)) ins
        | _ -> Ctx.return ins
      end
  ;;


  let change_ret_to_end_jump ins =
    Ctx.(
      match LH.L.Instruction.abs_type ins with
      | Abstract.Instruction.Return ->
        make
          (fun ctx ->
             match ctx.endlabel with
             | None ->
               (warn_in_ctx ctx Warn.MissingEndLabel), ins
             | Some endl ->
               ctx,
               LH.L.Instruction.jump endl
          )
      | _ -> return ins
    )

  (** [sanitise_loc] performs sanitisation at the single location
      level. *)
  let sanitise_loc loc =
    let open Ctx in
    return loc
    >>= (LangHooks      |-> LH.on_location)

  (** [sanitise_all_locs loc] iterates location sanitisation over
     every location in [loc], threading the context through
     monadically. *)
  let sanitise_all_locs ins =
    Ctx.make
      (fun ctx ->
         LH.L.Instruction.OnLocations.fold_map
           ~f:(fun ctx' loc ->
               Ctx.run (sanitise_loc loc)
                 ctx')
           ~init:ctx
           ins
      )

  (** [sanitise_ins] performs sanitisation at the single instruction
      level. *)
  let sanitise_ins ins =
    let open Ctx in
    return ins
    >>= (LangHooks      |-> LH.on_instruction)
    >>= (Warn           |-> warn_unknown_instructions)
    >>= (Warn           |-> warn_operands)
    >>= sanitise_all_locs
    >>= (SimplifyLitmus |-> change_ret_to_end_jump)
    >>= (SimplifyLitmus |-> change_stack_to_heap)

  (** [mangle_identifiers] reduces identifiers into a form that herd
      can parse. *)
  let mangle_identifiers stm =
    Ctx.return
      (LH.L.Statement.OnSymbols.map stm
         ~f:(LH.L.Symbol.OnStrings.map ~f:mangle))

  (** [warn_unknown_statements stm] emits warnings for each statement in
      [stm] without a high-level analysis. *)
  let warn_unknown_statements stm =
    match LH.L.Statement.abs_type stm with
    | Abstract.Statement.Other ->
      Ctx.warn (Warn.UnknownElt (Warn.Statement stm)) stm
    | _ -> Ctx.return stm

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
  let sanitise_stm _ stm =
    let open Ctx in
    return stm
    >>= (LangHooks |-> LH.on_statement)
    (* Do warnings after the language-specific hook has done any
       reduction necessary, but before we start making broad-brush
       changes to the statements. *)
    >>= (Warn |-> warn_unknown_statements)
    >>= sanitise_all_ins
    (* Do this last, in case the instruction sanitisers have
       introduced invalid identifiers. *)
    >>= (MangleSymbols |-> mangle_identifiers)

  let any (fs : ('a -> bool) list) (a : 'a) : bool =
    List.exists ~f:(fun f -> f a) fs

  (** [irrelevant_instruction_types] lists the high-level types of
      instruction that can be thrown out when converting to a litmus
      test. *)
  let irrelevant_instruction_types =
    Abstract.Instruction.(
      Set.of_list
        [ Call (* -not- Return: these need more subtle translation *)
        ; Stack
        ]
    )

  let instruction_is_irrelevant =
    LH.L.Statement.instruction_mem irrelevant_instruction_types

  (** [proglen_fix f prog] runs [f] on [prog] until the
      reported program length no longer changes. *)
  let proglen_fix f prog =
    let rec mu prog ctx =
      let (ctx', prog') = Ctx.run (f prog) ctx in
      if ctx.proglen = ctx'.proglen
      then (ctx', prog') (* Fixed point *)
      else mu prog' ctx'
    in
    Ctx.make (mu prog)

  (** [remove_generally_irrelevant_statements prog] completely removes
     statements in [prog] that have no use in general and cannot be
     rewritten. *)
  let remove_generally_irrelevant_statements prog =
    let open Ctx.Let_syntax in
    let%bind syms = Ctx.peek (fun ctx -> ctx.syms) in
    let%map remove_boundaries =
      Ctx.pass_enabled Pass.RemoveBoundaries
    in
    let ignore_boundaries = not remove_boundaries in
    let matchers =
      LH.L.Statement.(
        [ is_nop
        ; is_directive
        ; is_unused_label ~ignore_boundaries ~syms
        ])
    in
    MyList.exclude ~f:(any matchers) prog


  (** [remove_litmus_irrelevant_statements prog] completely removes
     statements in [prog] that have no use in Litmus and cannot be
     rewritten. *)
  let remove_litmus_irrelevant_statements prog =
    Ctx.return
      (let matchers =
         LH.L.Statement.(
           [ instruction_is_irrelevant
           ; is_stack_manipulation
           ])
       in
       MyList.exclude ~f:(any matchers) prog)

  let remove_useless_jumps prog =
    let rec mu skipped ctx =
      function
      | x::x'::xs when LH.L.Statement.is_jump_pair x x' ->
        let ctx' = Ctx.({ ctx with proglen = pred ctx.proglen }) in
        mu skipped ctx' (x'::xs)
      | x::x'::xs ->
        mu (x::skipped) ctx (x'::xs)
      | [] -> (ctx, List.rev skipped)
      | [x] -> (ctx, List.rev (x::skipped))
    in
    Ctx.make (Fn.flip (mu []) prog)

  let update_symbol_tables
      (prog : LH.L.Statement.t list) =
    Ctx.modify
      (fun ctx -> { ctx with syms = LH.L.symbols prog }
      )
      prog

  (** [add_end_label] adds an end-of-program label to the current
     program. *)
  let add_end_label (prog : LH.L.Statement.t list)
    : (LH.L.Statement.t list) Ctx.t =
    let open Ctx.Let_syntax in
    (* Don't generate duplicate endlabels! *)
    match%bind (Ctx.peek (fun ctx -> ctx.endlabel)) with
    | Some _ -> return prog
    | None ->
      let%bind progname = (Ctx.peek (fun ctx -> ctx.progname)) in
      let prefix = "END" ^ progname in
      let%bind lbl = Ctx.make_fresh_label prefix in
      Ctx.modify
        (fun ctx -> { ctx with endlabel = Some lbl })
        (prog @ [LH.L.Statement.label lbl])

  (** [remove_fix prog] performs a loop of statement-removing
     operations until we reach a fixed point in the program length. *)
  let remove_fix (prog : LH.L.Statement.t list)
    : LH.L.Statement.t list Ctx.t =
    let mu prog =
      Ctx.(
        return prog
        >>= update_symbol_tables
        >>= (RemoveUseless |-> remove_generally_irrelevant_statements)
        >>= (RemoveLitmus  |-> remove_litmus_irrelevant_statements)
        >>= (RemoveUseless |-> remove_useless_jumps)
      )
    in
    proglen_fix mu prog

  (** [sanitise_program] performs sanitisation on a single program. *)
  let sanitise_program
      (passes : Pass.Set.t)
      (i : int) (prog : LH.L.Statement.t list)
    : (LH.L.Statement.t list * Warn.t list) =
    let progname = sprintf "%d" i in
    let proglen = List.length prog in
    Ctx.(
      let ({ warnings; _ }, program) =
        run
          ((return prog)
           (* Initial table population. *)
           >>= update_symbol_tables
           >>= (SimplifyLitmus |-> add_end_label)
           >>= (LangHooks      |-> LH.on_program)
           (* The language hook might have invalidated the symbol
              tables. *)
           >>= update_symbol_tables
           (* Need to sanitise statements first, in case the
              sanitisation pass makes irrelevant statements
              (like jumps) relevant again. *)
           |> mapiM ~f:sanitise_stm
           >>= remove_fix
          )
          (initial ~passes ~progname ~proglen)
      in (program, warnings)
    )

  let sanitise ?passes prog =
    let passes' = Option.value ~default:(Pass.all_set ()) passes in
    let prog', warnlist = sanitise_program passes' 0 prog in
    Output.(
      { result   = prog'
      ; warnings = warnlist
      }
    )

  let sanitise_programs ?passes progs =
    let passes' = Option.value ~default:(Pass.all_set ()) passes in
    let progs', warnlists =
      progs
      |> List.mapi ~f:(sanitise_program passes')
      |> List.unzip
    in
    Output.(
      { result   = make_programs_uniform (LH.L.Statement.empty ()) progs'
      ; warnings = List.concat warnlists
      }
    )

  let split_and_sanitise ?passes stms = sanitise_programs ?passes (split_programs stms)
end
