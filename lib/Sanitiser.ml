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

include Sanitiser_intf

module Make_null_hook (Lang : Language.S)
  : Hook with module L = Lang = struct
  module L = Lang
  module Ctx = Sanitiser_ctx.Make (Sanitiser_ctx.Null_warn_hook (Lang))

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
;;
let mangle = Staged.unstage mangler

let%expect_test "mangle: sample" =
  print_string (mangle "_foo$bar.BAZ@lorem-ipsum+dolor,sit%amet");
  [%expect {| ZUfooZDbarZFBAZZZTloremZMipsumZAdolorZCsitZPamet |}]

module Make (B : Basic)
  : S with type statement = B.L.Statement.t
       and type sym = B.L.Symbol.t
       and type 'a Program_container.t = 'a B.Program_container.t = struct
  module Ctx = B.Ctx
  module Warn = B.Ctx.Warn
  module L = B.L

  module Program_container = B.Program_container

  (* Modules for building context-sensitive traversals over program
     containers and lists *)
  module Ctx_Pcon = Program_container.On_monad (Ctx)
  module Ctx_List = My_list.On_monad (Ctx)

  type statement = L.Statement.t
  type sym = L.Symbol.t

  module Output = struct
    type t =
      { result    : statement list Program_container.t
      ; warnings  : Warn.t list
      ; redirects : (sym, sym) List.Assoc.t
      } [@@deriving fields]
  end

  let make_programs_uniform nop ps =
    B.Program_container.right_pad ~padding:nop ps

  let change_stack_to_heap ins =
    let open Ctx.Let_syntax in
    let%map name = Ctx.get_prog_name in
    let f ln =
      match L.Location.abs_type ln with
      | Abstract.Location.StackOffset i ->
        L.Location.make_heap_loc
          (sprintf "t%ss%d" name i)
      | _ -> ln
    in L.Instruction.On_locations.map ~f ins
  ;;

  let if_instruction_has_type ins ty ~f =
    if L.Instruction.has_abs_type ty ins then f () else Ctx.return ins
  ;;

  (** [warn_unknown_instructions stm] emits warnings for each
      instruction in [stm] without a high-level analysis. *)
  let warn_unknown_instructions ins =
    if_instruction_has_type ins
     Abstract.Instruction.Unknown
     ~f:(Ctx.(
         fun () -> warn (Warn.UnknownElt (Warn.Instruction ins))
           >>| Fn.const ins))
  ;;

  let warn_unknown_operands ins abs_operands =
    if Abstract.Operands.is_part_unknown abs_operands
    then Ctx.warn (Warn.UnknownElt (Warn.Operands ins))
    else Ctx.return ()
  ;;

  let warn_erroneous_operands ins abs_operands =
    Ctx_List.iterM (Abstract.Operands.errors abs_operands)
      ~f:(fun error ->
          Ctx.warn (Warn.ErroneousElt (Warn.Operands ins, error))
        )
  ;;

  (** [warn_operands stm] emits warnings for each instruction
     in [stm] whose operands don't have a high-level analysis,
      or are erroneous. *)
  let warn_operands ins =
    (* Don't emit warnings for unknown instructions---the
       upper warning should be enough. *)
    match L.Instruction.abs_type ins with
    | Abstract.Instruction.Unknown -> Ctx.return ins
    | _ ->
      let open Ctx.Let_syntax in
      let abs_operands = L.Instruction.abs_operands ins in
      let%bind () = warn_unknown_operands ins abs_operands in
      let%map () = warn_erroneous_operands ins abs_operands in
      ins
  ;;

  (** [mangle_and_redirect sym] mangles [sym], either by
      generating and installing a new mangling into
      the redirects table if none already exists; or by
      fetching the existing mangle. *)
  let mangle_and_redirect sym =
    let open Ctx.Let_syntax in
    match%bind Ctx.get_redirect sym with
    | Some sym' when not (L.Symbol.equal sym sym') ->
      (* There's an existing redirect, so we assume it's a
         mangled version. *)
      Ctx.return sym'
    | Some _ | None ->
      let sym' = L.Symbol.On_strings.map ~f:mangle sym in
      Ctx.redirect ~src:sym ~dst:sym' >>| fun () -> sym'
  ;;

  let make_end_jump () =
    let open Ctx.Let_syntax in
    match%bind Ctx.get_end_label with
    | None ->
      Ctx.Monadic.return
        (Or_error.error_string
           "Tried to make an end-label jump without an end label")
    | Some endl ->
      return (L.Instruction.jump endl)

  let change_ret_to_end_jump ins =
    if_instruction_has_type ins Abstract.Instruction.Return
      ~f:make_end_jump
  ;;

  (** [sanitise_loc] performs sanitisation at the single location
      level. *)
  let sanitise_loc loc =
    let open Ctx in
    let open Sanitiser_pass in
    return loc
    >>= (LangHooks      |-> B.on_location)

  (** [sanitise_all_locs loc] iterates location sanitisation over
     every location in [loc], threading the context through
     monadically. *)
  let sanitise_all_locs =
    let module Loc = L.Instruction.On_locations.On_monad (Ctx) in
    Loc.mapM ~f:sanitise_loc
  ;;

  (** [sanitise_ins] performs sanitisation at the single instruction
      level. *)
  let sanitise_ins ins =
    let open Ctx in
    let open Sanitiser_pass in
    return ins
    >>= (LangHooks      |-> B.on_instruction)
    >>= (Warn           |-> warn_unknown_instructions)
    >>= (Warn           |-> warn_operands)
    >>= sanitise_all_locs
    >>= (SimplifyLitmus |-> change_ret_to_end_jump)
    >>= (SimplifyLitmus |-> change_stack_to_heap)
  ;;

  (** [mangle_identifiers progs] reduces identifiers across a program
      container [progs] into a form that herd can parse. *)
  let mangle_identifiers progs =
    let module Ctx_Stm_Sym = L.Statement.On_symbols.On_monad (Ctx) in
    (* Nested mapping:
       over symbols in statements in statement lists in programs. *)
    Ctx_Pcon.mapM progs
      ~f:(Ctx_List.mapM ~f:(Ctx_Stm_Sym.mapM ~f:mangle_and_redirect))
  ;;

  (** [warn_unknown_statements stm] emits warnings for each statement in
      [stm] without a high-level analysis. *)
  let warn_unknown_statements stm =
    let open Ctx.Let_syntax in
    match L.Statement.abs_type stm with
    | Abstract.Statement.Other ->
      let%map () = Ctx.warn (Warn.UnknownElt (Warn.Statement stm)) in stm
    | _ -> return stm

  (** [sanitise_all_ins stm] iterates instruction sanitisation over
     every instruction in [stm], threading the context through
     monadically. *)
  let sanitise_all_ins =
    let module L = L.Statement.On_instructions.On_monad (Ctx) in
    L.mapM ~f:sanitise_ins
  ;;

  (** [sanitise_stm] performs sanitisation at the single statement
      level. *)
  let sanitise_stm _ stm =
    let open Ctx in
    let open Sanitiser_pass in
    return stm
    >>= (LangHooks |-> B.on_statement)
    (* Do warnings after the language-specific hook has done any
       reduction necessary, but before we start making broad-brush
       changes to the statements. *)
    >>= (Warn |-> warn_unknown_statements)
    >>= sanitise_all_ins

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
    L.Statement.instruction_mem irrelevant_instruction_types

  let measure_ctx (measurement : 'a Ctx.t) (f : 'b -> 'c Ctx.t) (x : 'b)
    : ('a * 'c * 'a) Ctx.t =
    let open Ctx.Let_syntax in
    let%bind pre    = measurement in
    let%bind result = f x in
    let%map  post   = measurement in
    (pre, result, post)
  ;;

  (** [proglen_fix f prog] runs [f] on [prog] until the
      reported program length no longer changes. *)
  let proglen_fix f =
    Ctx.fix ~f:(
      fun mu prog ->
        let open Ctx.Let_syntax in
        let%bind (proglen, prog', proglen') =
          measure_ctx Ctx.get_prog_length f prog
        in
        if Int.equal proglen proglen'
        then Ctx.return prog'
        else mu prog'
    )
  ;;

  let remove_statements_in prog ~where =
    My_list.(exclude ~f:(any ~predicates:where)) prog
  ;;

  (** [remove_generally_irrelevant_statements prog] completely removes
     statements in [prog] that have no use in general and cannot be
     rewritten. *)
  let remove_generally_irrelevant_statements prog =
    let open Ctx.Let_syntax in
    let%bind syms = Ctx.get_symbol_table in
    let%map remove_boundaries =
      Ctx.is_pass_enabled Sanitiser_pass.RemoveBoundaries
    in
    let ignore_boundaries = not remove_boundaries in
      remove_statements_in prog
        ~where:L.Statement.
                 [ is_nop
                 ; is_directive
                 ; is_unused_label ~ignore_boundaries ~syms
                 ]
  ;;

  (** [remove_litmus_irrelevant_statements prog] completely removes
     statements in [prog] that have no use in Litmus and cannot be
     rewritten. *)
  let remove_litmus_irrelevant_statements prog =
    Ctx.return (
      remove_statements_in prog
        ~where:L.Statement.
                 [ instruction_is_irrelevant
                 ; is_stack_manipulation
                 ]
    )
  ;;

  let remove_useless_jumps prog =
    Ctx.(
      fix ~f:(
        fun mu (passed, remainder) ->
          match remainder with
          | x::x'::xs when L.Statement.is_jump_pair x x' ->
            dec_prog_length >>= fun () -> mu (passed, (x'::xs))
          | x::x'::xs ->
            mu (x::passed, (x'::xs))
          | [] -> return (List.rev passed, [])
          | [x] -> return (List.rev (x::passed), [])
      ) ([], prog)
      >>| fst
    )
  ;;

  let update_symbol_tables
      (prog : statement list) =
    let open Ctx.Let_syntax in
    let%map () = Ctx.set_symbol_table (L.symbols prog) in
    prog
  ;;

  (** [add_end_label] adds an end-of-program label to the current
     program. *)
  let add_end_label (prog : statement list)
    : (statement list) Ctx.t =
    let open Ctx.Let_syntax in
    (* Don't generate duplicate endlabels! *)
    match%bind Ctx.get_end_label with
    | Some _ -> return prog
    | None ->
      let%bind progname = Ctx.get_prog_name in
      let prefix = "END" ^ progname in
      let%bind lbl = Ctx.make_fresh_label prefix in
      let%map () = Ctx.set_end_label lbl in
      prog @ [L.Statement.label lbl]
  ;;

  (** [remove_fix prog] performs a loop of statement-removing
     operations until we reach a fixed point in the program length. *)
  let remove_fix (prog : L.Statement.t list)
    : L.Statement.t list Ctx.t =
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
      (i : int) (prog : L.Statement.t list)
    : (L.Statement.t list) Ctx.t =
    let name = sprintf "%d" i in
    Ctx.(
      enter_program ~name prog
      (* Initial table population. *)
      >>= update_symbol_tables
      >>= (SimplifyLitmus |-> add_end_label)
      >>= (LangHooks      |-> B.on_program)
      (* The language hook might have invalidated the symbol
         tables. *)
      >>= update_symbol_tables
      (* Need to sanitise statements first, in case the sanitisation
         pass makes irrelevant statements (like jumps) relevant
         again. *)
      >>= Ctx_List.mapiM ~f:sanitise_stm
      >>= remove_fix
    )
  ;;

  let all_symbols_in progs =
    progs
    |> Program_container.to_list
    |> List.concat_map
      ~f:(List.concat_map ~f:L.Statement.On_symbols.to_list)
  ;;

  let make_mangle_map progs =
    let all_symbols = all_symbols_in progs in
    (* Build a map src->dst, where each dst is a symbol in the
       assembly, and src is one possible demangling of dst.
       We're assuming that there'll only be one unique dst for
       each src, and taking the most recent dst. *)
    all_symbols
    |> List.concat_map ~f:(
      fun dst ->
        List.map (L.Symbol.abstract_demangle dst)
          ~f:(fun src -> (src, dst))
    )
    |> String.Map.of_alist_reduce ~f:(fun _ y -> y)
  ;;

  (** [find_initial_redirects symbols] tries to find the compiler-mangled
      version of each symbol in [symbols].  In each case, it sets up a
      redirect in the redirects table.

      If it fails to find at least one of the symbols, it'll raise a
      warning. *)
  let find_initial_redirects symbols progs =
    let open Ctx in
    if List.is_empty symbols
    then return ()
    else begin
      let mangle_map = make_mangle_map progs in
      Ctx_List.mapM symbols ~f:(
        fun src ->
          match String.Map.find mangle_map (L.Symbol.to_string src) with
          | Some dst -> Ctx.redirect ~src ~dst
          | None     -> Ctx.warn (Warn.SymbolRedirFail src)
      ) >>| (fun _ -> ())
    end
  ;;

  let sanitise_with_ctx symbols progs =
    let open Ctx.Let_syntax in
    let%bind () = find_initial_redirects symbols progs in
    let%bind progs' =
      Ctx_Pcon.mapiM ~f:sanitise_program progs
      (* We do this last, for two reasons: first, in case the
         instruction sanitisers have introduced invalid identifiers;
         and second, so that we know that the manglings agree across
         program boundaries.*)
      >>= Ctx.(MangleSymbols |-> mangle_identifiers)
    in
    let%bind warns = Ctx.take_warnings in
    let%map redirs = Ctx.get_redirect_alist symbols in
    Output.(
      { result = make_programs_uniform (L.Statement.empty ()) progs'
      ; warnings = warns
      ; redirects = redirs
      }
    )
  ;;

  let sanitise ?passes ?(symbols=[]) stms =
    let passes' = Option.value ~default:(Sanitiser_pass.all_set ()) passes in
    Ctx.(
      run
        (Monadic.return (B.split stms) >>= sanitise_with_ctx symbols)
        (initial ~passes:passes')
    )
  ;;
end

module Make_single (H : Hook) = Make(struct
    include H
    module Program_container = Utils.Singleton

    let split = Or_error.return (* no operation *)
  end)

module Make_multi (H : Hook) = Make(struct
    include H
    module Program_container = Utils.My_list

    let split stms =
      (* Adding a nop to the start forces there to be some
         instructions before the first program, meaning we can
         simplify discarding such instructions. *)
      let progs =
        (L.Statement.empty() :: stms)
        |> List.group ~break:(Fn.const L.Statement.is_program_boundary)
      in
      Or_error.return (List.drop progs 1)
      (* TODO(MattWindsor91): divine the end of the program. *)
  end)
