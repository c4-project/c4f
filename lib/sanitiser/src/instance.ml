(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Core_kernel
open Travesty_containers
module Tx = Travesty_core_kernel_exts
include Instance_intf

module Make_null_hook
    (Lang : Act_language.Definition.S)
    (P : Travesty.Traversable.S1) :
  Hook with module Lang = Lang and module Program_container = P = struct
  module Lang = Lang
  module Ctx = Ctx.Make (Lang)
  module Program_container = P

  let on_all = Ctx.return

  let on_program = Ctx.return

  let on_statement = Ctx.return

  let on_instruction = Ctx.return

  let on_location = Ctx.return
end

module Make (B : Basic) :
  S
  with module Lang := B.Lang
   and module Program_container = B.Program_container = struct
  module Ctx = B.Ctx
  module Warn = B.Ctx.Warn
  module Lang = B.Lang
  module Redirect = Lang.Symbol.R_map
  module Program_container = B.Program_container

  (* Modules for building context-sensitive traversals over program
     containers and lists *)
  module Zip = Zipper.Int_mark_zipper (* for now *)

  module Ctx_Pcon = Program_container.On_monad (Ctx)
  module Ctx_List = Tx.List.On_monad (Ctx)
  module Ctx_Zip = Zip.On_monad (Ctx)
  module Ctx_Loc = Lang.Instruction.On_locations.On_monad (Ctx)
  module Ctx_Lst = Lang.Program.On_listings.On_monad (Ctx)
  module Ctx_Stm = Lang.Program.On_statements.On_monad (Ctx)

  module Output = struct
    module Program = struct
      type t =
        { listing: Lang.Program.t
        ; warnings: Warn.t list
        ; symbol_table: Act_abstract.Symbol.Table.t }
      [@@deriving fields]
    end

    type t = {programs: Program.t Program_container.t; redirects: Redirect.t}
    [@@deriving fields]
  end

  module PC =
    Travesty.Traversable.Fix_elt (B.Program_container) (Lang.Program)
  module PC_listings =
    Travesty.Traversable.Chain0 (PC) (Lang.Program.On_listings)
  module Ctx_PC = PC_listings.On_monad (Ctx)

  (* TODO(@MattWindsor91): the two functions below are adapted forms of
     Travesty extensions that don't (yet) appear for the program container
     modules we have above. Eventually it'd be nice for them to be in
     Travesty itself. *)

  let max_measure ~measure ?(default = 0) xs =
    xs
    |> PC_listings.max_elt ~compare:(Comparable.lift ~f:measure Int.compare)
    |> Option.value_map ~f:measure ~default

  let right_pad ~padding xs =
    let maxlen = max_measure ~measure:List.length xs
    and f = Fn.const padding in
    PC_listings.map
      ~f:(fun p -> p @ List.init (maxlen - List.length p) ~f)
      xs

  let stack_offset_to_heap prog_name offset =
    Ctx.Let_syntax.(
      let base_str =
        Printf.sprintf "t%ss%s" prog_name (Act_abstract.Location.Address.to_string offset)
      in
      let%bind symbol_str =
        Ctx.add_symbol base_str Act_abstract.Symbol.Sort.Heap
      in
      let%map symbol =
        Ctx.Monadic.return (Lang.Symbol.require_of_string symbol_str)
      in
      Lang.Location.make_heap_loc symbol)

  let change_stack_to_heap ins =
    Ctx.Let_syntax.(
      let%bind prog_name = Ctx.get_prog_name in
      let f loc =
        match Lang.Location.as_stack_offset loc with
        | Some offset ->
            stack_offset_to_heap prog_name offset
        | None ->
            Ctx.return loc
      in
      Ctx_Loc.map_m ~f ins)

  (** [warn_unknown_instructions stm] emits warnings for each instruction in
      [stm] without a high-level analysis. *)
  let warn_unknown_instructions ins =
    Ctx.(
      warn_if
        (Lang.Instruction.has_opcode ins
           ~opcode:Act_abstract.Instruction.Opcode.Unknown)
        (Warn.Instruction ins) (Warn.not_understood ())
      >>| fun () -> ins)

  let warn_unknown_operands ins abs_operands =
    Ctx.(
      warn_if
        (Act_abstract.Operand.Bundle.is_part_unknown abs_operands)
        (Warn.Operands ins) (Warn.not_understood ()))

  let warn_erroneous_operands ins abs_operands =
    Ctx_List.iter_m (Act_abstract.Operand.Bundle.errors abs_operands)
      ~f:(fun error -> Ctx.warn (Warn.Operands ins) (Warn.erroneous error)
    )

  (** [warn_unsupported_operands stm] emits warnings for each instruction in
      [stm] whose operands don't have a high-level analysis, or are
      erroneous. *)
  let warn_unsupported_operands ins =
    (* Don't emit warnings for unknown instructions---the upper warning
       should be enough. *)
    Act_abstract.Instruction.(
      match Lang.Instruction.abs_kind ins with
      | Opcode.Unknown ->
          Ctx.return ins
      | _ ->
          let open Ctx.Let_syntax in
          let abs_operands = Lang.Instruction.abs_operands ins in
          let%bind () = warn_unknown_operands ins abs_operands in
          let%map () = warn_erroneous_operands ins abs_operands in
          ins)

  let warn_immediate_heap_symbol ins abs_operands =
    Ctx.(
      get_symbol_table
      >>= fun symbol_table ->
      let should_warn =
        Act_abstract.Operand.Bundle.has_immediate_heap_symbol abs_operands
          ~symbol_table
      in
      warn_if should_warn (Warn.Operands ins)
        (Info.of_string
           ( "Operands contain a heap symbol in immediate position. "
           ^ "This may cause problems for Herd." )))

  (** [warn_untranslated_operands stm] emits warnings for each instruction
      in [stm] whose operands should have been lowered to a Herd-compatible
      form, but haven't been. *)
  let warn_untranslated_operands ins =
    let open Ctx.Let_syntax in
    let abs_operands = Lang.Instruction.abs_operands ins in
    let%map () = warn_immediate_heap_symbol ins abs_operands in
    ins

  let make_end_jump () =
    let open Ctx.Let_syntax in
    match%bind Ctx.get_end_label with
    | None ->
        Ctx.Monadic.return
          (Or_error.error_string
             "Tried to make an end-label jump without an end label")
    | Some endl ->
        return (Lang.Instruction.jump endl)

  let change_ret_to_end_jump ins =
    if
      Lang.Instruction.has_opcode ins
        ~opcode:Act_abstract.Instruction.Opcode.Return
    then make_end_jump ()
    else Ctx.return ins

  (** [sanitise_loc] performs sanitisation at the single location level. *)
  let sanitise_loc loc =
    let open Ctx in
    return loc >>= (`Language_hooks |-> B.on_location)

  (** [sanitise_all_locs loc] iterates location sanitisation over every
      location in [loc], threading the context through monadically. *)
  let sanitise_all_locs = Ctx_Loc.map_m ~f:sanitise_loc

  (** [sanitise_ins] performs sanitisation at the single instruction level. *)
  let sanitise_ins ins =
    let open Ctx in
    return ins
    >>= (`Language_hooks |-> B.on_instruction)
    >>= (`Warn |-> warn_unknown_instructions)
    >>= (`Warn |-> warn_unsupported_operands)
    >>= sanitise_all_locs
    >>= (`Simplify_litmus |-> change_ret_to_end_jump)
    >>= (`Simplify_litmus |-> change_stack_to_heap)
    >>= (`Warn |-> warn_untranslated_operands)

  (** [warn_unknown_statements stm] emits warnings for each statement in
      [stm] without a high-level analysis. *)
  let warn_unknown_statements stm =
    Ctx.(
      warn_if
        (Lang.Statement.is_unknown stm)
        (Warn.Statement stm) (Warn.not_understood ())
      >>| fun () -> stm)

  (** [sanitise_all_ins stm] iterates instruction sanitisation over every
      instruction in [stm], threading the context through monadically. *)
  let sanitise_all_ins =
    let module L = Lang.Statement.On_instructions.On_monad (Ctx) in
    L.map_m ~f:sanitise_ins

  (** [sanitise_stm] performs sanitisation at the single statement level. *)
  let sanitise_stm _ stm =
    let open Ctx in
    return stm
    >>= (`Language_hooks |-> B.on_statement)
    (* Do warnings after the language-specific hook has done any reduction
       necessary, but before we start making broad-brush changes to the
       statements. *)
    >>= (`Warn |-> warn_unknown_statements)
    >>= sanitise_all_ins

  (** [irrelevant_instruction_types] lists the high-level types of
      instruction that can be thrown out when converting to a litmus test. *)
  let irrelevant_instruction_types =
    Act_abstract.Instruction.Opcode.Kind.(
      Set.of_list
        [Call (* -not- Return: these need more subtle translation *); Stack])

  let instruction_is_irrelevant =
    Lang.Statement.opcode_in ~opcodes:irrelevant_instruction_types

  (** [proglen_fix f prog] runs [f] on [prog] until the program length no
      longer changes. *)
  let proglen_fix f prog =
    Ctx.(
      fix
        ~f:(fun mu (prog, len) ->
          let open Ctx.Let_syntax in
          let%bind prog' = f prog in
          let len' = Lang.Program.On_statements.length prog' in
          if len = len' then Ctx.return (prog', len') else mu (prog', len')
          )
        (prog, -1)
      >>| fst)

  let remove_statements_in prog ~where =
    Lang.Program.On_statements.exclude prog
      ~f:(Tx.List.any ~predicates:where)

  (** [remove_generally_irrelevant_statements prog] completely removes
      statements in [prog] that have no use in general and cannot be
      rewritten. *)
  let remove_generally_irrelevant_statements prog =
    let open Ctx.Let_syntax in
    let%bind symbol_table = Ctx.get_symbol_table in
    let%map remove_boundaries = Ctx.is_pass_enabled `Remove_boundaries in
    let label_check =
      if remove_boundaries then Lang.Statement.is_unused_label
      else Lang.Statement.is_unused_ordinary_label
    in
    remove_statements_in prog
      ~where:
        Lang.Statement.
          [is_nop; is_blank; is_directive; label_check ~symbol_table]

  (** [remove_litmus_irrelevant_statements prog] completely removes
      statements in [prog] that have no use in Litmus and cannot be
      rewritten. *)
  let remove_litmus_irrelevant_statements prog =
    Ctx.return
      (remove_statements_in prog
         ~where:
           Lang.Statement.[instruction_is_irrelevant; is_stack_manipulation])

  let process_possible_useless_jump () statement zipper =
    match Zip.peek_opt zipper with
    | Some next when Lang.Statement.is_jump_pair statement next ->
        Ctx.return (`Drop ())
    | Some _ | None ->
        Ctx.return (`Swap (statement, ()))

  let remove_useless_jumps_in_listing lst =
    Ctx_Zip.fold_m_until (Zip.of_list lst) ~f:process_possible_useless_jump
      ~init:() ~finish:(fun () zipper -> Ctx.return (Zip.to_list zipper))

  let remove_useless_jumps =
    Ctx_Lst.map_m ~f:remove_useless_jumps_in_listing

  module Deref = Deref.Make (B)
  module Symbols = Symbols.Make (B)

  let update_symbol_table (prog : Lang.Program.t) =
    let open Ctx.Let_syntax in
    let%bind targets = Ctx.get_all_redirect_targets in
    let known_heap_symbols = Lang.Symbol.Set.abstract targets in
    let symbols' = Lang.Program.symbols prog ~known_heap_symbols in
    Ctx.set_symbol_table symbols'

  let update_and_get_symbol_table prog =
    Ctx.(prog |> update_symbol_table >>= fun _ -> get_symbol_table)

  let generate_and_add_end_label (prog : Lang.Program.t) :
      Lang.Program.t Ctx.t =
    let open Ctx.Let_syntax in
    let%bind progname = Ctx.get_prog_name in
    let prefix = "END" ^ progname in
    let%bind lbl = Ctx.make_fresh_label prefix in
    let%map () = Ctx.set_end_label lbl in
    Lang.Program.On_listings.map prog ~f:(fun s ->
        s @ [Lang.Statement.label lbl] )

  (** [add_end_label] adds an end-of-program label to the current program. *)
  let add_end_label (prog : Lang.Program.t) : Lang.Program.t Ctx.t =
    let open Ctx.Let_syntax in
    (* Don't generate duplicate endlabels! *)
    match%bind Ctx.get_end_label with
    | Some _ ->
        return prog
    | None ->
        generate_and_add_end_label prog

  (** [remove_fix prog] performs a loop of statement-removing operations
      until we reach a fixed point in the program length. *)
  let remove_fix (prog : Lang.Program.t) : Lang.Program.t Ctx.t =
    let mu (prog : Lang.Program.t) : Lang.Program.t Ctx.t =
      Ctx.(
        prog
        |> tee_m ~f:update_symbol_table
        >>= (`Remove_useless |-> remove_generally_irrelevant_statements)
        >>= (`Remove_litmus |-> remove_litmus_irrelevant_statements)
        >>= (`Remove_useless |-> remove_useless_jumps))
    in
    proglen_fix mu prog

  let program_name i program =
    Option.value (Lang.Program.name program) ~default:(sprintf "%d" i)

  (** [sanitise_program] performs sanitisation on a single program. *)
  let sanitise_program (i : int) (prog : Lang.Program.t) :
      Lang.Program.t Ctx.t =
    let name = program_name i prog in
    Ctx.(
      enter_program ~name prog
      (* Initial table population. *)
      >>= tee_m ~f:update_symbol_table
      >>= (`Simplify_litmus |-> add_end_label)
      >>= (`Language_hooks |-> B.on_program)
      (* The language hook might have invalidated the symbol tables. *)
      >>= tee_m ~f:update_symbol_table
      >>= (`Simplify_deref_chains |-> Deref.on_program)
      (* Need to sanitise statements first, in case the sanitisation pass
         makes irrelevant statements (like jumps) relevant again. *)
      >>= Ctx_Stm.mapi_m ~f:sanitise_stm
      >>= remove_fix)

  let all_symbols_in (progs : Lang.Program.t Program_container.t) =
    progs |> Program_container.to_list
    |> List.concat_map ~f:Lang.Program.On_symbols.to_list

  let make_mangle_map (progs : Lang.Program.t Program_container.t) =
    let all_symbols = all_symbols_in progs in
    (* Build a map src->dst, where each dst is a symbol in the assembly, and
       src is one possible demangling of dst. We're assuming that there'll
       only be one unique dst for each src, and taking the most recent dst. *)
    all_symbols
    |> List.concat_map ~f:(fun dst ->
           List.map (Lang.Symbol.abstract_demangle dst) ~f:(fun src ->
               (src, dst) ) )
    |> String.Map.of_alist_reduce ~f:(fun _ y -> y)

  let warn_missing_redirect () : Info.t =
    Info.(
      of_list
        [ of_string "This symbol couldn't be found in the assembly."
        ; of_string
            "Any state set analysis for this program may not be valid." ])

  let find_initial_redirect mangle_map src =
    match String.Map.find mangle_map (Lang.Symbol.to_string src) with
    | Some dst ->
        Ctx.redirect ~src ~dst
    | None ->
        Ctx.warn (Symbol src) (warn_missing_redirect ())

  (** [find_initial_redirects symbols] tries to find the compiler-mangled
      version of each symbol in [symbols]. In each case, it sets up a
      redirect in the redirects table.

      If it fails to find at least one of the symbols, it'll raise a
      warning. *)
  let find_initial_redirects (progs : Lang.Program.t Program_container.t) =
    let open Ctx.Let_syntax in
    let%bind symbols = Ctx.get_variables in
    Ctx.unless_m (Set.is_empty symbols) ~f:(fun () ->
        let mangle_map = make_mangle_map progs in
        Ctx_List.iter_m (Set.to_list symbols)
          ~f:(find_initial_redirect mangle_map) )

  let build_single_program_output i listing =
    let open Ctx.Let_syntax in
    let name = program_name i listing in
    let%bind symbol_table = update_and_get_symbol_table listing in
    let%map warnings = Ctx.take_warnings name in
    {Output.Program.listing; symbol_table; warnings}

  let make_programs_uniform ps =
    right_pad ~padding:(Lang.Statement.empty ()) ps

  let build_output (rough_programs : Lang.Program.t Program_container.t) =
    let open Ctx.Let_syntax in
    (* TODO: this should be the same as Language.make_uniform *)
    let listings = make_programs_uniform rough_programs in
    let%bind programs =
      Ctx_Pcon.mapi_m ~f:build_single_program_output listings
    in
    let%bind var_set = Ctx.get_variables in
    let var_list = Set.to_list var_set in
    let%bind redirect_alist = Ctx.get_redirect_alist var_list in
    let%map redirects =
      Ctx.Monadic.return (Redirect.of_symbol_alist redirect_alist)
    in
    {Output.programs; redirects}

  let sanitise_with_ctx (progs : Lang.Program.t Program_container.t) =
    Ctx.(
      find_initial_redirects progs
      >>= fun () ->
      Ctx_Pcon.mapi_m ~f:sanitise_program progs
      (* We do this last, for two reasons: first, in case the instruction
         sanitisers have introduced invalid variables; and second, so that
         we know that the symbol changes agree across program boundaries.*)
      >>= Symbols.on_all
      >>= build_output)

  let sanitise ?passes ?(symbols = []) (prog : Lang.Program.t) =
    let passes' =
      Option.value ~default:Act_config.Sanitiser_pass.standard passes
    in
    let variables = Lang.Symbol.Set.of_list symbols in
    Ctx.(
      run
        (Monadic.return (B.split prog) >>= sanitise_with_ctx)
        (initial ~passes:passes' ~variables))
end

module Make_single (H : Hook_maker) :
  S
  with module Lang := H(Singleton).Lang
   and type 'a Program_container.t = 'a = Make (struct
  include H (Singleton)

  let split = Or_error.return (* no operation *)
end)

module Make_multi (H : Hook_maker) :
  S
  with module Lang := H(Tx.List).Lang
   and type 'a Program_container.t = 'a list = Make (struct
  include H (Tx.List)

  let split = Lang.Program.split_on_boundaries
end)
