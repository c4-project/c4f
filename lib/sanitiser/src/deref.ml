(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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
module A = Act_common

module Mark = struct
  module M = struct
    type t = [`Start_of_chain] [@@deriving compare, sexp]
  end

  include M
  include Comparable.Make (M)
end

module Zip = Zipper.Make_marked (Mark)

module State = struct
  (** [t] is the internal state of the deref chain finder at any given time,
      parametrised on the type of concrete instructions. *)
  type 'ins t =
    | Read of
        { first_target_src: 'ins
        ; last_target_dst: Act_abstract.Location.t }
    | Write of
        { first_target_src: 'ins
        ; first_value_src: 'ins
        ; last_target_dst: Act_abstract.Location.t
        ; last_value_dst: Act_abstract.Location.t }
  [@@deriving sexp]

  let update_with_target state new_dst =
    Some
      ( match state with
      | Read r ->
          Read {r with last_target_dst= new_dst}
      | Write w ->
          Write {w with last_target_dst= new_dst} )

  let update_with_value state new_dst =
    match state with
    | Read _ ->
        None
    | Write w ->
        Some (Write {w with last_value_dst= new_dst})

  let to_write state first_value_src last_value_dst =
    match state with
    | Read {first_target_src; last_target_dst} ->
        Some
          (Write
             { first_target_src
             ; last_target_dst
             ; first_value_src
             ; last_value_dst })
    | Write _ ->
        None

  let initial first_target_src last_target_dst =
    Read {first_target_src; last_target_dst}
end

(** [Chain_item] contains types and functions for interpreting instructions
    as items in the middle of an ongoing deref chain. *)
module Chain_item = struct
  (** [t] is the type returned by the chain item finder on each valid chain
      item, parametrised on the type of concrete instructions. *)
  type 'ins t =
    | End of 'ins
    | New_write of 'ins * Act_abstract.Location.t
    | Target_step of Act_abstract.Location.t
    | Value_step of Act_abstract.Location.t
  [@@deriving sexp]

  let is_read_end {A.Src_dst.src; _} last_target_dst =
    Act_abstract.Location.is_dereference src last_target_dst

  let is_write_end {A.Src_dst.src; dst} ~last_target_dst ~last_value_dst =
    Act_abstract.Location.(
      is_dereference dst last_target_dst && equal src last_value_dst)

  let%expect_test "is_write_end: valid write end" =
    printf "%b"
      (is_write_end
         { src= Heap (Int 20)
         ; dst= Register_indirect {reg= General "eax"; offset= Int 0} }
         ~last_target_dst:(Register_direct (General "eax"))
         ~last_value_dst:(Heap (Int 20))) ;
    [%expect {| true |}]

  let is_end locs = function
    | State.Read {last_target_dst; _} ->
        is_read_end locs last_target_dst
    | Write {last_target_dst; last_value_dst; _} ->
        is_write_end locs ~last_target_dst ~last_value_dst

  let end_of_locations ins locs state =
    if is_end locs state then Some (End ins) else None

  let step_of_locations is_move {A.Src_dst.src; dst} = function
    | _ when not is_move ->
        None
    | (State.Read {last_target_dst; _} | Write {last_target_dst; _})
      when Act_abstract.Location.equal src last_target_dst ->
        Some (Target_step dst)
    | Write {last_value_dst; _}
      when Act_abstract.Location.equal src last_value_dst ->
        Some (Value_step dst)
    | Read _ | Write _ ->
        None

  let of_locations ins is_move locs state =
    (* Note: these two classifications overlap somewhat, so the order of end
       before step is deliberate. *)
    Tx.Option.first_some_of_thunks
      [ (fun () -> end_of_locations ins locs state)
      ; (fun () -> step_of_locations is_move locs state) ]

  let%expect_test "of_locations: read chain step" =
    Sexp.output_hum Out_channel.stdout
      [%sexp
        ( Act_abstract.Location.(
            of_locations 10 true
              {src= Heap (Address.Int 20); dst= Heap (Address.Int 10)}
              (Read
                 { first_target_src= 15
                 ; last_target_dst= Heap (Address.Int 20) }))
          : int t option )] ;
    [%expect {| ((Target_step (Heap (Int 10)))) |}]

  let%expect_test "of_locations: not a move, so not a valid step" =
    Sexp.output_hum Out_channel.stdout
      [%sexp
        ( Act_abstract.Location.(
            of_locations 10 false
              {src= Heap (Address.Int 20); dst= Heap (Address.Int 10)}
              (Read
                 { first_target_src= 15
                 ; last_target_dst= Heap (Address.Int 20) }))
          : int t option )] ;
    [%expect {| () |}]

  let%expect_test "of_locations: read chain end" =
    Sexp.output_hum Out_channel.stdout
      [%sexp
        ( Act_abstract.Location.(
            of_locations 10 true
              { src= Register_indirect {reg= General "eax"; offset= Int 0}
              ; dst= Heap (Address.Int 10) }
              (Read
                 { first_target_src= 15
                 ; last_target_dst= Register_direct (General "eax") }))
          : int t option )] ;
    [%expect {| ((End 10)) |}]

  let%expect_test "of_locations: read chain end, not a move" =
    Sexp.output_hum Out_channel.stdout
      [%sexp
        ( Act_abstract.Location.(
            of_locations 10 false
              { src= Register_indirect {reg= General "eax"; offset= Int 0}
              ; dst= Heap (Address.Int 10) }
              (Read
                 { first_target_src= 15
                 ; last_target_dst= Register_direct (General "eax") }))
          : int t option )] ;
    [%expect {| ((End 10)) |}]

  let%expect_test "of_locations: chain break" =
    Sexp.output_hum Out_channel.stdout
      [%sexp
        ( Act_abstract.Location.(
            of_locations 10 true
              { src= Register_indirect {reg= General "eax"; offset= Int 0}
              ; dst= Heap (Address.Int 10) }
              (Read
                 { first_target_src= 15
                 ; last_target_dst= Register_direct (General "esi") }))
          : int t option )] ;
    [%expect {| () |}]

  let%expect_test "of_locations: write chain target step" =
    Sexp.output_hum Out_channel.stdout
      [%sexp
        ( Act_abstract.Location.(
            of_locations 10 true
              {src= Heap (Address.Int 20); dst= Heap (Address.Int 10)}
              (Write
                 { first_target_src= 15
                 ; first_value_src= 25
                 ; last_target_dst= Heap (Address.Int 20)
                 ; last_value_dst= Heap (Address.Int 40) }))
          : int t option )] ;
    [%expect {| ((Target_step (Heap (Int 10)))) |}]

  let%expect_test "of_locations: write chain value step" =
    Sexp.output_hum Out_channel.stdout
      [%sexp
        ( Act_abstract.Location.(
            of_locations 10 true
              {src= Heap (Address.Int 40); dst= Heap (Address.Int 10)}
              (Write
                 { first_target_src= 15
                 ; first_value_src= 25
                 ; last_target_dst= Heap (Address.Int 20)
                 ; last_value_dst= Heap (Address.Int 40) }))
          : int t option )] ;
    [%expect {| ((Value_step (Heap (Int 10)))) |}]

  let%expect_test "of_locations: write chain end" =
    Sexp.output_hum Out_channel.stdout
      [%sexp
        ( Act_abstract.Location.(
            of_locations 10 true
              { src= Heap (Address.Int 10)
              ; dst= Register_indirect {reg= General "eax"; offset= Int 0}
              }
              (Write
                 { first_target_src= 15
                 ; first_value_src= 25
                 ; last_target_dst= Register_direct (General "eax")
                 ; last_value_dst= Heap (Address.Int 10) }))
          : int t option )] ;
    [%expect {| ((End 10)) |}]

  let of_operands_as_locations ins is_move {A.Src_dst.src; dst} state =
    let open Option.Let_syntax in
    let%bind src_loc = Act_abstract.Operand.as_location src
    and dst_loc = Act_abstract.Operand.as_location dst in
    of_locations ins is_move {src= src_loc; dst= dst_loc} state

  let as_possible_new_write ins {A.Src_dst.src; dst} =
    if Act_abstract.Operand.is_immediate src then
      let open Option.Let_syntax in
      let%map dst_loc = Act_abstract.Operand.as_location dst in
      New_write (ins, dst_loc)
    else None

  let%expect_test "as_possible_new_write: valid example" =
    Sexp.output_hum Out_channel.stdout
      [%sexp
        ( Act_abstract.Location.(
            as_possible_new_write 10
              { src= Int 32
              ; dst=
                  Location
                    (Register_indirect {reg= General "eax"; offset= Int 0})
              })
          : int t option )] ;
    [%expect
      {| ((New_write 10 (Register_indirect (reg (General eax)) (offset (Int 0))))) |}]

  let of_operands ins is_move locs state =
    Tx.Option.first_some_of_thunks
      [ (fun () -> of_operands_as_locations ins is_move locs state)
      ; (fun () -> as_possible_new_write ins locs) ]
end

module Portable = struct
  let operands_as_chain_start ins symbol_table {A.Src_dst.src; dst} =
    let open Option.Let_syntax in
    let is_immediate_heap_src =
      Act_abstract.Operand.is_immediate_heap_symbol ~symbol_table src
    in
    let%bind dst_loc = Act_abstract.Operand.as_location dst in
    Option.some_if is_immediate_heap_src (ins, dst_loc)

  let replace_chain_with value zipper =
    Zip.(
      zipper |> push ~value
      |> On_option.delete_to_mark_m ~mark:`Start_of_chain
           ~on_empty:(Fn.const None))

  let%expect_test "replace_chain_with_value: positive" =
    let open Option.Monad_infix in
    let result =
      Zip.(
        of_list [10; 40; 32; 9; 174; -12]
        |> On_option.step_m ~steps:2 ~on_empty:(Fn.const None) (* on 32 *)
        >>= On_option.mark_m ~mark:`Start_of_chain ~on_empty:(Fn.const None)
        >>= On_option.step_m ~steps:2 ~on_empty:(Fn.const None) (* on 174 *)
        >>= replace_chain_with 64 >>| to_list)
    in
    Sexp.output_hum Out_channel.stdout [%sexp (result : int list option)] ;
    [%expect {| ((10 40 64 174 -12)) |}]

  let%expect_test "replace_chain_with_value: missing mark" =
    let open Option.Monad_infix in
    let result =
      Zip.(
        of_list [10; 40; 32; 9; 174; -12]
        |> On_option.step_m ~steps:2 ~on_empty:(Fn.const None) (* on 32 *)
        >>= On_option.step_m ~steps:2 ~on_empty:(Fn.const None) (* on 174 *)
        >>= replace_chain_with 64 >>| to_list)
    in
    Sexp.output_hum Out_channel.stdout [%sexp (result : int list option)] ;
    [%expect {| () |}]

  let handle_broken_chain current zipper =
    `Stop (Zip.push zipper ~value:current)

  let try_update_state_and_continue state_maybe current zipper =
    match state_maybe with
    | Some state' ->
        `Swap (current, `Found state')
    | None ->
        handle_broken_chain current zipper

  let handle_target_step state current zipper new_dst =
    let state_maybe = State.update_with_target state new_dst in
    try_update_state_and_continue state_maybe current zipper

  let handle_value_step state current zipper new_dst =
    let state_maybe = State.update_with_value state new_dst in
    try_update_state_and_continue state_maybe current zipper

  let handle_new_write current zipper ins value_dst state =
    let state_maybe = State.to_write state ins value_dst in
    try_update_state_and_continue state_maybe current zipper
end

module Make (B : Pass_intf.Basic) :
  Pass_intf.S with type t := B.Lang.Program.t and type 'a ctx := 'a B.Ctx.t =
struct
  include Portable
  module Ctx_Zip = Zip.On_monad (B.Ctx)

  let is_move =
    B.Lang.Instruction.has_opcode
      ~opcode:Act_abstract.Instruction.Opcode.Move

  let as_move_with_abstract_operands ins =
    Option.(
      ins
      |> Option.some_if (is_move ins)
      >>= B.Lang.Instruction.On_operands.as_src_dst)

  let instruction_as_chain_start symbol_table ins =
    Option.(
      ins |> as_move_with_abstract_operands
      >>= operands_as_chain_start ins symbol_table)

  let as_chain_start symbol_table stm =
    B.Lang.Statement.On_instructions.find_map stm
      ~f:(instruction_as_chain_start symbol_table)

  let instruction_as_chain_item state ins =
    let open Option.Let_syntax in
    let%bind locs = B.Lang.Instruction.On_operands.as_src_dst ins in
    Chain_item.of_operands ins (is_move ins) locs state

  let as_chain_item stm state =
    B.Lang.Statement.On_instructions.find_map stm
      ~f:(instruction_as_chain_item state)

  let find_chain_start symbol_table current =
    match as_chain_start symbol_table current with
    | Some (ins, dst) ->
        let state = State.initial ins dst in
        `Mark (`Start_of_chain, current, `Found state)
    | None ->
        `Swap (current, `Not_found)

  module Direct_move = struct
    let make_read first_move final_move =
      let open Option.Let_syntax in
      B.Lang.Instruction.(
        let%bind src_sym = as_move_symbol first_move `Src
        and dst = as_move_location final_move `Dst in
        let src = B.Lang.Location.make_heap_loc src_sym in
        let%map ins = Or_error.ok (location_move ~src ~dst) in
        B.Lang.Statement.instruction ins)

    let make_write first_move final_move =
      let open Option.Let_syntax in
      B.Lang.Instruction.(
        let%bind src = as_move_immediate first_move `Src
        and dst_sym = as_move_symbol final_move `Src in
        let dst = B.Lang.Location.make_heap_loc dst_sym in
        let%map ins = Or_error.ok (immediate_move ~src ~dst) in
        B.Lang.Statement.instruction ins)

    let make final_move = function
      (* TODO(@MattWindsor91): propagate errors here as warnings. *)
      | State.Read {first_target_src; _} ->
          make_read first_target_src final_move
      | Write {first_value_src; first_target_src; _} ->
          make_write first_value_src first_target_src
  end

  let handle_complete_chain_opt zipper final_ins state =
    let open Option.Let_syntax in
    let%bind move = Direct_move.make final_ins state in
    let%map zipper' = replace_chain_with move zipper in
    `Stop zipper'

  let handle_complete_chain current zipper final_ins state =
    Option.value
      ~default:(`Stop (Zip.push ~value:current zipper))
      (handle_complete_chain_opt zipper final_ins state)

  let handle_chain_item state current zipper = function
    | Chain_item.Target_step new_dst ->
        handle_target_step state current zipper new_dst
    | Value_step new_dst ->
        handle_value_step state current zipper new_dst
    | End final_move ->
        handle_complete_chain current zipper final_move state
    | New_write (ins, value_dst) ->
        handle_new_write current zipper ins value_dst state

  let find_next_chain_item state current zipper =
    match as_chain_item current state with
    | Some item ->
        handle_chain_item state current zipper item
    | None ->
        handle_broken_chain current zipper

  let chain_iter state current zipper =
    B.Ctx.(
      get_symbol_table
      >>| fun symbol_table ->
      match state with
      | `Not_found ->
          find_chain_start symbol_table current
      | `Found state ->
          find_next_chain_item state current zipper)

  let run_once_on_zipper prog_zipper =
    Ctx_Zip.fold_m_until prog_zipper ~f:chain_iter ~init:`Not_found
      ~finish:(fun _ zipper -> B.Ctx.return zipper)

  let rec mu zipper =
    let open B.Ctx.Let_syntax in
    if Zip.is_at_end zipper then return zipper
    else
      let%bind zipper' = run_once_on_zipper zipper in
      assert (Zip.(right_length zipper' < right_length zipper)) ;
      mu zipper'

  let on_listing lst = B.Ctx.(lst |> Zip.of_list |> mu >>| Zip.to_list)

  module PL = B.Lang.Program.On_listings.On_monad (B.Ctx)

  let run = PL.map_m ~f:on_listing
end
