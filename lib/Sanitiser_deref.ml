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

open Core_kernel
open Utils

module Portable = struct
  module Zip_opt = Zipper.On_monad (Option)

  type 'ins chain_item =
    | End of 'ins
    | Step of Abstract.Location.t
  [@@deriving sexp]
  ;;

  let operands_as_chain_start ins symbol_table { Src_dst.src; dst } =
    let open Option.Let_syntax in
    let is_immediate_heap_src =
      Abstract.Operand.is_immediate_heap_symbol ~symbol_table src
    in
    let%bind dst_loc = Abstract.Operand.as_location dst in
    Option.some_if is_immediate_heap_src (ins, dst_loc)
  ;;

  let chain_end_of_locations ins src last_dst =
    if Abstract.Location.is_dereference src last_dst
    then Some (End ins)
    else None
  ;;

  let chain_item_of_locations ins locs last_dst =
    let src = Src_dst.src locs in
    if Abstract.Location.equal src last_dst
    then Some (Step (Src_dst.dst locs))
    else chain_end_of_locations ins src last_dst
  ;;

  let%expect_test "chain_item_of_locations: chain step" =
    Sexp.output_hum Out_channel.stdout
      [%sexp
        (Abstract.Location.(
            chain_item_of_locations 10
              { src = Heap (Address.Int 20)
              ; dst = Heap (Address.Int 10)
              }
              (Heap (Address.Int 20))
          ) : int chain_item option)];
    [%expect {| ((Step (Heap (Int 10)))) |}]
  ;;

  let%expect_test "chain_item_of_locations: chain end" =
    Sexp.output_hum Out_channel.stdout
      [%sexp
        (Abstract.Location.(
            chain_item_of_locations 10
              { src = Register_indirect
                    { reg = General "eax"; offset = Int 0 }
              ; dst = Heap (Address.Int 10)
              }
              (Register_direct (General "eax"))
          ) : int chain_item option)];
    [%expect {| ((End 10)) |}]
  ;;

  let%expect_test "chain_item_of_locations: chain break" =
    Sexp.output_hum Out_channel.stdout
      [%sexp
        (Abstract.Location.(
            chain_item_of_locations 10
              { src = Register_indirect
                    { reg = General "eax"; offset = Int 0 }
              ; dst = Heap (Address.Int 10)
              }
              (Register_direct (General "esi"))
          ) : int chain_item option)];
    [%expect {| () |}]
  ;;

  let chain_item_of_operands ins last_dst { Src_dst.src; dst } =
    let open Option.Let_syntax in
    let%bind src_loc = Abstract.Operand.as_location src in
    let%bind dst_loc = Abstract.Operand.as_location dst in
    chain_item_of_locations ins
      { src = src_loc; dst = dst_loc } last_dst
  ;;

  let start_mark = 1

  let replace_chain_with value zipper =
    zipper
    |> Zipper.push ~value
    |> Zip_opt.delete_to_mark_m
      ~mark:start_mark ~on_empty:(Fn.const None)
  ;;

  let%expect_test "replace_chain_with_value: positive" =
    let open Option.Monad_infix in
    let result =
      Zipper.of_list [ 10; 40; 32; 9; 174; -12 ]
      |> Zip_opt.step_m ~steps:2 ~on_empty:(Fn.const None) (* on 32 *)
      >>= Zip_opt.mark_m ~mark:start_mark ~on_empty:(Fn.const None)
      >>= Zip_opt.step_m ~steps:2 ~on_empty:(Fn.const None) (* on 174 *)
      >>= replace_chain_with 64
      >>| Zipper.to_list
    in
    Sexp.output_hum Out_channel.stdout
      [%sexp (result : int list option) ];
    [%expect {| ((10 40 64 174 -12)) |}]
  ;;

  let%expect_test "replace_chain_with_value: missing mark" =
    let open Option.Monad_infix in
    let result =
      Zipper.of_list [ 10; 40; 32; 9; 174; -12 ]
      |> Zip_opt.step_m ~steps:2 ~on_empty:(Fn.const None) (* on 32 *)
      >>= Zip_opt.step_m ~steps:2 ~on_empty:(Fn.const None) (* on 174 *)
      >>= replace_chain_with 64
      >>| Zipper.to_list
    in
    Sexp.output_hum Out_channel.stdout
      [%sexp (result : int list option) ];
    [%expect {| () |}]
  ;;
end

module Make (B : Sanitiser_base.Basic) :
  Sanitiser_base.S_program with module Lang := B.Lang
                            and module Ctx := B.Ctx = struct
  include B
  include Portable

  module Ctx_Zip = Zipper.On_monad (Ctx)

  let is_move =
    Lang.Instruction.has_opcode
      ~opcode:Abstract.Instruction.Opcode.Move
  ;;

  let as_move_with_abstract_operands ins =
    let open Option.Let_syntax in
    let%bind ins = Option.some_if (is_move ins) ins in
    match Lang.Instruction.operands ins with
    | Src_dst sd -> Some sd
    | _ -> None
  ;;

  let instruction_as_chain_start symbol_table ins =
    Option.(
      ins
      |> as_move_with_abstract_operands
      >>= operands_as_chain_start ins symbol_table
    )
  ;;

  let as_chain_start symbol_table stm =
    Lang.Statement.On_instructions.find_map stm
      ~f:(instruction_as_chain_start symbol_table)
  ;;

  let instruction_as_chain_item last_loc ins =
    Option.(
      ins
      |> as_move_with_abstract_operands
      >>= chain_item_of_operands ins last_loc
    )
  ;;

  let as_chain_item stm last_loc =
    Lang.Statement.On_instructions.find_map stm
      ~f:(instruction_as_chain_item last_loc)
  ;;

  let find_chain_start symbol_table current =
    match as_chain_start symbol_table current with
    | Some (ins, dst) -> `Mark (start_mark, current, `Found (ins, dst))
    | None            -> `Swap (current, `Not_found)
  ;;

  let make_sanitised_move first_move final_move =
    let open Option.Let_syntax in
    Lang.Instruction.(
      let%bind src_sym = as_move_src_symbol   first_move
      and      dst     = as_move_dst_location final_move in
      let      src     = Lang.Location.make_heap_loc src_sym in
      let%map  ins = Or_error.ok (location_move ~src ~dst) in
      Lang.Statement.instruction ins
    )
  ;;

  let make_sanitised_zipper zipper first_move final_move =
    let open Option.Let_syntax in
    let%bind move = make_sanitised_move first_move final_move in
    replace_chain_with move zipper
  ;;

  let handle_complete_chain current zipper first_src final_dst =
    (** TODO(@MattWindsor91): propagate errors here as warnings. *)
    let zipper' =
      Option.value ~default:(Zipper.push zipper ~value:current)
        (make_sanitised_zipper zipper first_src final_dst)
    in `Stop zipper'
  ;;

  let handle_broken_chain current zipper =
    `Stop (Zipper.push zipper ~value:current)
  ;;

  let find_chain_step first_move last_loc current zipper =
    match as_chain_item current last_loc with
    | Some (Step next_loc) ->
      `Swap (current, `Found (first_move, next_loc))
    | Some (End final_move) ->
      handle_complete_chain current zipper first_move final_move
    | None -> handle_broken_chain current zipper
  ;;

  let chain_iter state current zipper =
    Ctx.(
      get_symbol_table >>| fun symbol_table ->
      match state with
      | `Not_found -> find_chain_start symbol_table current
      | `Found (first_src, last_dst) ->
        find_chain_step first_src last_dst current zipper
    )
  ;;

  let run_once_on_zipper prog_zipper =
    Ctx_Zip.fold_m_until prog_zipper
      ~f:chain_iter
      ~init:`Not_found
      ~finish:(fun _ zipper -> Ctx.return zipper)
  ;;

  let rec mu zipper =
    let open Ctx.Let_syntax in
    if Zipper.is_at_end zipper
    then return zipper
    else (
      let%bind zipper' = run_once_on_zipper zipper in
      assert Zipper.(right_length zipper' < right_length zipper);
      mu zipper'
    )
  ;;

  let on_program prog =
    Ctx.(prog |> Zipper.of_list |> mu >>| Zipper.to_list)
  ;;
end