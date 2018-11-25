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

(* Define this in a separate module so we can include it as
   [Elt] below. *)
module M = struct
  (* For some reason, [eq]'s [@equal] override doesn't work,
     and [equal] in ppx_compare hasn't stabilised yet, so we have
     to do this dance to implement error equality. *)
  type err = Error.t
  let equal_err = My_fn.on Error.sexp_of_t Sexp.equal
  let err_of_sexp = Error.t_of_sexp
  let sexp_of_err = Error.sexp_of_t

  type t =
    | Int of int
    | Location of Abstract_location.t
    | Symbol of Abstract_symbol.t
    | Erroneous of err
    | Other
    | Unknown
  [@@deriving sexp, eq]
  ;;

  let is_unknown = function
    | Unknown -> true
    | Erroneous _ | Int _ | Location _ | Symbol _ | Other -> false
  ;;

  let is_stack_pointer = function
    | Location (Abstract_location.StackPointer) -> true
    | Location _
    | Erroneous _ | Int _ | Symbol _ | Other | Unknown -> false
  ;;

  let is_immediate_heap_symbol o ~syms = match o with
    | Symbol sym ->
      Abstract_symbol.(Table.mem syms ~sort:Sort.Heap sym)
    | Erroneous _ | Int _ | Location _ | Other | Unknown -> false
  ;;

  let is_jump_symbol_where o ~f = match o with
    | Symbol sym
    | Location (Abstract_location.Heap sym) -> f sym
    | Location _
    | Erroneous _ | Int _ | Other | Unknown -> false
  ;;

  let pp f = function
    | Int k        -> Format.fprintf f "@[<h>$%d@]" k
    | Erroneous e  -> Format.fprintf f "@[<h><ERR: %a>@]" Error.pp e
    | Location loc -> Abstract_location.pp f loc
    | Symbol s     -> Format.fprintf f "@[<h>sym:%s@]" s
    | Other        -> String.pp f "other"
    | Unknown      -> String.pp f "??"
  ;;
end
include M

module Bundle = struct
  type elt = M.t

  type t =
    | None
    | Single of M.t
    | Double of M.t * M.t
    | Src_dst of (M.t, M.t) Src_dst.t
  [@@deriving sexp, variants]
  ;;

  (* Intentionally override the [variants] version. *)
  let src_dst ~src ~dst = Src_dst { Src_dst.src; dst }

  let is_src_dst : t -> bool = function
    | Src_dst _ -> true
    | None | Single _ | Double _ -> false
  ;;

  include Fold_map.Make_container0 (struct
      module Elt = M
      type nonrec t = t

      module On_monad (Mo : Monad.S) = struct
        module H = Fold_map.Helpers (Mo)

        let fold_mapM ~f ~init bundle =
          Variants.map bundle
            ~none:(H.proc_variant0 H.fold_nop init)
            ~single:(H.proc_variant1 f init)
            ~double:(H.proc_variant2
                       (fun i (x, y) ->
                          let open Mo.Let_syntax in
                          let%bind i' , x' = f i  x in
                          let%map  i'', y' = f i' y in
                          (i'', (x', y')))
                          init)
            ~src_dst:(H.proc_variant1
                       (fun i { Src_dst.src; dst } ->
                          let open Mo.Let_syntax in
                          let%bind i' , src' = f i  src in
                          let%map  i'', dst' = f i' dst in
                          (i'', { Src_dst.src = src'; dst = dst' }))
                          init)
        ;;
      end
    end)

  let errors bundle =
    let f = function
      | Erroneous e -> Some e
      | Int _ | Symbol _ | Location _ | Unknown | Other -> None
    in bundle |> to_list |> List.filter_map ~f
  ;;

  let is_part_unknown = exists ~f:is_unknown
  let has_stack_pointer = exists ~f:is_stack_pointer

  let has_stack_pointer_src = function
    | Src_dst { Src_dst.src; _ } -> is_stack_pointer src
    | None | Single _ | Double _ -> false
  ;;

  let has_stack_pointer_dst = function
    | Src_dst { Src_dst.dst; _ } -> is_stack_pointer dst
    | None | Single _ | Double _ -> false
  ;;

  let is_single_jump_symbol_where operands ~f =
    match operands with
    | Single o -> M.is_jump_symbol_where o ~f
    | None | Src_dst _ | Double _ -> false
  ;;

  let%expect_test "has_stack_pointer_dst: positive" =
    Utils.Io.print_bool
      (has_stack_pointer_dst
         (src_dst
            ~src:(Location (Abstract_location.GeneralRegister))
            ~dst:(Location (Abstract_location.StackPointer))));
    [%expect {| true |}]
  ;;

  let%expect_test "has_stack_pointer_dst: negative" =
    Utils.Io.print_bool
      (has_stack_pointer_dst
         (src_dst
            ~dst:(Location (Abstract_location.GeneralRegister))
            ~src:(Location (Abstract_location.StackPointer))));
    [%expect {| false |}]
  ;;

  let%expect_test "has_stack_pointer_src: positive" =
    Utils.Io.print_bool
      (has_stack_pointer_src
         (src_dst
            ~dst:(Location (Abstract_location.GeneralRegister))
            ~src:(Location (Abstract_location.StackPointer))));
    [%expect {| true |}]
  ;;

  let%expect_test "has_stack_pointer_src: negative" =
    Utils.Io.print_bool
      (has_stack_pointer_src
         (src_dst
            ~src:(Location (Abstract_location.GeneralRegister))
            ~dst:(Location (Abstract_location.StackPointer))));
    [%expect {| false |}]
  ;;

  let has_immediate_heap_symbol operands ~syms =
    exists ~f:(is_immediate_heap_symbol ~syms) operands
  ;;

  let%expect_test "has_immediate_heap_symbol: src/dst positive" =
    let syms = Abstract_symbol.(
        Table.of_sets
          [ Set.of_list [ "foo"; "bar"; "baz" ], Sort.Heap
          ; Set.of_list [ "froz" ], Sort.Label
          ]
      )
    in
    let result = has_immediate_heap_symbol ~syms
        (Src_dst
           { src = Symbol "foo"
           ; dst = Location GeneralRegister
           })
    in
    Sexp.output_hum Out_channel.stdout [%sexp (result : bool)];
    [%expect {| true |}]
  ;;

  let%expect_test "has_immediate_heap_symbol: src/dst negative" =
    let syms = Abstract_symbol.(
        Table.of_sets
          [ Set.of_list [ "foo"; "bar"; "baz" ], Sort.Heap
          ; Set.of_list [ "froz" ], Sort.Label
          ]
      )
    in
    let result = has_immediate_heap_symbol ~syms
        (Src_dst
           { src = Symbol "froz"
           ; dst = Location GeneralRegister
           })
    in
    Sexp.output_hum Out_channel.stdout [%sexp (result : bool)];
    [%expect {| false |}]
  ;;

  let pp f = function
    | None -> String.pp f "none"
    | Single x -> M.pp f x
    | Double (op1, op2) ->
      Format.fprintf f "@[%a,@ %a@]" M.pp op1 M.pp op2
    | Src_dst { Src_dst.src; dst} ->
      Format.fprintf f "@[%a@ ->@ %a@]" M.pp src M.pp dst
  ;;
end

module Flag = Abstract_flag.None
