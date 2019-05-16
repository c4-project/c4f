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
open Act_utils
module Tx = Travesty_core_kernel_exts

module Scope = struct
  module M = struct
    type t = Unknown | Local | Global [@@deriving sexp, equal, quickcheck]
  end

  include M

  include Comparable.Make (struct
    (* The comparison scheme used here is very deliberate, hence why we
       write it out explicitly:

       - information about a scope > no information about a scope; -
       considering a variable as global > considering it as local *)

    include M

    let weight = function Unknown -> 0 | Local -> 1 | Global -> 2

    let compare = Tx.Fn.on weight ~f:Int.compare
  end)

  let is_global = function Global -> true | Local | Unknown -> false

  let is_local = function Local -> true | Global | Unknown -> false
end

module Initial_value = struct
  type t = int option [@@deriving sexp, compare, equal, quickcheck]
end

module Record = struct
  module M = struct
    type t =
      {scope: Scope.t; initial_value: Initial_value.t; tid: int option}
    [@@deriving sexp, compare, equal, make, fields, quickcheck]
  end

  include M
  include Comparable.Make (M)

  let remove_tid (record : t) : t = {record with tid= None}

  let has_tid (record : t) : bool = Option.is_some (tid record)

  let is_global (record : t) : bool = Scope.is_global (scope record)

  let is_local (record : t) : bool = Scope.is_local (scope record)

  let resolve_clash : [`Left of t | `Right of t | `Both of t * t] -> t =
    function
    | `Left x | `Right x ->
        x
    | `Both (l, r) ->
        max l r
end

module Map = struct
  type t = Record.t C_id.Map.t [@@deriving sexp, equal]

  module Q : My_quickcheck.S_with_sexp with type t := t = struct
    let sexp_of_t = sexp_of_t

    let quickcheck_generator =
      C_id.Map.quickcheck_generator C_id.quickcheck_generator
        Record.quickcheck_generator

    let quickcheck_observer =
      C_id.Map.quickcheck_observer C_id.quickcheck_observer
        Record.quickcheck_observer

    let quickcheck_shrinker =
      C_id.Map.quickcheck_shrinker C_id.quickcheck_shrinker
        Record.quickcheck_shrinker
  end

  include Q

  let of_single_scope_map ?(tid : int option)
      ?(scope : Scope.t = Scope.Unknown)
      (cvars : Initial_value.t C_id.Map.t) : t =
    C_id.Map.map cvars ~f:(fun initial_value ->
        Record.make ?tid ~scope ~initial_value () )

  let of_single_scope_set ?(tid : int option)
      ?(scope : Scope.t = Scope.Unknown) (cvars : C_id.Set.t) : t =
    let cvars_map = C_id.Set.to_map cvars ~f:(Fn.const None) in
    of_single_scope_map ?tid ~scope cvars_map

  let vars_satisfying (map : t) ~(f : Record.t -> bool) : C_id.Set.t =
    map |> C_id.Map.filter ~f |> C_id.Map.keys |> C_id.Set.of_list

  let globals : t -> C_id.Set.t = vars_satisfying ~f:Record.is_global

  let locals : t -> C_id.Set.t = vars_satisfying ~f:Record.is_local

  let resolve_cvar_clashes ~key value =
    ignore (key : C_id.t) ;
    Some (Record.resolve_clash value)

  let merge : t -> t -> t = C_id.Map.merge ~f:resolve_cvar_clashes

  let merge_list_opt (xs : t list) : t option =
    List.reduce_balanced xs ~f:merge

  let merge_list (xs : t list) : t =
    xs |> merge_list_opt |> Option.value ~default:C_id.Map.empty

  let of_litmus_id_pair ?(scope : Scope.t = Scope.Unknown)
      (id : Litmus_id.t) (initial_value : Initial_value.t) :
      C_id.t * Record.t =
    let tid = Litmus_id.tid id in
    let name = Litmus_id.variable_name id in
    (name, Record.make ~scope ?tid ~initial_value ())

  let of_litmus_id_alist ?(scope : Scope.t option)
      (xs : (Litmus_id.t, Initial_value.t) List.Assoc.t) : t Or_error.t =
    xs
    |> List.map ~f:(Tuple2.uncurry (of_litmus_id_pair ?scope))
    |> C_id.Map.of_alist_or_error

  let map (m : t) ~(f : C_id.t -> Record.t -> C_id.t * Record.t) :
      t Or_error.t =
    m |> C_id.Map.to_alist
    |> List.map ~f:(Tuple2.uncurry f)
    |> C_id.Map.of_alist_or_error
end

let%test_module "Map tests" =
  ( module struct
    let%expect_test "merge_list_opt: no maps" =
      Stdio.print_s [%sexp (Map.merge_list_opt [] : Map.t option)] ;
      [%expect {| () |}]

    let%expect_test "of_value_maps_opt: empty maps" =
      Stdio.print_s
        [%sexp
          ( Map.merge_list_opt [C_id.Map.empty; C_id.Map.empty]
            : Map.t option )] ;
      [%expect {| (()) |}]

    let%test_unit "vars_satisfying false = empty" =
      Base_quickcheck.Test.run_exn
        (module Map)
        ~f:(fun m ->
          [%test_result: C_id.Set.t] ~here:[[%here]]
            ~equal:[%equal: C_id.Set.t]
            (Map.vars_satisfying m ~f:(Fn.const false))
            ~expect:C_id.Set.empty )
  end )

module String_lang = struct
  module T_opt = Tx.Option.With_errors

  let parse_list_as_alist :
      string list -> (Litmus_id.t, Initial_value.t) List.Assoc.t Or_error.t
      =
    Litmus_id.Assoc.try_parse
      ~value_parser:
        (Tx.Option.With_errors.map_m ~f:(fun s ->
             Or_error.try_with (fun () -> Int.of_string s) ))

  let%expect_test "parse_list_as_alist: present, no tid" =
    Stdio.print_s
      [%sexp
        ( parse_list_as_alist ["foo = 3"]
          : (Litmus_id.t, Initial_value.t) List.Assoc.t Or_error.t )] ;
    [%expect {| (Ok ((foo (3)))) |}]

  let%expect_test "parse_list_as_alist: absent" =
    Stdio.print_s
      [%sexp
        ( parse_list_as_alist ["foobar"]
          : (Litmus_id.t, Initial_value.t) List.Assoc.t Or_error.t )] ;
    [%expect {| (Ok ((foobar ()))) |}]

  let parse_list ?(scope : Scope.t option) (sl : string list) :
      Map.t Or_error.t =
    Or_error.Monad_infix.(
      sl |> parse_list_as_alist >>= Map.of_litmus_id_alist ?scope)
end
