(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(* Needed because Base shadows it: *)
module Ty = Type

open Base
open Import

module Lang :
  C4f_litmus.Test_types.Basic
    with type Statement.t =
      [`Stm of unit Statement.t | `Decl of Initialiser.t Common.C_named.t]
     and type Program.t = unit Function.t Common.C_named.t
     and type Constant.t = Constant.t = struct
  module Constant = Constant

  module Statement = struct
    type t =
      [`Stm of unit Statement.t | `Decl of Initialiser.t Common.C_named.t]
    [@@deriving sexp]

    let empty () : t =
      `Stm Accessor.(construct (Statement.prim' @> Prim_statement.nop) ())

    let make_uniform = Tx.List.right_pad ~padding:(empty ())
  end

  module Type = Ty

  module Program = struct
    type t = unit Function.t Common.C_named.t [@@deriving sexp]

    let name ({name; _} : t) : string option =
      Some (Common.C_id.to_string name)

    let listing ({value; _} : t) : Statement.t list =
      List.map (Function.body_decls value) ~f:(fun (name, value) ->
          `Decl (Common.C_named.make value ~name) )
      @ List.map (Function.body_stms value) ~f:(fun x -> `Stm x)

    let global_vars (np : t) =
      np.@(Common.C_named.value @> Function.Access.parameters)
      |> Map.of_alist_or_error (module Common.C_id)
      |> Result.ok
  end

  let name = "C"
end

module Test = C4f_litmus.Test.Make (Lang)

module Var = struct
  module Record = struct
    (* TODO(MattWindsor91): merge this with one of the 9000 other variable
       record modules? *)

    type t = {ty: Ty.t; param_index: int; initial_value: Constant.t option}
    [@@deriving compare, sexp]
  end

  let expand_parameter (param_index : int)
      ~(init : Constant.t Common.C_named.Alist.t)
      ((id, ty) : Common.C_id.t * Ty.t) : Common.C_id.t * Record.t =
    let initial_value = List.Assoc.find init ~equal:Common.C_id.equal id in
    (id, {ty; param_index; initial_value})

  let merge_parameters (pss : Ty.t Common.C_named.Alist.t list) :
      Ty.t Common.C_named.Alist.t Or_error.t =
    Or_error.Let_syntax.(
      let%bind ps =
        pss
        |> List.reduce
             ~f:
               (Utils.My_list.merge_preserving_order
                  [%equal: Common.C_id.t * Ty.t] )
        |> Result.of_option
             ~error:(Error.of_string "need at least one function")
      in
      let%map () =
        ps
        |> List.find_all_dups
             ~compare:(Comparable.lift ~f:fst Common.C_id.compare)
        |> List.map ~f:(fun (d, _) ->
               Or_error.errorf "parameter definition inconsistency: %s"
                 (Common.C_id.to_string d) )
        |> Or_error.combine_errors_unit
      in
      ps)

  let merge_and_expand_parameters (init : Constant.t Common.C_named.Alist.t)
      (pss : Ty.t Common.C_named.Alist.t list) :
      Record.t Common.C_named.Alist.t Or_error.t =
    Or_error.(
      pss |> merge_parameters >>| List.mapi ~f:(expand_parameter ~init))

  let make_global_alist (init : Constant.t Common.C_named.Alist.t)
      (progs : Test.Lang.Program.t list) :
      (Common.Litmus_id.t, Record.t) List.Assoc.t Or_error.t =
    Or_error.(
      Accessor_base.(
        progs.@*(List.each @> Common.C_named.value
                 @> Function.Access.parameters))
      |> merge_and_expand_parameters init
      |> Or_error.tag ~tag:"couldn't deduce global parameters"
      >>| Tx.Alist.map_left ~f:C4f_common.Litmus_id.global)

  let make_local_alist (tid : int) (prog : Test.Lang.Program.t)
      ~(starts_at : int) : (Common.Litmus_id.t, Record.t) List.Assoc.t =
    let decs =
      Accessor.to_listi
        ( Common.C_named.value @> Function.Access.body_decls
        @> Accessor.List.eachi @> Accessor.Tuple2.sndi )
        prog
    in
    List.map decs ~f:(fun ([name; ix], {Initialiser.ty; value}) ->
        let param_index = ix + starts_at in
        ( Common.Litmus_id.local tid name
        , {Record.ty; param_index; initial_value= Some value} ) )

  let make_alist (vast : Test.t) :
      (C4f_common.Litmus_id.t, Record.t) List.Assoc.t Or_error.t =
    let init = Test.init vast in
    let programs = Test.threads vast in
    Or_error.Let_syntax.(
      let%map global_alist = make_global_alist init programs in
      let local_alist =
        List.concat_mapi programs
          ~f:(make_local_alist ~starts_at:(List.length global_alist))
      in
      global_alist @ local_alist)
end
