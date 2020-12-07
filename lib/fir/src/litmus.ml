(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Lang :
  Act_litmus.Test_types.Basic
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

  module Type = Type

  module Program = struct
    type t = unit Function.t Common.C_named.t [@@deriving sexp]

    let name ({name; _} : t) : string option =
      Some (Common.C_id.to_string name)

    let listing ({value; _} : t) : Statement.t list =
      List.map (Function.body_decls value) ~f:(fun (name, value) ->
          `Decl (Common.C_named.make value ~name))
      @ List.map (Function.body_stms value) ~f:(fun x -> `Stm x)

    let global_vars (np : t) =
      np.@(Common.C_named.value @> Function.Access.parameters)
      |> Map.of_alist_or_error (module Common.C_id)
      |> Result.ok
  end

  let name = "C"
end

module Test = Act_litmus.Test.Make (Lang)

module Var = struct
  module Record = struct
    (* TODO(MattWindsor91): merge this with one of the 9000 other variable
       record modules? *)

    type t = {ty: Type.t; param_index: int} [@@deriving compare, sexp]
  end

  let number_parameter ([param_index] : (int * unit) Accessor.Index.t)
      (ty : Type.t) : Record.t =
    {ty; param_index}

  let merge_and_number_parameters (pss : Type.t Common.C_named.Alist.t list)
      : Record.t Common.C_named.Alist.t Or_error.t =
    Or_error.(
      pss
      |> Accessor.(
           mapi (List.each @> List.eachi @> Tuple2.snd) ~f:number_parameter)
      |> Tx.Or_error.combine_map
           ~f:(Map.of_alist_or_error (module Common.C_id))
      >>= Utils.My_map.merge_with_overlap ~compare:Record.compare
      >>| Map.to_alist)

  let make_global_alist (progs : Test.Lang.Program.t list) :
      (Common.Litmus_id.t, Record.t) List.Assoc.t Or_error.t =
    match progs with
    | [] ->
        Or_error.error_string "need at least one function"
    | xs ->
        Or_error.(
          Accessor_base.(
            xs.@*(List.each @> Common.C_named.value
                  @> Function.Access.parameters))
          |> merge_and_number_parameters
          |> Or_error.tag ~tag:"Functions do not agree on parameter lists"
          >>| Tx.Alist.map_left ~f:Act_common.Litmus_id.global)

  let make_local_alist (tid : int) (prog : Test.Lang.Program.t)
      ~(starts_at : int) : (Common.Litmus_id.t, Record.t) List.Assoc.t =
    let decs =
      Accessor.to_listi
        ( Common.C_named.value @> Function.Access.body_decls
        @> Accessor.List.eachi @> Accessor.Tuple2.sndi @> Initialiser.ty )
        prog
    in
    List.map decs ~f:(fun ([name; ix], ty) ->
        let param_index = ix + starts_at in
        (Common.Litmus_id.local tid name, {Record.ty; param_index}))

  let make_alist (vast : Test.t) :
      (Act_common.Litmus_id.t, Record.t) List.Assoc.t Or_error.t =
    let programs = Test.threads vast in
    Or_error.Let_syntax.(
      let%map global_alist = make_global_alist programs in
      let local_alist =
        List.concat_mapi programs
          ~f:(make_local_alist ~starts_at:(List.length global_alist))
      in
      global_alist @ local_alist)
end
