(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Ac = Act_common
  module Tx = Travesty_base_exts
  module Named = Ac.C_named
end

let parameter_list_equal :
       (Ac.C_id.t, Type.t) List.Assoc.t
    -> (Ac.C_id.t, Type.t) List.Assoc.t
    -> bool =
  [%equal: (Ac.C_id.t * Type.t) list]

let check_parameters_consistent (params : Type.t Named.Alist.t)
    (next : unit Function.t) : unit Or_error.t =
  let params' = Function.parameters next in
  if parameter_list_equal params params' then Ok ()
  else
    Or_error.error_s
      [%message
        "Functions do not agree on parameter lists"
          ~first_example:(params : Type.t Named.Alist.t)
          ~second_example:(params' : Type.t Named.Alist.t)]

let make_global_var_alist (progs : Litmus.Test.Lang.Program.t list) :
    (Ac.Litmus_id.t, Type.t) List.Assoc.t Or_error.t =
  match progs with
  | [] ->
      Or_error.error_string "need at least one function"
  | x :: xs ->
      let params = Function.parameters (Named.value x) in
      Or_error.(
        xs
        |> Tx.Or_error.combine_map_unit
             ~f:(Fn.compose (check_parameters_consistent params) Named.value)
        >>| fun () -> Tx.Alist.map_left params ~f:Act_common.Litmus_id.global)

let make_local_var_alist (tid : int) (prog : Litmus.Test.Lang.Program.t) :
    (Ac.Litmus_id.t, Type.t) List.Assoc.t =
  prog |> Named.value |> Function.body_decls
  |> Tx.Alist.bi_map
       ~left:(Act_common.Litmus_id.local tid)
       ~right:(Accessor.get Initialiser.ty)

let make_local_var_alists :
       Litmus.Test.Lang.Program.t list
    -> (Act_common.Litmus_id.t, Type.t) List.Assoc.t list =
  List.mapi ~f:make_local_var_alist

let make_type_alist (vast : Litmus.Test.t) :
    (Act_common.Litmus_id.t, Type.t) List.Assoc.t Or_error.t =
  let programs = Litmus.Test.threads vast in
  let local_alists = make_local_var_alists programs in
  Or_error.Let_syntax.(
    let%map global_alist = make_global_var_alist programs in
    List.concat (global_alist :: local_alists))

let make_set (vast : Litmus.Test.t) :
    Set.M(Act_common.Litmus_id).t Or_error.t =
  (* TODO(@MattWindsor91): this is likely to be inefficient. *)
  Or_error.(
    vast |> make_type_alist >>| List.map ~f:fst
    >>| Set.of_list (module Act_common.Litmus_id))
