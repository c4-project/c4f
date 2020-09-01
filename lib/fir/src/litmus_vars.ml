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

let merge_parameters (pss : Type.t Named.Alist.t list) :
    Type.t Named.Alist.t Or_error.t =
  Or_error.(
    pss
    |> Tx.Or_error.combine_map ~f:(Map.of_alist_or_error (module Ac.C_id))
    >>= Act_utils.My_map.merge_with_overlap ~compare:Type.compare
    >>| Map.to_alist)

let make_global_var_alist (progs : Litmus.Test.Lang.Program.t list) :
    (Ac.Litmus_id.t, Type.t) List.Assoc.t Or_error.t =
  match progs with
  | [] ->
      Or_error.error_string "need at least one function"
  | xs ->
      Or_error.(
        xs
        |> Accessor_base.(
             to_list
               ( List.each @> Ac.C_named.Access.value
               @> Function.Access.parameters ))
        |> merge_parameters
        |> Or_error.tag ~tag:"Functions do not agree on parameter lists"
        >>| Tx.Alist.map_left ~f:Act_common.Litmus_id.global)

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
