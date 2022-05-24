(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Make (B : Runner_types.Basic) : Runner_types.S = struct
  module Amake = Aux_maker.Make (B)

  let make_global ({c_type; c_id; initial_value; _} : Var_map.Record.t) :
      Common.C_id.t * Fir.Initialiser.t =
    let value =
      Option.value initial_value ~default:(Fir.Constant.zero_of_type c_type)
    in
    (c_id, Fir.{Initialiser.ty= c_type; value})

  let make_globals (ctx : Context.t) :
      (Common.C_id.t, Fir.Initialiser.t) List.Assoc.t =
    let vm = Context.var_map ctx in
    let global_recs = Var_map.globally_mapped_vars vm in
    List.map ~f:(fun (_, v) -> make_global v) global_recs

  let make_program (input : Fir.Litmus.Test.t) (context : Context.t) :
      unit Fir.Program.t Or_error.t =
    let raw_functions = Fir.Litmus.Test.threads input in
    let globals = make_globals context in
    Or_error.Let_syntax.(
      let%map function_list =
        B.Function.rewrite_all raw_functions ~context
      in
      let functions = Common.C_named.alist_of_list function_list in
      Fir.Program.make ~globals ~functions)

  let make_local_init (fn : unit Fir.Function.t) :
      (Common.C_id.t, Fir.Constant.t) List.Assoc.t =
    fn |> Fir.Function.body_decls
    |> Accessor_base.(
         map (List.each @> Tuple2.snd) ~f:(get Fir.Initialiser.value))

  let make_local_inits :
         unit Fir.Function.t list
      -> (int, (Common.C_id.t, Fir.Constant.t) List.Assoc.t) List.Assoc.t =
    List.mapi ~f:(fun tid fn -> (tid, make_local_init fn))

  let make_context (input : Fir.Litmus.Test.t) (aux : Aux.t) : Context.t =
    (* We can get the context just from looking at functions, because of the
       way in which C litmus tests are constructed. *)
    let functions =
      Accessor_base.(to_list (List.each @> Common.C_named.value))
        (Fir.Litmus.Test.threads input)
    in
    let local_inits = make_local_inits functions in
    Context.make ~aux ~local_inits

  let run (input : Fir.Litmus.Test.t) : Output.t Or_error.t =
    Or_error.Let_syntax.(
      let%bind aux = Amake.make_aux input in
      let context = make_context input aux in
      let%map program = make_program input context in
      Output.make ~program ~aux)
end
